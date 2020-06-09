module Sound.Cumbia.Parser (parseLang, render) where

import Sound.Cumbia.Program
import Sound.Cumbia.GlobalMaterial
import Sound.Cumbia.Style as S
import Sound.Cumbia.Instrument
import Sound.Cumbia.InstrumentState
import Sound.Cumbia.Harmony
import Sound.Cumbia.Rhythm
import Sound.Cumbia.Generic

import Language.Haskellish as LH
import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Control.Monad.State
import Data.Map as Map
import qualified Sound.OSC as H
import qualified Data.Text as T
import Data.Bifunctor
import Data.Tempo
import Data.Time


type H = Haskellish GlobalMaterial

-- data Program = Program [Layer] GlobalMaterial
-- type Program = ([Layer], GlobalMaterial)

-- f :: (Style, Intrument)
-- f cumbia piano

-- so I can do :
-- cumbia piano
-- (noDownBeats cumbia) piano

parseLang :: String -> Either String ([Layer], GlobalMaterial)
parseLang s = (f . parseExp) $ ( "do {" ++ s ++ "}" )
  where
    f (ParseOk x) = runHaskellish layers defaultGlobalMaterial x -- Either String (a, st)
    f (ParseFailed l s) = Left s

layers :: H [Layer]
layers =  listOfDoStatements statement

statement :: H Layer
statement =  parseLayer <|> globalStatement <|> silencio -- programaLiteral <|> programaConEstiloEInst

-- GlobalMaterial
-- e.g. clave dosTres --gets parsed 1st that changes the state
-- then harmony Cmaj Emin -- 2nd, and changes the state
-- then clave tresDos -- 3rd -- and changes the state

globalStatement :: H Layer
globalStatement = do
  f <- globalMaterialFunctions -- progressionToGm
  st <- get
  let newState = f st
  put newState
  return emptyLayer

silencio :: H Layer
silencio = emptyLayer <$ reserved "silencio"

globalMaterialFunctions :: H (GlobalMaterial -> GlobalMaterial)
globalMaterialFunctions = parseSetChordProg

parseSetChordProg :: H (GlobalMaterial -> GlobalMaterial)
parseSetChordProg = (reserved "harmonia" >> return setChordProg) <*> progressionParser

setChordProg :: [Harmony] -> GlobalMaterial -> GlobalMaterial
setChordProg hs gm = gm { harmony = hs }

progressionToGm :: H GlobalMaterial
progressionToGm = progressionToGm' <*> progressionParser

progressionToGm' :: H ([Harmony] -> GlobalMaterial)
progressionToGm' = setNewGlobalMaterial <$ reserved "harmonia"

progressionParser :: H [Harmony]
progressionParser = list harmoniaParser

harmoniaParser :: H Harmony
harmoniaParser = harmoniaParser' <*> rationalOrInteger

harmoniaParser' :: H (Rational -> Harmony)
harmoniaParser' = harmoniaParser'' <*> rationalOrInteger

harmoniaParser'' :: H (Rational -> Rational -> Harmony)
harmoniaParser'' = harmoniaParser''' <*> rationalOrInteger

harmoniaParser''' :: H (Rational -> Rational -> Rational -> Harmony)
harmoniaParser''' = harmoniaParser'''' <*> chordTypeParser

harmoniaParser'''' :: H (ChordType -> Rational -> Rational -> Rational -> Harmony)
harmoniaParser'''' = do
  p <- pitchParser
  return $ setHarmony p

setNewGlobalMaterial :: [Harmony] -> GlobalMaterial
setNewGlobalMaterial hs = GlobalMaterial { harmony = hs }

setHarmony :: Pitch -> ChordType -> Rational -> Rational -> Rational -> Harmony
setHarmony p t m s e = Harmony (Chord p t) (m, s) (m, e)

-- ?
setHarmony' :: Rational -> (Pitch, ChordType, Rational, Rational) -> Harmony
setHarmony' metre (p, t, s, e) = Harmony (Chord p t) (metre, s) (metre, e)

pitchParser :: H Pitch
pitchParser =
               c <$ reserved "c"
           <|> cs <$ reserved "c'"
           <|> cs <$ reserved "db"
           <|> d <$ reserved "d"
           <|> ds <$ reserved "d'"
           <|> ds <$ reserved "eb"
           <|> e <$ reserved "e"
           <|> f <$ reserved "f"
           <|> fs <$ reserved "f'"
           <|> fs <$ reserved "gb"
           <|> g <$ reserved "g"
           <|> gs <$ reserved "g'"
           <|> gs <$ reserved "ab"
           <|> a <$ reserved "a"
           <|> as <$ reserved "a'"
           <|> as <$ reserved "bb"
           <|> b <$ reserved "b"

chordTypeParser :: H ChordType
chordTypeParser =
                  major <$ reserved "maj"
              <|> minor <$ reserved "min"
              <|> major7 <$ reserved "maj7"
              <|> minor7 <$ reserved "min7"
              <|> dom <$ reserved "dom"
              <|> fifths <$ reserved "quintas"
              <|> sus4 <$ reserved "sus4"
              <|> sus2 <$ reserved "sus2"
              <|> aug <$ reserved "aug"
              <|> dim <$ reserved "dim"
              <|> dim7 <$ reserved "dim7"
              <|> semidim <$ reserved "sdim"

parseLayer :: H Layer
parseLayer =  parseInstAsLayer
          <|> transformadoresDeLayer

transformadoresDeLayer :: H Layer
transformadoresDeLayer =  parseSeleccionarEstilo
                      <|> parseSeleccionarSample
                      <|> parseSeleccionarSamples
                      <|> parseTonicaYquinta
                      <|> parseTonicaYquinta2
                      <|> parseTonicaQoctava
                      <|> parseTonicaQtercera
                      <|> parseCambiarNota
                      <|> parseCambiarNotas
                      <|> parseCambiarRitmo
                      <|> parseCambiarRitmos
                      <|> parseCambiarIntervalo
                      <|> parsePreset

inst :: H Instrument
inst =
        piano <$ reserved "piano"
    <|> bajo <$ reserved "bajo"
    <|> guira <$ reserved "guira"
    <|> contras <$ reserved "contratiempos"
    <|> cuerda <$ reserved "cuerda"
    <|> tarola <$ reserved "tarola"
    <|> efecto <$ reserved "efecto"

parseInstAsLayer :: H Layer
parseInstAsLayer = do
  x <- inst
  return $ instAsLayer x

-- Convert an instrument to a Layer using a defaultStyle
instAsLayer :: Instrument -> Layer
instAsLayer i = Layer (defaultStyle, i)

--or
-- parseStyleInstAsLayer :: H Layer
-- parseStyleInstAsLayer = parseStyleAsFunction <*> inst

estilo :: H S.Style
estilo = cumbia <$ reserved "cumbia"

-- a function to change the style of the layer
parseSeleccionarEstilo :: H Layer
parseSeleccionarEstilo = parseSeleccionarEstilo' <*> parseLayer

parseSeleccionarEstilo' :: H (Layer -> Layer)
parseSeleccionarEstilo' = do
  s <- estilo
  return $ \l -> seleccionarEstilo s l

seleccionarEstilo :: S.Style -> Layer -> Layer
seleccionarEstilo st (Layer (s, i)) = Layer (st, i)

--
-- parseStyleAsFunction :: H (Instrument -> Layer)
-- parseStyleAsFunction = do
--   s <- estilo
--   return $ \i -> styleAndInstrumentToLayer s i
--
-- styleAndInstrumentToLayer ::  S.Style -> Instrument -> Layer
-- styleAndInstrumentToLayer s i = Layer (s, i)

-- a function for selecting a different sample n, e.g. (sample [2] cumbia) piano
parseSeleccionarSamples :: H Layer
parseSeleccionarSamples = parseSeleccionarSamples' <*> parseLayer

parseSeleccionarSamples' :: H (Layer -> Layer)
parseSeleccionarSamples' = parseSeleccionarSamples'' <*> intList

parseSeleccionarSamples'' :: H ([Int] -> Layer -> Layer)
parseSeleccionarSamples'' = seleccionarSamples <$ reserved "sample"

seleccionarSamples :: [Int] -> Layer -> Layer
seleccionarSamples is (Layer (s, i)) =  Layer (nuevoE, i)
  where nuevoE = s {
                   cuerdaSampleNPattern0 = is,
                   pianoSampleNPattern0 =  is,
                   bassSampleNPattern0 =  is,
                   guiraSampleNPattern0 = is,
                   contrasSampleNPattern0 = is,
                   tarolaSampleNPattern0 = is,
                   efectoSampleNPattern0 = is
                  }

-- a function to select a new sample from the folder
parseSeleccionarSample :: H Layer
parseSeleccionarSample = parseSeleccionarSample' <*> parseLayer

parseSeleccionarSample' :: H (Layer -> Layer)
parseSeleccionarSample' = parseSeleccionarSample'' <*> int

parseSeleccionarSample'' :: H (Int -> Layer -> Layer)
parseSeleccionarSample'' = seleccionarSample <$ reserved "sample"

seleccionarSample :: Int -> Layer -> Layer
seleccionarSample index (Layer (s, i)) =  Layer (nuevoE, i)
   where nuevoE = s {
                     cuerdaSampleNPattern0 = [index],
                     pianoSampleNPattern0 =  [index],
                     bassSampleNPattern0 =  [index],
                     guiraSampleNPattern0 = [index],
                     contrasSampleNPattern0 = [index],
                     tarolaSampleNPattern0 = [index],
                     efectoSampleNPattern0 = [index]
                    }

-- transforms the preset bass to just fundamental and fifth of the chord
-- e.g  (tonicaYquinta cumbia) bajo
parseTonicaYquinta :: H Layer
parseTonicaYquinta = parseTonicaYquinta' <*> parseLayer

parseTonicaYquinta' :: H (Layer -> Layer) -- (tonicaYquinta cumbia) bajo
parseTonicaYquinta' = tonicaYquinta <$ reserved "tonicayquinta"

-- una función que devuelve a tonica y la quinta del bajo
tonicaYquinta :: Layer -> Layer -- ?
tonicaYquinta (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                    bassPitchPattern0 =  ("intervalo", [(intervalo "unisono" 0), (intervalo "5a" 0)]), -- index from list of pitches i.e. [60, 67]
                    bassRhythmPattern0 = [(1, 0), (1, 0.5)]  --i.e. [♩ 𝄽  ♩ 𝄽 ],
                    }

-- Arriba, el bajo toca la tónica, la quinta y la quinta una octava más alta.
parseTonicaYquinta2 :: H Layer
parseTonicaYquinta2 = parseTonicaYquinta2' <*> parseLayer

parseTonicaYquinta2' :: H (Layer -> Layer)
parseTonicaYquinta2' = tonicaYquinta2 <$ reserved "tonicayquinta2"

tonicaYquinta2 :: Layer -> Layer
tonicaYquinta2 (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                    bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],
                    bassPitchPattern0 = ("intervalo", [intervalo "unisono" 0, intervalo "5a" 0, intervalo "5a" (-1)]) -- index from list of pitches i.e. [60, 64, 67]
                  }

--tonicaQtonica $ cumbia bajo, el bajo toca la tónica, la quinta y la octava alta de la tónica.
parseTonicaQoctava :: H Layer
parseTonicaQoctava = parseTonicaQoctava' <*> parseLayer

parseTonicaQoctava' :: H (Layer -> Layer)
parseTonicaQoctava' = tonicaQoctava <$ reserved "tonicayquinta2"

tonicaQoctava :: Layer -> Layer
tonicaQoctava (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                    bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],
                    bassPitchPattern0 = ("intervalo", [intervalo "unisono" 0, intervalo "5a" 0, intervalo "8a" 1]) -- index from list of pitches i.e. [60, 64, 67]
                  }

-- tonicaQtercera  $ cumbia bajo, el bajo toca la tónica, la quinta y la tercer del acorde.
parseTonicaQtercera :: H Layer
parseTonicaQtercera = parseTonicaQtercera' <*> parseLayer

parseTonicaQtercera' :: H (Layer -> Layer)
parseTonicaQtercera' = tonicaQtercera <$ reserved "tonicayquinta2"

tonicaQtercera :: Layer -> Layer
tonicaQtercera (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                    bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],
                    bassPitchPattern0 = ("intervalo", [intervalo "unisono" 0, intervalo "5a" 0, intervalo "3a" 0]) -- index from list of pitches i.e. [60, 64, 67]
                  }


-- a function for changing the preset pitch pattern provided by the style
parseCambiarNotas :: H Layer
parseCambiarNotas = parseCambiarNotas' <*> parseLayer

parseCambiarNotas' :: H (Layer -> Layer)
parseCambiarNotas' = parseCambiarNotas'' <*>  doubleList

parseCambiarNotas'' :: H ([Double] -> Layer -> Layer)
parseCambiarNotas'' = cambiarNotas <$ reserved "nota"

cambiarNotas :: [Double] -> Layer -> Layer
cambiarNotas ps (Layer (s, i)) = Layer (st, i)
  where st = s {
                cuerdaPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                pianoPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                bassPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                efectoPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps)
                }

listDeNotasConRelacion :: Relacion -> [Double] -> [(Relacion, Octava, Double)]
listDeNotasConRelacion r ns = fmap (\n -> (r, 0, n)) ns

-- cambia una sola nota
parseCambiarNota :: H Layer
parseCambiarNota = parseCambiarNota' <*> parseLayer

parseCambiarNota' :: H (Layer -> Layer)
parseCambiarNota' = parseCambiarNota'' <*>  double

parseCambiarNota'' :: H (Double -> Layer -> Layer)
parseCambiarNota'' = cambiarNota <$ reserved "nota"

cambiarNota :: Double -> Layer -> Layer
cambiarNota ps (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                  cuerdaPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                  pianoPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                  bassPitchPattern0= ("midinote", [("mn", ps, 0)]),
                  efectoPitchPattern0 = ("midinote", [("mn", ps, 0)])
                   }

-- provee el intervalo con respecto a la tonica y cualidad del acorde
parseCambiarIntervalo :: H Layer
parseCambiarIntervalo = parseCambiarIntervalo' <*> parseLayer

parseCambiarIntervalo' :: H (Layer -> Layer)
parseCambiarIntervalo' = parseCambiarIntervalo'' <*> string

parseCambiarIntervalo'' :: H (String -> Layer -> Layer)
parseCambiarIntervalo'' = cambiarIntervalo <$ reserved "intervalo"

cambiarIntervalo :: String -> Layer -> Layer
cambiarIntervalo index (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                    cuerdaPitchPattern0 = ("intervalo", [intervalo index 0]),
                    pianoPitchPattern0 = ("intervalo", [intervalo index 0]),
                    bassPitchPattern0= ("intervalo", [intervalo index 0]),
                    efectoPitchPattern0 = ("intervalo", [intervalo index 0])
                    }

-- type RhythmicPattern = [(Rational,Rational)]
-- ritmo 0.5 cumbia cuerda
parseCambiarRitmo :: H Layer
parseCambiarRitmo =  parseCambiarRitmo' <*> parseLayer

parseCambiarRitmo' :: H (Layer -> Layer)
parseCambiarRitmo' =  parseCambiarRitmo'' <*> rationalOrInteger

parseCambiarRitmo'' :: H (Rational -> Layer -> Layer)
parseCambiarRitmo'' = parseCambiarRitmo''' <*> rationalOrInteger

parseCambiarRitmo''' :: H (Rational -> Rational -> Layer -> Layer)
parseCambiarRitmo''' = cambiarRitmo <$ reserved "ritmo"

cambiarRitmo :: Rational -> Rational -> Layer -> Layer
cambiarRitmo metre attacks (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                    cuerdaRhythmPattern0 = [cambiarRitmo' metre attacks],
                    pianoRhythmPattern0 = [cambiarRitmo' metre attacks],
                    bassRhythmPattern0 = [cambiarRitmo' metre attacks],
                    guiraRhythmPattern0 = [cambiarRitmo' metre attacks],
                    contrasRhythmPattern0 = [cambiarRitmo' metre attacks],
                    tarolaRhythmPattern0 = [cambiarRitmo' metre attacks],
                    efectoRhythmPattern0 = [cambiarRitmo' metre attacks]
                    }

cambiarRitmo' :: Rational -> Rational -> (Rational, Rational)
cambiarRitmo' metre attack = (metre, attack)


-- ritmo [0.125, 0.25] cumbia cuerda
parseCambiarRitmos :: H Layer
parseCambiarRitmos =  parseCambiarRitmos' <*> parseLayer

parseCambiarRitmos' :: H (Layer -> Layer)
parseCambiarRitmos' =  parseCambiarRitmos'' <*> rationalList

parseCambiarRitmos'' :: H ([Rational] -> Layer -> Layer)
parseCambiarRitmos'' = parseCambiarRitmos''' <*> rationalOrInteger

parseCambiarRitmos''' :: H (Rational -> [Rational] -> Layer -> Layer)
parseCambiarRitmos''' = cambiarRitmos <$ reserved "ritmo"

cambiarRitmos :: Rational -> [Rational] -> Layer -> Layer
cambiarRitmos metre attacks (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                    cuerdaRhythmPattern0 = cambiarRitmo'' metre attacks,
                    pianoRhythmPattern0 = cambiarRitmo'' metre attacks,
                    bassRhythmPattern0 = cambiarRitmo'' metre attacks,
                    guiraRhythmPattern0 = cambiarRitmo'' metre attacks,
                    contrasRhythmPattern0 = cambiarRitmo'' metre attacks,
                    tarolaRhythmPattern0 = cambiarRitmo'' metre attacks,
                    efectoRhythmPattern0 = cambiarRitmo'' metre attacks
                    }

cambiarRitmo'' :: Rational -> [Rational] -> [(Rational, Rational)]
cambiarRitmo'' metre attacks = fmap (cambiarRitmo' metre) attacks


-- a function that allows switching between presets
-- e.g: preset 1 cumbia bajo
parsePreset :: H Layer
parsePreset = parsePreset' <*> parseLayer

parsePreset' :: H (Layer -> Layer)
parsePreset' = parsePreset'' <*> int

parsePreset'' :: H (Int -> Layer -> Layer)
parsePreset'' = preset <$ reserved "preset"

preset :: Int -> Layer -> Layer
preset 0 (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                    pianoRhythmPattern0 = pianoRhythmPattern0 s, -- ie.  [𝄽  𝄽  𝄽  ♩],
                    pianoSampleNPattern0 = pianoSampleNPattern0 s,

                    cuerdaRhythmPattern0 = cuerdaRhythmPattern0 s,
                    cuerdaSampleNPattern0 = cuerdaSampleNPattern0 s,
                    cuerdaPitchPattern0 = cuerdaPitchPattern0 s, -- or double? (nota [0, 2, 3] cumbia) cuerda

                    bassRhythmPattern0 = bassRhythmPattern0 s,  --i.e. [♩ 𝄽  ♩ ♩],
                    bassSampleNPattern0 = bassSampleNPattern0 s,
                    bassPitchPattern0 = bassPitchPattern0 s, -- index from list of pitches i.e. [60, 64, 67]

                    guiraRhythmPattern0 = guiraRhythmPattern0 s, --i.e. [♪♫ ♪♫ ♪♫ ♪♫]
                    guiraSampleNPattern0 = guiraSampleNPattern0 s,

                    contrasRhythmPattern0 = contrasRhythmPattern0 s,
                    contrasSampleNPattern0 = contrasSampleNPattern0 s,

                    tarolaRhythmPattern0 = tarolaRhythmPattern0 s,
                    tarolaSampleNPattern0 =tarolaSampleNPattern0 s,

                    efectoRhythmPattern0 = efectoRhythmPattern0 s,
                    efectoSampleNPattern0 = efectoSampleNPattern0 s,
                    efectoPitchPattern0 = efectoPitchPattern0 s
                  }

preset 1 (Layer (s, i)) = Layer (nuevoE, i)
  where nuevoE = s {
                    pianoRhythmPattern0 = pianoRhythmPattern1 s, -- ie. [𝄽 ♩ 𝄽 ♩],
                    pianoSampleNPattern0 = pianoSampleNPattern0 s,

                    bassRhythmPattern0 = bassRhythmPattern1 s,  --i.e. [♩ 𝄽  ♩ 𝄽 ],
                    bassSampleNPattern0 = pianoSampleNPattern0 s,
                    bassPitchPattern0 = bassPitchPattern1 s
                  }

preset 2 (Layer (s, i)) = Layer (nuevoE, i)
   where nuevoE = s {
                    bassRhythmPattern0 = bassRhythmPattern2 s,  --i.e. [♩ 𝄽  ♩ 𝄽 ],
                    bassSampleNPattern0 = pianoSampleNPattern0 s,
                    bassPitchPattern0 = bassPitchPattern2 s
                    }

preset _ (Layer (s, i)) = preset 0 (Layer (s, i))

-- heper functions

rationalList :: H [Rational]
rationalList = list $ rationalOrInteger

intList :: H [Int]
intList = list $ fromIntegral <$> integer

int :: H Int
int = fromIntegral <$> integer

double :: H Double
double = fromRational <$> rationalOrInteger

doubleList :: H [Double]
doubleList = list double

-- the renderer
--   getEvents :: GlobalMaterial -> Style -> Tempo -> BeginWindowTime -> EndWindowTime -> State InstrumentState [Event]
-- render :: (GlobalMaterial,Style,Instrument) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime,Map Text Datum)]
-- runState :: State s a -> s -> (a, s) -- as soon as the state is meaningful I should stop discarding it.
--check Tidal.params for looking at the available params for webdirt
render :: ([Layer], GlobalMaterial) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime, Map T.Text H.Datum)]
render (ls, gm) tempo iw ew = Prelude.concat $ fmap (\l -> render' (l, gm) tempo iw ew) ls


render' :: (Layer, GlobalMaterial) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime, Map T.Text H.Datum)]
render' ((Layer (s, i)), gm) tempo iw ew = fst $ runState x emptyInstrumentState --this should be another argument to my render function
  where
     x = getEvents i gm s tempo iw ew
