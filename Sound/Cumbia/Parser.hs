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
statement =  parseLayer <|> globalStatement -- programaLiteral <|> programaConEstiloEInst

-- GlobalMaterial
-- e.g. clave dosTres --gets parsed 1st that changes the state
-- then harmony Cmaj Emin -- 2nd, and changes the state
-- then clave tresDos -- 3rd -- and changes the state

-- globalStatement :: H Layer -- should return []?
-- globalStatement = do
--   f <-  globalMaterialFunctions
--   st <- get
--   let newState = f st
--   put newState
--   return emptyLayer

globalStatement :: H Layer
globalStatement = do
  f <- progressionToGm -- globalMaterialFunctions --
  st <- get
  let newState = f -- f st
  put newState
  return emptyLayer

-- globalMaterialFunctions :: H (GlobalMaterial -> GlobalMaterial)
-- globalMaterialFunctions gm = progressionToGm ?

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
  c <$ reserved "c"     <|>
  cs <$ reserved "c'"   <|>
  cs <$ reserved "db"   <|>
  d <$ reserved "d"     <|>
  ds <$ reserved "d'"   <|>
  ds <$ reserved "eb"   <|>
  e <$ reserved "e"     <|>
  f <$ reserved "f"     <|>
  fs <$ reserved "f'"   <|>
  fs <$ reserved "gb"   <|>
  g <$ reserved "g"     <|>
  gs <$ reserved "g'"   <|>
  gs <$ reserved "ab"   <|>
  a <$ reserved "a"     <|>
  as <$ reserved "a'"   <|>
  as <$ reserved "bb"   <|>
  b <$ reserved "b"

chordTypeParser :: H ChordType
chordTypeParser =
  major <$ reserved "maj"      <|> -- no capital letters?
  minor <$ reserved "min"      <|>
  major7 <$ reserved "maj7"    <|>
  minor7 <$ reserved "min7"    <|>
  dom <$ reserved "dom"        <|>
  fifths <$ reserved "quintas" <|>
  sus4 <$ reserved "sus4"      <|>
  sus2 <$ reserved "sus2"      <|>
  aug <$ reserved "aug"        <|>
  dim <$ reserved "dim"

parseLayer :: H Layer
parseLayer = parseStyleInstAsLayer <|> parseInstAsLayer

inst :: H Instrument
inst =
  piano <$ reserved "piano" <|>
  bajo <$ reserved "bajo" <|>
  guira <$ reserved "guira" <|>
  contras <$ reserved "contratiempos" <|>
  cuerda <$ reserved "cuerda" <|>
  tarola <$ reserved "tarola" <|>
  efecto <$ reserved "efecto"

parseInstAsLayer :: H Layer
parseInstAsLayer = do
  x <- inst
  return $ instAsLayer x

-- Convert an instrument to a Layer using a defaultStyle
instAsLayer :: Instrument -> Layer
instAsLayer i = Layer (defaultStyle, i)

--or
parseStyleInstAsLayer :: H Layer
parseStyleInstAsLayer = parseStyleAsFunction <*> inst

estilo :: H S.Style
estilo = cumbia <$ reserved "cumbia"
      <|> transformadoresDeEstilo

transformadoresDeEstilo :: H S.Style
transformadoresDeEstilo = parseSeleccionarSample
                      <|> parseTonicaYquinta
                      <|> parseCambiarNota
                      <|> parseCambiarRitmo
                      <|> parsePreset

-- provide that missing function similar to asignarEstiloEInst
parseStyleAsFunction :: H (Instrument -> Layer)
parseStyleAsFunction = do
  s <- estilo
  return $ \i -> styleAndInstrumentToLayer s i

styleAndInstrumentToLayer ::  S.Style -> Instrument -> Layer
styleAndInstrumentToLayer s i = Layer (s, i)

-- a function for selecting a different sample n, e.g. (seleccionarSample [2] cumbia) piano
parseSeleccionarSample  :: H S.Style
parseSeleccionarSample = parseSeleccionarSample' <*> estilo

parseSeleccionarSample' :: H (S.Style -> S.Style)
parseSeleccionarSample' = parseSeleccionarSample'' <*> intList

parseSeleccionarSample'' :: H ([Int] -> S.Style -> S.Style)
parseSeleccionarSample'' = seleccionarSample <$ reserved "seleccionarSample"

rationalList :: H [Rational]
rationalList = list $ rationalOrInteger

intList :: H [Int]
intList = list $ fromIntegral <$> integer

int :: H Int
int = fromIntegral <$> integer

-- selects other samples for the instrument folder
seleccionarSample :: [Int] -> S.Style -> S.Style
seleccionarSample is s =  s {
                             cuerdaSampleNPattern0 = is,
                             pianoSampleNPattern0 =  is,
                             bassSampleNPattern0 =  is,
                             guiraSampleNPattern0 = is,
                             contrasSampleNPattern0 = is,
                             tarolaSampleNPattern0 = is,
                             efectoSampleNPattern0 = is
                            }

-- transforms the preset bass to just fundamental and fifth of the chord
-- e.g  (tonicaYquinta cumbia) bajo
parseTonicaYquinta :: H S.Style
parseTonicaYquinta = parseTonicaYquinta' <*> estilo

parseTonicaYquinta' :: H (S.Style -> S.Style) -- (tonicaYquinta cumbia) bajo
parseTonicaYquinta' = tonicaYquinta <$ reserved "tonicaYquinta"

-- una función que devuelve a tonica y la quinta del bajo
tonicaYquinta :: S.Style -> S.Style -- ?
tonicaYquinta s = s {
                bassPitchPattern0 =  [0, 2], -- index from list of pitches i.e. [60, 67]
                bassRhythmPattern0 = [(1, 0), (1, 0.5)]  --i.e. [♩ 𝄽  ♩ 𝄽 ],
              }

-- a function for changing the preset pitch pattern provided by the style
parseCambiarNota :: H S.Style
parseCambiarNota = parseCambiarNota' <*> estilo

parseCambiarNota' :: H (S.Style -> S.Style)
parseCambiarNota' = parseCambiarNota'' <*>  intList

parseCambiarNota'' :: H ([Int] -> S.Style -> S.Style)
parseCambiarNota'' = cambiarNota <$ reserved "nota"

cambiarNota :: [Int] -> S.Style -> S.Style
cambiarNota ps s = s {
                  cuerdaPitchPattern0 = ps,
                  pianoPitchPattern = ps,
                  bassPitchPattern0 = ps,
                  efectoPitchPattern0 = ps
                   }

-- type RhythmicPattern = [(Rational,Rational)]
-- ritmo [1, 2] cumbia cuerda
parseCambiarRitmo :: H S.Style
parseCambiarRitmo =  parseCambiarRitmo' <*> estilo

parseCambiarRitmo' :: H (S.Style -> S.Style)
parseCambiarRitmo' =  parseCambiarRitmo'' <*> rationalList

parseCambiarRitmo'' :: H ([Rational] -> S.Style -> S.Style)
parseCambiarRitmo'' = parseCambiarRitmo''' <*> rationalOrInteger

parseCambiarRitmo''' :: H (Rational -> [Rational] -> S.Style -> S.Style)
parseCambiarRitmo''' = cambiarRitmo <$ reserved "ritmo"

cambiarRitmo :: Rational -> [Rational] -> S.Style -> S.Style
cambiarRitmo metre attacks s = s {
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

cambiarRitmo' :: Rational -> Rational -> (Rational, Rational)
cambiarRitmo' metre attack = (metre, attack)

-- a function that allows switching between presets
-- e.g: preset 1 cumbia bajo
parsePreset :: H S.Style
parsePreset = parsePreset' <*> estilo

parsePreset' :: H (S.Style -> S.Style)
parsePreset' = parsePreset'' <*> int

parsePreset'' :: H (Int -> S.Style -> S.Style)
parsePreset'' = preset <$ reserved "preset"

preset :: Int -> S.Style -> S.Style
preset 0 s = s {
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

preset 1 s = s {
    pianoRhythmPattern0 = pianoRhythmPattern1 s, -- ie. [𝄽 ♩ 𝄽 ♩],
    pianoSampleNPattern0 = pianoSampleNPattern1 s,

    bassRhythmPattern0 = bassRhythmPattern1 s,  --i.e. [♩ 𝄽  ♩ 𝄽 ],
    bassSampleNPattern0 = bassSampleNPattern1 s,
    bassPitchPattern0 = bassPitchPattern1 s
  }

preset 2 s = s {
    bassRhythmPattern0 = bassRhythmPattern2 s,  --i.e. [♩ 𝄽  ♩ 𝄽 ],
    bassSampleNPattern0 = bassSampleNPattern2 s,
    bassPitchPattern0 = bassPitchPattern2 s
  }

preset _ s = preset 0 s

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
