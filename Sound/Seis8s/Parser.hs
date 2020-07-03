{-# LANGUAGE OverloadedStrings #-}

module Sound.Seis8s.Parser (parseLang, render) where

import Sound.Seis8s.Program
import Sound.Seis8s.GlobalMaterial
import Sound.Seis8s.Style as S
import Sound.Seis8s.Layer
import Sound.Seis8s.LayerState
import Sound.Seis8s.Harmony
import Sound.Seis8s.Rhythm
import Sound.Seis8s.Generic

import Language.Haskellish as LH
import qualified Language.Haskell.Exts as Exts
import Control.Applicative
import Data.IntMap.Strict
import Control.Monad.State
import qualified Data.Map as Map
import qualified Sound.OSC as H
import qualified Data.Text as T
import qualified Data.List as List
import Data.Bifunctor
import Data.Tempo
import Data.Time
import Data.Fixed
import Data.Maybe
import qualified Sound.OSC as H


type H = Haskellish GlobalMaterial

-- data Program = Program [Layer] GlobalMaterial
-- type Program = ([Layer], GlobalMaterial)

-- f :: (Style, Intrument)
-- f cumbia piano

-- so I can do :
-- cumbia piano
-- (noDownBeats cumbia) piano

parseLang :: String -> Either String ([Layer], GlobalMaterial)
parseLang s = (f . Exts.parseExp) $ ( "do {" ++ s ++ "}" )
  where
    f (Exts.ParseOk x) = runHaskellish layers defaultGlobalMaterial x -- Either String (a, st)
    f (Exts.ParseFailed l s) = Left s

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
parseSetChordProg = parseSetChordProgWMetre
                 <|> parseSetChordProgMetreAuto

parseSetChordProgWMetre :: H (GlobalMaterial -> GlobalMaterial)
parseSetChordProgWMetre = parseSetChordProgWMetre' <*> chordList

parseSetChordProgWMetre' :: H ([Chord] -> GlobalMaterial -> GlobalMaterial)
parseSetChordProgWMetre' = parseSetChordProgWMetre'' <*> rationalOrInteger

parseSetChordProgWMetre'' :: H (Metre -> [Chord] -> GlobalMaterial -> GlobalMaterial)
parseSetChordProgWMetre'' = (setChordProg <$ reserved "harmonia")

parseSetChordProgMetreAuto :: H (GlobalMaterial -> GlobalMaterial)
parseSetChordProgMetreAuto = (reserved "harmonia" >> return setChordProgMetreAuto) <*>  chordListMetreAuto

setChordProg :: Metre -> [Chord] -> GlobalMaterial -> GlobalMaterial
setChordProg metre hs gm = gm { harmony = castProgression metre hs }

setChordProgMetreAuto ::[Chord] -> GlobalMaterial -> GlobalMaterial
setChordProgMetreAuto hs gm = gm { harmony = castProgressionMetreAuto hs }

chordList :: H [Chord]
chordList = list chordParser

chordParser :: H Chord
chordParser = chordParser' <*> rationalOrInteger

chordParser' :: H (Rational -> Chord)
chordParser' = chordParser'' <*> rationalOrInteger

chordParser'' :: H (Rational -> Rational -> Chord)
chordParser'' = chordParser''' <*> chordTypeParser

chordParser''' :: H (ChordType -> Rational -> Rational -> Chord)
chordParser''' = do
  p <- pitchParser
  return $ \t s e -> castHarmony p t s e

--parses a list of chords with default dur of 1c, e.g. harmonia [c maj, d maj]
chordListMetreAuto :: H [Chord]
chordListMetreAuto = do
  x <- listofpitchandchord
  return $ pitchandtypetochord x

listofpitchandchord :: H [(Pitch, ChordType)]
listofpitchandchord = list parsepitchandtypetotuple

parsepitchandtypetotuple ::  H (Pitch, ChordType)
parsepitchandtypetotuple = parsepitchandtypetotuple' <*> chordTypeParser

parsepitchandtypetotuple' :: H (ChordType -> (Pitch, ChordType))
parsepitchandtypetotuple' = do
  p <- pitchParser
  return $ \t -> pitchandtypetotuple p t

pitchandtypetotuple :: Pitch -> ChordType -> (Pitch, ChordType)
pitchandtypetotuple p t = (p,t)

pitchandtypetochord :: [(Pitch, ChordType)] -> [Chord]
pitchandtypetochord xs = do
  let startandend = fmap (\s -> (toRational s, toRational s+1)) [0 .. (length xs)]
  let zipXSwithStartEnd = zip xs startandend --[((),())]
  fmap (\((p,t), (s,e)) -> Chord p t (s,e)) zipXSwithStartEnd

-- harmonia 1 [C maj 0 1]
castProgression :: Rational -> [Chord] -> Progression
castProgression metre cs = Progression metre cs

-- harmonia 1 [C maj 0 1]
castProgressionMetreAuto :: [Chord] -> Progression
castProgressionMetreAuto cs = do
  let metre = 1 + (realToFrac $ length cs)
  Progression metre cs

castHarmony :: Pitch -> ChordType -> Rational -> Rational -> Chord
castHarmony p t s e = Chord p t (s, e)


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
parseLayer =  inst
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
                      <|> parseCambiarIntervaloConString
                      <|> parsePreset
                      <|> parseAlternar
--
inst :: H Layer
inst =
        piano <$ reserved "piano"
    <|> bajo <$ reserved "bajo"
    <|> guira <$ reserved "guira"
    <|> contras <$ reserved "contratiempos"
    <|> cuerda <$ reserved "cuerda"
    <|> tarola <$ reserved "tarola"
    <|> efecto <$ reserved "efecto"


estilo :: H S.Style
estilo =  defaultStyle <$ reserved "def"
      <|> cumbia <$ reserved "cumbia"

-- a function to change the style of the layer
parseSeleccionarEstilo :: H Layer
parseSeleccionarEstilo = parseSeleccionarEstilo' <*> parseLayer

parseSeleccionarEstilo' :: H (Layer -> Layer)
parseSeleccionarEstilo' = do
  e <- estilo
  return $ \c -> seleccionarEstilo e c

seleccionarEstilo :: S.Style -> Layer -> Layer
seleccionarEstilo e c = c {style = e}

-- a function for selecting a different sample n, e.g. (sample [2] cumbia) piano
parseSeleccionarSamples :: H Layer
parseSeleccionarSamples = parseSeleccionarSamples' <*> parseLayer

parseSeleccionarSamples' :: H (Layer -> Layer)
parseSeleccionarSamples' = parseSeleccionarSamples'' <*> intList

parseSeleccionarSamples'' :: H ([Int] -> Layer -> Layer)
parseSeleccionarSamples'' = seleccionarSamples <$ reserved "sample"

seleccionarSamples :: [Int] -> Layer -> Layer
seleccionarSamples is c =  c {style = nuevoE}
  where nuevoE = (style c) {
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
seleccionarSample index c =  c {style = nuevoE}
   where nuevoE = (style c) {
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
tonicaYquinta c = c {style = nuevoE}
  where nuevoE = (style c) {
                            bassPitchPattern0 =  ("intervalo", [(intervalo "unisono" 0), (intervalo "5a" 0)]), -- index from list of pitches i.e. [60, 67]
                            bassRhythmPattern0 = [(1, 0), (1, 0.5)]  --i.e. [♩ 𝄽  ♩ 𝄽 ],
                            }

-- Arriba, el bajo toca la tónica, la quinta y la quinta una octava más alta.
parseTonicaYquinta2 :: H Layer
parseTonicaYquinta2 = parseTonicaYquinta2' <*> parseLayer

parseTonicaYquinta2' :: H (Layer -> Layer)
parseTonicaYquinta2' = tonicaYquinta2 <$ reserved "tonicayquinta2"

tonicaYquinta2 :: Layer -> Layer
tonicaYquinta2 c = c {style = nuevoE}
  where nuevoE = (style c) {
                            bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],
                            bassPitchPattern0 = ("intervalo", [intervalo "unisono" 0, intervalo "5a" 0, intervalo "5a" (-1)]) -- index from list of pitches i.e. [60, 64, 67]
                          }

--tonicaQtonica $ cumbia bajo, el bajo toca la tónica, la quinta y la octava alta de la tónica.
parseTonicaQoctava :: H Layer
parseTonicaQoctava = parseTonicaQoctava' <*> parseLayer

parseTonicaQoctava' :: H (Layer -> Layer)
parseTonicaQoctava' = tonicaQoctava <$ reserved "tonicaQoctava"

tonicaQoctava :: Layer -> Layer
tonicaQoctava c = c {style = nuevoE}
  where nuevoE = (style c) {
                            bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],
                            bassPitchPattern0 = ("intervalo", [intervalo "unisono" 0, intervalo "5a" 0, intervalo "8a" 0]) -- index from list of pitches i.e. [60, 64, 67]
                          }

-- tonicaQtercera  $ cumbia bajo, el bajo toca la tónica, la quinta y la tercer del acorde.
parseTonicaQtercera :: H Layer
parseTonicaQtercera = parseTonicaQtercera' <*> parseLayer

parseTonicaQtercera' :: H (Layer -> Layer)
parseTonicaQtercera' = tonicaQtercera <$ reserved "tonicaQtercera"

tonicaQtercera :: Layer -> Layer
tonicaQtercera c = c {style = nuevoE}
  where nuevoE = (style c) {
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
cambiarNotas ps c = c {style = nuevoE}
  where nuevoE = (style c) {
                            cuerdaPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            pianoPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            bassPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            efectoPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps)
                            }

listDeNotasConRelacion :: Relacion -> [Double] -> [(Relacion, Double, Octava)]
listDeNotasConRelacion r ns = fmap (\n -> (r, n, 0)) ns

-- cambia una sola nota
parseCambiarNota :: H Layer
parseCambiarNota = parseCambiarNota' <*> parseLayer

parseCambiarNota' :: H (Layer -> Layer)
parseCambiarNota' = parseCambiarNota'' <*>  double

parseCambiarNota'' :: H (Double -> Layer -> Layer)
parseCambiarNota'' = cambiarNota <$ reserved "nota"

cambiarNota :: Double -> Layer -> Layer
cambiarNota ps c = c {style = nuevoE}
  where nuevoE = (style c) {
                            cuerdaPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            pianoPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            bassPitchPattern0= ("midinote", [("mn", ps, 0)]),
                            efectoPitchPattern0 = ("midinote", [("mn", ps, 0)])
                             }

-- provee los intervalos de una lista
-- ("intervalo", [intervalo "unisono" 0, intervalo "3a" 0, intervalo "5a" 0])
-- cambiarIntervalos :: [String] -> Layer -> Layer
-- cambiarIntervalos index c = c {style = nuevoE}
--   where nuevoE = (style c) {
--                             cuerdaPitchPattern0 = ("intervalo", [intervalo index 0]),
--                             pianoPitchPattern0 = ("intervalo", [intervalo index 0]),
--                             bassPitchPattern0= ("intervalo", [intervalo index 0]),
--                             efectoPitchPattern0 = ("intervalo", [intervalo index 0])
--                             }
-- provee el intervalo con respecto a la tonica y cualidad del acorde
parseCambiarIntervaloConString :: H Layer
parseCambiarIntervaloConString = parseCambiarIntervaloConString' <*> parseLayer

parseCambiarIntervaloConString' :: H (Layer -> Layer)
parseCambiarIntervaloConString' = parseCambiarIntervaloConString'' <*> string

parseCambiarIntervaloConString'' :: H (String -> Layer -> Layer)
parseCambiarIntervaloConString'' = cambiarIntervaloConString <$ reserved "intervalo"

cambiarIntervaloConString :: String -> Layer -> Layer
cambiarIntervaloConString index c = c {style = nuevoE}
  where nuevoE = (style c) {
                            cuerdaPitchPattern0 = ("intervalo", [intervalo index 0]),
                            pianoPitchPattern0 = ("intervalo", [intervalo index 0]),
                            bassPitchPattern0= ("intervalo", [intervalo index 0]),
                            efectoPitchPattern0 = ("intervalo", [intervalo index 0])
                            }

-- parseCambiarIntervaloConInt :: H Layer
-- parseCambiarIntervaloConInt = parseCambiarIntervaloConInt' <*> parseLayer
--
-- parseCambiarIntervaloConInt' :: H (Layer -> Layer)
-- parseCambiarIntervaloConInt' = parseCambiarIntervaloConInt'' <*> int
--
-- parseCambiarIntervaloConInt'' :: H (Int -> Layer -> Layer)
-- parseCambiarIntervaloConInt'' = cambiarIntervaloConInt <$ reserved "intervalo"

-- cambiarIntervalos :: [String] -> Layer -> Layer
-- cambiarIntervalos index c = c {style = nuevoE}
--   where nuevoE = (style c) {
--                             cuerdaPitchPattern0 = ("intervalo", [(index, 0)]),
--                             pianoPitchPattern0 = ("intervalo", [(index, 0)]),
--                             bassPitchPattern0= ("intervalo", [(index, 0)]),
--                             efectoPitchPattern0 = ("intervalo", [(index, 0)])
--                             }

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
cambiarRitmo metre attacks c = c {style = nuevoE}
  where nuevoE = (style c) {
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


--e.g. ritmo [0.125, 0.25] cumbia cuerda
parseCambiarRitmos :: H Layer
parseCambiarRitmos =  parseCambiarRitmos' <*> parseLayer

parseCambiarRitmos' :: H (Layer -> Layer)
parseCambiarRitmos' =  parseCambiarRitmos'' <*> rationalList

parseCambiarRitmos'' :: H ([Rational] -> Layer -> Layer)
parseCambiarRitmos'' = parseCambiarRitmos''' <*> rationalOrInteger

parseCambiarRitmos''' :: H (Rational -> [Rational] -> Layer -> Layer)
parseCambiarRitmos''' = cambiarRitmos <$ reserved "ritmo"

cambiarRitmos :: Rational -> [Rational] -> Layer -> Layer
cambiarRitmos metre attacks c = c {style = nuevoE}
  where nuevoE = (style c) {
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
preset 0 c = c {style = nuevoE}
  where nuevoE = (style c) {
                            pianoRhythmPattern0 = pianoRhythmPattern0 (style c), -- ie.  [𝄽  𝄽  𝄽  ♩],
                            pianoSampleNPattern0 = pianoSampleNPattern0 (style c),

                            cuerdaRhythmPattern0 = cuerdaRhythmPattern0 (style c),
                            cuerdaSampleNPattern0 = cuerdaSampleNPattern0 (style c),
                            cuerdaPitchPattern0 = cuerdaPitchPattern0 (style c), -- or double? (nota [0, 2, 3] cumbia) cuerda

                            bassRhythmPattern0 = bassRhythmPattern0 (style c),  --i.e. [♩ 𝄽  ♩ ♩],
                            bassSampleNPattern0 = bassSampleNPattern0 (style c),
                            bassPitchPattern0 = bassPitchPattern0 (style c), -- index from list of pitches i.e. [60, 64, 67]

                            guiraRhythmPattern0 = guiraRhythmPattern0 (style c), --i.e. [♪♫ ♪♫ ♪♫ ♪♫]
                            guiraSampleNPattern0 = guiraSampleNPattern0 (style c),

                            contrasRhythmPattern0 = contrasRhythmPattern0 (style c),
                            contrasSampleNPattern0 = contrasSampleNPattern0 (style c),

                            tarolaRhythmPattern0 = tarolaRhythmPattern0 (style c),
                            tarolaSampleNPattern0 =tarolaSampleNPattern0 (style c),

                            efectoRhythmPattern0 = efectoRhythmPattern0 (style c),
                            efectoSampleNPattern0 = efectoSampleNPattern0 (style c),
                            efectoPitchPattern0 = efectoPitchPattern0 (style c)
                          }

preset 1 c = c {style = nuevoE}
  where nuevoE = (style c) {
                            pianoRhythmPattern0 = pianoRhythmPattern1 (style c), -- ie. [𝄽 ♩ 𝄽 ♩],
                            pianoSampleNPattern0 = pianoSampleNPattern1 (style c),

                            bassRhythmPattern0 = bassRhythmPattern1 (style c),  --i.e. [♩ 𝄽  ♩ 𝄽 ],
                            bassSampleNPattern0 = pianoSampleNPattern0 (style c),
                            bassPitchPattern0 = bassPitchPattern1 (style c)
                          }

preset 2 c = c {style = nuevoE}
   where nuevoE = (style c) {
                            bassRhythmPattern0 = bassRhythmPattern2 (style c),  --i.e. [♩ 𝄽  ♩ 𝄽 ],
                            bassSampleNPattern0 = pianoSampleNPattern0 (style c),
                            bassPitchPattern0 = bassPitchPattern2 (style c)
                            }

preset _ c = preset 0 c

parseAlternar :: H Layer
parseAlternar = parseAlternar' <*> parseLayer

parseAlternar' :: H (Layer -> Layer)
parseAlternar' = parseAlternar'' <*> parseLayerToLayerFunc

parseAlternar'' :: H ((Layer -> Layer) -> Layer -> Layer)
parseAlternar'' = parseAlternar''' <*> int

parseAlternar''' :: H (Int -> (Layer -> Layer) -> Layer -> Layer)
parseAlternar''' = alternar <$ reserved "alternar"

parseLayerToLayerFunc :: H (Layer -> Layer)
parseLayerToLayerFunc = parseSeleccionarEstilo'
                      <|> parseSeleccionarSample'
                      <|> parseSeleccionarSamples'
                      <|> parseTonicaYquinta'
                      <|> parseTonicaYquinta2'
                      <|> parseTonicaQoctava'
                      <|> parseTonicaQtercera'
                      <|> parseCambiarNota'
                      <|> parseCambiarNotas'
                      <|> parseCambiarRitmo'
                      <|> parseCambiarRitmos'
                      <|> parseCambiarIntervaloConString'
                      <|> parsePreset'
                      <|> parseAlternar'

seleccionarSampleF ::  Layer -> Layer
seleccionarSampleF c =  c {style = nuevoE}
   where
      index = 1
      nuevoE = (style c) {
                             cuerdaSampleNPattern0 = [index],
                             pianoSampleNPattern0 =  [index],
                             bassSampleNPattern0 =  [index],
                             guiraSampleNPattern0 = [index],
                             contrasSampleNPattern0 = [index],
                             tarolaSampleNPattern0 = [index],
                             efectoSampleNPattern0 = [index]
                            }

alternar :: Int -> (Layer -> Layer) -> Layer -> Layer
alternar 0 f x = Layer { getEvents = updatedEv, style = style x}
  where
    f0 = getEvents x
    s0 = style x
    updatedEv gm s t iw ew = f0 gm s0 t iw ew

alternar 1 f x = Layer { getEvents = updatedEv, style = style (f x)}
  where
    f1 = getEvents (f x)
    s1 = style (f x)
    updatedEv gm s t iw ew = f1 gm s1 t iw ew

alternar n f x = Layer { getEvents = updatedEv, style = style (f x)}
  where
    f0 = getEvents x -- lista orginal de evs
    f1 = getEvents (f x) -- lista nueva de evs
    s0 = style x
    s1 = style (f x)

    updatedEv gm s t iw ew = liftM concat $ liftM2 (++) es0  es1
      where
        iw' = timeToCount t iw -- Rational
        ew' = timeToCount t ew --

        (w0, w1) = alternarWindows n iw' ew' -- find all the windows that comply with certain condition within the provided window

        es0 = mapM (\(i,e) -> f0 gm s0 t (countToTime t i) (countToTime t e)) w0 -- State LayerState [[Events]]
        es1 = mapM (\(i,e) -> f1 gm s1 t (countToTime t i) (countToTime t e)) w1

    -- updatedStyle gm s t iw ew = if (mod' iw' (realToFrac n) /= realToFrac n - 1) then s0 else s1
    --   where
    --     iw' = timeToCount t iw -- Rational
    --     ew' = timeToCount t ew --


alternarWindows n iw ew = do
  let lista = [realToFrac $ floor iw .. realToFrac $ floor ew]
  let lista' = fmap (\e -> (e, e + 1)) lista
  let lista'' = drop 1 $ init lista'
  let lista''' = firstItem : lista'' ++ lastItem
      firstItem | (realToFrac $ floor iw) == (realToFrac $ floor ew) = (realToFrac iw, realToFrac ew)
                | otherwise = (realToFrac iw, (realToFrac $ floor iw) + 1)

      lastItem | (realToFrac $ floor ew) == (realToFrac $ floor iw) = []
               | (realToFrac ew) > (realToFrac $ floor ew) = [(realToFrac $ floor ew, realToFrac ew)]
               | otherwise = []
  let x = catMaybes $ fmap (\x -> if (mod' (realToFrac $ floor $ fst x) (realToFrac n) /= (realToFrac n -1)) then Just x else Nothing) lista'''
  let fx = catMaybes $ fmap (\x -> if (mod' (realToFrac $ floor $ fst x) (realToFrac n) == (realToFrac n -1)) then Just x else Nothing ) lista'''
  (x, fx)


-- test funcs
-- let l = alternar 3 (seleccionarSampleF) (seleccionarEstilo cumbia bajo)
-- (runState  (getEvents l testgmm cumbia  mytempo (mytime 0) (mytime 1)) emptyLayerState)

-- helper functions

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
render :: ([Layer], GlobalMaterial) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime, Map.Map T.Text H.Datum)]
render (ls, gm) tempo iw ew = Prelude.concat $ fmap (\l -> render' (l, gm) tempo iw ew) ls


render' :: (Layer, GlobalMaterial) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime, Map.Map T.Text H.Datum)]
render' (layer, gm) tempo iw ew = do
   fst $ runState x emptyLayerState --this should be another argument to my render function
    where
      x = getEvents layer gm (style layer) tempo iw ew
