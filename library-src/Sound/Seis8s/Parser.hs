{-# LANGUAGE OverloadedStrings #-}

module Sound.Seis8s.Parser (parseLang, render, renderForStandalone) where

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
import Data.List.Split (splitOn)
import Data.Bifunctor
import Data.Tempo
import Data.Time
import Data.Fixed
import Data.Maybe
import Data.Char as C
import qualified Sound.OSC as H

type H = Haskellish GlobalMaterial

-- data Program = Program [Layer] GlobalMaterial
-- type Program = ([Layer], GlobalMaterial)

-- f :: (Style, Intrument)
-- f cumbia teclado

-- so I can do :
-- cumbia teclado
-- (noDownBeats cumbia) teclado
parseLang :: String -> Either String ([Layer], GlobalMaterial)
parseLang s | all C.isSpace s = return ([emptyLayer], defaultGlobalMaterial)
            | otherwise = do
              let s' = preParse s
              (f . Exts.parseExp) $  ( "do {" ++ s' ++ "}" )
            -- | otherwise = (f . Exts.parseExp) $  ( "do {" ++ s ++ "}" )
    where
      f (Exts.ParseOk x) = runHaskellish layers defaultGlobalMaterial x -- Either String (a, st)
      f (Exts.ParseFailed l s) = Left s


preParse :: String -> String
preParse s = do
  let s' = T.replace (T.pack "do,") (T.pack "di") (T.pack s) -- Text
  let s'' = T.replace (T.pack "M") (T.pack "maj") s' -- Text
  let s''' = T.unpack $ T.replace (T.pack "remaj") (T.pack "re maj") s'' --String
  s'''


-- working on functions for comments
-- parseLang' :: String -> Either String (String, ())
-- parseLang' s = do
--   let sourceAsList = "[" ++ (List.intercalate "," $ fmap (++ " _0") $ splitOn ";" s) ++ "\n]"
--   (f . Exts.parseExp) $ sourceAsList
--     where
--       f (Exts.ParseOk x) = runHaskellish test () x -- Either String (a, st)
--       f (Exts.ParseFailed _ s) = Left s
--
--
-- listTest :: Haskellish () [String]
-- listTest = list test
--
-- test :: Haskellish () String
-- test = _0Arg'$ reserved "test" >> return "testOk"
--
-- _0Arg' :: Haskellish () a -> Haskellish () a
-- _0Arg' p = fmap fst (functionApplication p $ reserved "_0")


layers :: H [Layer]
layers =  listOfDoStatements statement

statement :: H Layer
statement =  parseLayer <|> globalStatement <|> silencio

-- GlobalMaterial
-- e.g. clave dosTres --gets parsed 1st that changes the state
-- then harmony Cmaj Emin -- 2nd, and changes the state
-- then clave tresDos -- 3rd -- and changes the state

globalStatement :: H Layer
globalStatement =  do
  f <- globalMaterialFunctions -- progressionToGm
  st <- get
  let newState = f st
  put newState
  return emptyLayer

silencio :: H Layer
silencio =  emptyLayer <$ reserved "silencio"

globalMaterialFunctions :: H (GlobalMaterial -> GlobalMaterial)
globalMaterialFunctions = parseSetChordProg
                       <|> parseCompasPartido
                       <|> parsetempoAGlobalMaterial

-- compás "partido"
-- compás "¢"
-- compas "4/4"
parseCompasPartido :: H (GlobalMaterial -> GlobalMaterial)
parseCompasPartido = parseCompasPartido' <*> string

parseCompasPartido' :: H (String -> GlobalMaterial -> GlobalMaterial)
parseCompasPartido' = compasAGlobalMaterial <$ reserved "compas"


compasAGlobalMaterial :: String -> GlobalMaterial -> GlobalMaterial
compasAGlobalMaterial s gm = gm {compas = establecerCompas s}


parsetempoAGlobalMaterial :: H (GlobalMaterial -> GlobalMaterial)
parsetempoAGlobalMaterial = parsetempoAGlobalMaterial' <*> double

parsetempoAGlobalMaterial' :: H (Double -> GlobalMaterial -> GlobalMaterial)
parsetempoAGlobalMaterial' = tempoAGlobalMaterial <$ reserved "tempo"

tempoAGlobalMaterial :: Double -> GlobalMaterial -> GlobalMaterial
tempoAGlobalMaterial newFreq gm = gm {tempoForStandalone =  Tempo { freq = toRational $ newFreq, time=mytime 0, Data.Tempo.count=0}}
--
parseSetChordProg :: H (GlobalMaterial -> GlobalMaterial)
parseSetChordProg = parseSetChordProgWMetre
                 <|> parseSetChordProgMetreAuto

parseSetChordProgWMetre :: H (GlobalMaterial -> GlobalMaterial)
parseSetChordProgWMetre = parseSetChordProgWMetre' <*> chordList

parseSetChordProgWMetre' :: H ([Chord] -> GlobalMaterial -> GlobalMaterial)
parseSetChordProgWMetre' = parseSetChordProgWMetre'' <*> rationalOrInteger

parseSetChordProgWMetre'' :: H (Metre -> [Chord] -> GlobalMaterial -> GlobalMaterial)
parseSetChordProgWMetre'' = setChordProg <$ (reserved "armonia" <|> reserved "acordes")

setChordProg :: Metre -> [Chord] -> GlobalMaterial -> GlobalMaterial
setChordProg metre hs gm = gm { harmony = castProgression metre (compas gm) (multiplicarCompasInicioYFinal (compas gm) hs)}

-- parseSetChordProgMetreAuto :: H (GlobalMaterial -> GlobalMaterial)
-- parseSetChordProgMetreAuto = (reserved "armonia" >> return setChordProgMetreAuto) <*>  chordListMetreAuto

parseSetChordProgMetreAuto :: H (GlobalMaterial -> GlobalMaterial)
parseSetChordProgMetreAuto =  parseSetChordProgMetreAuto' <*> listaDePitchOrPitchType -- chordListMetreAuto

parseSetChordProgMetreAuto' :: H ([(Pitch, ChordType)] -> GlobalMaterial -> GlobalMaterial)
parseSetChordProgMetreAuto' =  setChordProgMetreAuto <$ (reserved "armonia" <|> reserved "acordes")

setChordProgMetreAuto :: [(Pitch, ChordType)] -> GlobalMaterial -> GlobalMaterial
setChordProgMetreAuto hs gm = gm { harmony = castProgressionMetreAuto (compas gm) (listaDePitchOPitchWType hs)}

-- listaDePitchOPitchWType :: Double -> [(Pitch, ChordType)] -> [Chord]


chordList :: H [Chord]
chordList = list $ (chordParser <|> chordParserMajAuto)

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

--
chordParserMajAuto :: H Chord
chordParserMajAuto = chordParserMajAuto' <*> rationalOrInteger

chordParserMajAuto' :: H (Rational -> Chord)
chordParserMajAuto' = chordParserMajAuto'' <*> rationalOrInteger

chordParserMajAuto'' :: H (Rational -> Rational -> Chord)
chordParserMajAuto'' = do
  p <- pitchParser
  return $ \s e -> castHarmonyMajAuto p s e


--parses a list of chords with default dur of 1c, e.g. armonia [c maj, d maj]
chordListMetreAuto :: H [Chord]
chordListMetreAuto = parsePitchtochord
                  <|> chordListMetreAuto'

--
--
parsePitchtochord :: H [Chord]
parsePitchtochord = do
  p <- list pitchParser
  return $ pitchtochord p

pitchtochord :: [Pitch] -> [Chord]
pitchtochord ps = do
  let startandend = fmap (\s -> (toRational s, toRational s+1)) [0 .. (length ps)]
  let zipXSwithStartEnd = zip ps startandend --[(x,())]
  fmap (\(p, (s,e)) -> Chord p major (s,e)) zipXSwithStartEnd

pitchAPicthYTipo :: Pitch -> (Pitch, ChordType)
pitchAPicthYTipo p = (p, major)

--
chordListMetreAuto' :: H [Chord]
chordListMetreAuto' = do
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

-- start here
pitchandtypetochord :: [(Pitch, ChordType)] -> [Chord]
pitchandtypetochord xs = do
  let startandend = fmap (\s -> (toRational s, toRational s+1)) [0 .. (length xs)]
  let zipXSwithStartEnd = zip xs startandend --[((),())]
  fmap (\((p,t), (s,e)) -> Chord p t (s,e)) zipXSwithStartEnd

--
-- parsePitchandtypetoPitchType :: H ()

listaDePitchOrPitchType :: H [(Pitch, ChordType)]
listaDePitchOrPitchType = list (parsePitchToPitchType <|> parsePitchandtypetoPitchType)

parsePitchToPitchType :: H (Pitch, ChordType)
parsePitchToPitchType = do
  p <- pitchParser
  return $ pitchToPitchType p

pitchToPitchType :: Pitch -> (Pitch, ChordType)
pitchToPitchType p = (p, major)


parsePitchandtypetoPitchType :: H (Pitch, ChordType)
parsePitchandtypetoPitchType = parsePitchandtypetoPitchType' <*> chordTypeParser


parsePitchandtypetoPitchType' :: H (ChordType -> (Pitch, ChordType))
parsePitchandtypetoPitchType' = do
  p <- pitchParser
  return $ \t -> pitchandtypetoPitchType p t

pitchandtypetoPitchType :: Pitch -> ChordType -> (Pitch, ChordType)
pitchandtypetoPitchType p t = (p, t)

--
listaDePitchOPitchWType :: [(Pitch, ChordType)] -> [Chord]
listaDePitchOPitchWType xs = do
  let startandend = fmap (\s -> (toRational s, toRational s+1)) [0 .. (length xs)]
  let zipXSwithStartEnd = zip xs startandend --[((),())]
  fmap (\((p,t), (s,e)) -> Chord p t (s, e)) zipXSwithStartEnd

--
multiplicarCompasInicioYFinal :: Double -> [Chord] -> [Chord]
multiplicarCompasInicioYFinal compas hs = fmap (multiplicarCompasInicioYFinal' compas) hs

multiplicarCompasInicioYFinal' :: Double -> Chord -> Chord
multiplicarCompasInicioYFinal' compas (Chord p t (s, e)) = Chord p t (s * toRational compas, e * toRational compas)

-- armonia 1 [C maj 0 1]
castProgression :: Rational -> Double -> [Chord] -> Progression
castProgression metre compas cs  = Progression (metre * toRational compas) cs

-- armonia 1 [C maj 0 1]
castProgressionMetreAuto :: Double -> [Chord] -> Progression
castProgressionMetreAuto compas cs = do
  let metre = realToFrac $ length cs
  let cs' = multiplicarCompasInicioYFinal compas cs
  Progression (metre * toRational compas) cs'

castHarmony :: Pitch -> ChordType -> Rational -> Rational ->  Chord
castHarmony p t s e = Chord p t (s, e)

castHarmonyMajAuto :: Pitch -> Rational -> Rational -> Chord
castHarmonyMajAuto p s e = Chord p major (s, e)


pitchParser :: H Pitch
pitchParser =
               c <$ (reserved "c" <|> reserved "di")
           <|> cs <$ (reserved "c'" <|> reserved "do'")
           <|> cs <$ (reserved "db" <|> reserved "reb")
           <|> d <$ (reserved "d" <|> reserved "re")
           <|> ds <$ (reserved "d'" <|> reserved "re'")
           <|> ds <$ (reserved "eb" <|> reserved "mib")
           <|> e <$ (reserved "e" <|> reserved "mi")
           <|> f <$ (reserved "f" <|> reserved "fa")
           <|> fs <$ (reserved "f'" <|> reserved "fa'")
           <|> fs <$ (reserved "gb" <|> reserved "solb")
           <|> g <$ (reserved "g" <|> reserved "sol")
           <|> gs <$ (reserved "g'" <|> reserved "sol'")
           <|> gs <$ (reserved "ab" <|> reserved "lab")
           <|> a <$ (reserved "a" <|> reserved "la")
           <|> as <$ (reserved "a'" <|> reserved "la'")
           <|> as <$ (reserved "bb" <|> reserved "sib")
           <|> b <$ (reserved "b" <|> reserved "si")

chordTypeParser :: H ChordType
chordTypeParser =
                  major <$ (reserved "maj" <|> reserved "M")
              <|> minor <$ (reserved "min" <|> reserved "m")
              <|> major7 <$ (reserved "maj7" <|> reserved "M7")
              <|> minor7 <$ (reserved "min7" <|> reserved "m7")
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
                      <|> parseCambiarRitmoAuto
                      <|> parseCambiarRitmosAuto
                      <|> parseCambiarIntervalo
                      <|> parseCambiarIntervalos
                      <|> parseCambiarIntervaloConOctava
                      <|> parseCambiarIntervalosConOctava
                      <|> parseCambiarIntervaloDouble
                      <|> parseCambiarIntervaloDoubleConOctava
                      <|> parseCambiarIntervalosDouble
                      <|> parseCambiarIntervalosDoubleConOctava
                      <|> parsePreset
                      <|> parseAlternar
                      <|> parseCambiarGain
                      <|> parseCambiarPaneo
                      <|> parsePunteo
                      <|> parsePunteos
                      <|> parseTumbao
                      <|> parseaTumbaoBajoVoicingSel
                      <|> parseaTumbaoBajoVoicingYRitmoSel
                      <|> parseaTumbaoBajoVoicingsYRitmoSel
                      <|> parseTumbaoCongasGolpesSel
                      <|> parseTumbaoCongasGolpesYRitmoSel
                      <|> parseTumbaoCongasListaDeGolpesSel
                      <|> parseTumbaoCongasListaDeGolpesYRitmoSel
                      <|> parseacompanamiento
                      <|> parseacompanamientos
                      <|> parseAcompanamientoConVoicingSel
                      <|> parseAcompanamientosConVoicingSel
--
inst :: H Layer
inst =
        teclado <$ reserved "teclado"
    <|> bajo <$ reserved "bajo"
    <|> guira <$ (reserved "guira" <|> reserved "guiro")
    <|> contras <$ reserved "contratiempos"
    <|> cuerda <$ reserved "cuerda"
    <|> acordeon <$ reserved "acordeon"
    <|> zampoña <$ (reserved "zampoña" <|> reserved "flauta")
    <|> tarola <$ reserved "tarola"
    <|> efecto <$ reserved "efecto"
    <|> altavoz <$ reserved "altavoz"
    <|> clave <$ reserved "clave"
    <|> jamblock <$ (reserved "jamblock" <|> reserved "jam" <|> reserved "block")
    <|> congas <$ reserved "congas"
    <|> extras <$ reserved "extras"


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

-- a function for selecting a different sample n, e.g. (sample [2] cumbia) teclado
parseSeleccionarSamples :: H Layer
parseSeleccionarSamples = parseSeleccionarSamples' <*> parseLayer

parseSeleccionarSamples' :: H (Layer -> Layer)
parseSeleccionarSamples' = parseSeleccionarSamples'' <*> parseNPattern1

parseSeleccionarSamples'' :: H (NPattern -> Layer -> Layer)
parseSeleccionarSamples'' = seleccionarSamples <$ reserved "sample"

seleccionarSamples :: NPattern -> Layer -> Layer
seleccionarSamples is c =  c {style = nuevoE}
  where nuevoE = (style c) {
                           acordeonSampleNPattern0 = is,
                           zampoñaSampleNPattern0 = is,
                           cuerdaSampleNPattern0 = is,
                           tecladoSampleNPattern0 =  is,
                           bassSampleNPattern0 =  is,
                           guiraSampleNPattern0 = is,
                           contrasSampleNPattern0 = is,
                           tarolaSampleNPattern0 = is,
                           efectoSampleNPattern0 = is,
                           altavozSampleNPattern0 = is,
                           extrasSampleNPattern0 = is,
                           jamblockSampleNPattern0 = is,
                           claveSampleNPattern0 = is
                           -- congasSampleNPattern0 = is
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
                             cuerdaSampleNPattern0 = NPattern1 [index],
                             acordeonSampleNPattern0 = NPattern1 [index],
                             zampoñaSampleNPattern0 = NPattern1 [index],
                             tecladoSampleNPattern0 = NPattern1 [index],
                             bassSampleNPattern0 = NPattern1 [index],
                             guiraSampleNPattern0 = NPattern1 [index],
                             contrasSampleNPattern0 = NPattern1 [index],
                             tarolaSampleNPattern0 = NPattern1 [index],
                             efectoSampleNPattern0 = NPattern1 [index],
                             altavozSampleNPattern0 = NPattern1 [index],
                             extrasSampleNPattern0 = NPattern1 [index],
                             claveSampleNPattern0 = NPattern1 [index],
                             jamblockSampleNPattern0 = NPattern1 [index],
                             congasSampleNPattern0 = NPattern1 [index]
                            }

-- TO DO:  punteo [mi do] ... Y CON MIDINOTE
-- punteo "1a" 3 $ cumbia teclado;
-- parsePunteoConMidiNote :: H Layer
-- parsePunteoConMidiNote = parsePunteoConMidiNote' <*> parseLayer
--
-- parsePunteoConMidiNote' :: H (Layer -> Layer)
-- parsePunteoConMidiNote' =  parsePunteoConMidiNote'' <*> rationalOrInteger
--
-- parsePunteoConMidiNote'' :: H (Rational -> Layer -> Layer)
-- parsePunteoConMidiNote'' = parsePunteoConMidiNote''' <*>  parseUnNAListaDeN
--
-- parsePunteoConMidiNote''' :: H ([N] -> Rational -> Layer -> Layer)
-- parsePunteoConMidiNote''' = punteoConMidiNote <$ (reserved "punteo")
--
-- punteoConMidiNote :: [N] -> Rational -> Layer -> Layer
-- punteoConMidiNote nota ataque c = c {style = nuevoE}
--   where
--     rPat = cambiarRitmo'''' 1 [[ataque]]
--     nuevoE = (style c) {
--                     tecladoRhythmPattern0 = rPat, --
--                     tecladoPitchPattern0 = ("midinote", nota), --new ("intervalo", concat notes)-- ("acorde", [note])
--                     acordeonRhythmPattern0 =  rPat,
--                     acordeonPitchPattern0 = ("midinote", nota),
--
--                     cuerdaRhythmPattern0 =  rPat,
--                     cuerdaPitchPattern0 = ("midinote", nota),
--
--                     extrasRhythmPattern0 =  rPat,
--                     extrasPitchPattern0 = ("midinote", nota)
--   }

-- punteo "1a" 3 $ cumbia teclado;
parsePunteo :: H Layer
parsePunteo = parsePunteo' <*> parseLayer

parsePunteo' :: H (Layer -> Layer)
parsePunteo' =  parsePunteo'' <*> rationalOrInteger

parsePunteo'' :: H (Rational -> Layer -> Layer)
parsePunteo'' =parsePunteo''' <*> parseUnStringAListadeNotas --

parsePunteo''' :: H ([Note] -> Rational -> Layer -> Layer)
parsePunteo''' = punteo <$ (reserved "punteo")

punteo :: [Note] -> Rational -> Layer -> Layer
punteo nota ataque c = c {style = nuevoE}
  where
    rPat = cambiarRitmo'''' 1 [[ataque]]
    nuevoE = (style c) {
                    tecladoRhythmPattern0 = rPat, --
                    tecladoPitchPattern0 = ("intervalo", nota), --new ("intervalo", concat notes)-- ("acorde", [note])

                    acordeonRhythmPattern0 =  rPat,
                    acordeonPitchPattern0 = ("intervalo", nota),

                    zampoñaRhythmPattern0 =  rPat,
                    zampoñaPitchPattern0 = ("intervalo", nota),

                    cuerdaRhythmPattern0 =  rPat,
                    cuerdaPitchPattern0 = ("intervalo", nota),

                    extrasRhythmPattern0 =  rPat,
                    extrasPitchPattern0 = ("intervalo", nota)
  }

-- punteo ["f" "5a", "f" "3a" "5a"] [1 3, 1 3 4] $ cumbia acordeon;
parsePunteos :: H Layer
parsePunteos = parsePunteos' <*> parseLayer

parsePunteos' :: H (Layer -> Layer)
parsePunteos' = parsePunteos'' <*> parseListasDeListasDeAtaques -- rationalList --

parsePunteos'' :: H ( [[Rational]] -> Layer -> Layer)
parsePunteos'' = parsePunteos''' <*> praseListaDeListaStringAListaDeAcordes

parsePunteos''' :: H ([[Note]] -> [[Rational]] -> Layer -> Layer)
parsePunteos''' = listaDepunteos <$ (reserved "punteo")

listaDepunteos :: [[Note]] -> [[Rational]] -> Layer -> Layer
listaDepunteos notes rs c = c {style = nuevoE}
  where
    metre = toRational $ length rs -- [[Nothing], [1, 2, 3]] = metre 2 -- (realToFrac $ floor rs') + 1
    rPat = cambiarRitmo'''' metre rs
    nPat (NPattern1 xs) = NPattern1 $ concat $ replicate (length rPat) xs
    pPat =  take (length rPat) $ concat notes -- new
    nuevoE = (style c) {
                            tecladoRhythmPattern0 = rPat, --
                            tecladoSampleNPattern0 = nPat $ tecladoSampleNPattern0 (style c), -- listaDeStringsANPattern nPat notes,
                            tecladoPitchPattern0 = ("intervalo", pPat), --new ("intervalo", concat notes)-- ("acorde", [note])

                            acordeonRhythmPattern0 = rPat,
                            acordeonSampleNPattern0 = nPat $ acordeonSampleNPattern0 (style c),
                            acordeonPitchPattern0 = ("intervalo", pPat),

                            zampoñaRhythmPattern0 = rPat,
                            zampoñaSampleNPattern0 = nPat $ acordeonSampleNPattern0 (style c),
                            zampoñaPitchPattern0 = ("intervalo", pPat),

                            cuerdaRhythmPattern0 = rPat,
                            cuerdaSampleNPattern0 = nPat $ cuerdaSampleNPattern0 (style c),
                            cuerdaPitchPattern0 = ("intervalo", pPat),

                            extrasRhythmPattern0 = rPat,
                            extrasSampleNPattern0 = nPat $ extrasSampleNPattern0 (style c),
                            extrasPitchPattern0 = ("intervalo", pPat)
                          }


-- tumbao ("f" "3a" "5a") $ cumbia bajo;
parseaTumbaoBajoVoicingSel :: H Layer
parseaTumbaoBajoVoicingSel = parseaTumbaoBajoVoicingSel' <*> parseLayer

parseaTumbaoBajoVoicingSel' :: H (Layer -> Layer)
parseaTumbaoBajoVoicingSel' = parseaTumbaoBajoVoicingSel'' <*> parseStringsAListaDeNotes --

parseaTumbaoBajoVoicingSel'' :: H ([Note] -> Layer -> Layer)
parseaTumbaoBajoVoicingSel'' = tumbaoBajoVoicingSel <$ (reserved "tumbao")

tumbaoBajoVoicingSel :: [Note] -> Layer -> Layer
tumbaoBajoVoicingSel notes c = c {style = nuevoE}
  where
    nuevoE = (style c) {
                            bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)], --
                            bassSampleNPattern0 = bassSampleNPattern0 (style c), -- listaDeStringsANPattern nPat notes,
                            bassPitchPattern0 = ("intervalo", notes)-- ("acorde", [note])
                          }

-- tumbao ("f" 5a") (1 3) $ cumbia bajo;
parseaTumbaoBajoVoicingYRitmoSel :: H Layer
parseaTumbaoBajoVoicingYRitmoSel = parseaTumbaoBajoVoicingYRitmoSel' <*> parseLayer

parseaTumbaoBajoVoicingYRitmoSel' :: H (Layer -> Layer)
parseaTumbaoBajoVoicingYRitmoSel' = parseaTumbaoBajoVoicingYRitmoSel'' <*> parseAtaquesAListaDeAtaques -- [] -- rationalList --

parseaTumbaoBajoVoicingYRitmoSel'' :: H ( [Rational] -> Layer -> Layer)
parseaTumbaoBajoVoicingYRitmoSel'' = parseaTumbaoBajoVoicingYRitmoSel''' <*> parseStringsAListaDeNotes

parseaTumbaoBajoVoicingYRitmoSel''' :: H ([Note] -> [Rational] -> Layer -> Layer)
parseaTumbaoBajoVoicingYRitmoSel''' = tumbaoBajoVoicingYRitmoSel <$ (reserved "tumbao")

tumbaoBajoVoicingYRitmoSel :: [Note] -> [Rational] -> Layer -> Layer
tumbaoBajoVoicingYRitmoSel notes rs c = c {style = nuevoE}
  where
    metre = 1
    -- indices = [0 .. metre]
    -- rs' = fmap (\n -> if (n == 0) then 0 else (abs $ n - 1)) rs -- [1, 2, 3, 4] a [0, 1, 2, 3]
    rPat = cambiarRitmo'' metre rs -- fmap (\n -> (metre, (realToFrac n) /4)) rs'-- [(1, (realToFrac n') / 4)]
    nPat (NPattern1 xs) = NPattern1 $ concat $ replicate (length rPat) xs
    pPat =  take (length rPat) notes -- new
    nuevoE = (style c) {
                            bassRhythmPattern0 = rPat, --
                            bassSampleNPattern0 =  nPat $ bassSampleNPattern0 (style c), -- listaDeStringsANPattern nPat notes,
                            bassPitchPattern0 = ("intervalo", pPat)-- new ("intervalo", notes)-- ("acorde", [note])
                          }

-- tumbao ["f" "5a", "f" "3a" "5a"] [1 3, 1 3 4] $ cumbia bajo;
parseaTumbaoBajoVoicingsYRitmoSel :: H Layer
parseaTumbaoBajoVoicingsYRitmoSel = parseaTumbaoBajoVoicingsYRitmoSel' <*> parseLayer

parseaTumbaoBajoVoicingsYRitmoSel' :: H (Layer -> Layer)
parseaTumbaoBajoVoicingsYRitmoSel' = parseaTumbaoBajoVoicingsYRitmoSel'' <*> parseListasDeListasDeAtaques -- rationalList --

parseaTumbaoBajoVoicingsYRitmoSel'' :: H ( [[Rational]] -> Layer -> Layer)
parseaTumbaoBajoVoicingsYRitmoSel'' = parseaTumbaoBajoVoicingsYRitmoSel''' <*> praseListaDeListaStringAListaDeAcordes

parseaTumbaoBajoVoicingsYRitmoSel''' :: H ([[Note]] -> [[Rational]] -> Layer -> Layer)
parseaTumbaoBajoVoicingsYRitmoSel''' = tumbaoBajoVoicingsYRitmoSel <$ (reserved "tumbao")

tumbaoBajoVoicingsYRitmoSel :: [[Note]] -> [[Rational]] -> Layer -> Layer
tumbaoBajoVoicingsYRitmoSel notes rs c = c {style = nuevoE}
  where
    -- rs' = ((maximum $ concat rs) - 1) / 4
    metre = toRational $ length rs -- [[Nothing], [1, 2, 3]] = metre 2 -- (realToFrac $ floor rs') + 1
    rPat = cambiarRitmo'''' metre rs
    nPat (NPattern1 xs) = NPattern1 $ concat $ replicate (length rPat) xs
    pPat =  take (length rPat) $ concat notes -- new
    nuevoE = (style c) {
                            bassRhythmPattern0 = rPat, --
                            bassSampleNPattern0 = nPat $ bassSampleNPattern0 (style c), -- listaDeStringsANPattern nPat notes,
                            bassPitchPattern0 = ("intervalo", pPat) --new ("intervalo", concat notes)-- ("acorde", [note])
                          }

-- marcha ("p" "t" "p" "a") $ cumbia congas -- accepts only 4 beats
parseTumbaoCongasGolpesSel :: H Layer
parseTumbaoCongasGolpesSel = parseTumbaoCongasGolpesSel' <*> parseLayer

parseTumbaoCongasGolpesSel' :: H (Layer -> Layer)
parseTumbaoCongasGolpesSel' = parseTumbaoCongasGolpesSel'' <*> parseNAListaDeN -- ["a" "t"] = ["a", "t"]-- congasN

parseTumbaoCongasGolpesSel'' :: H ([N] -> Layer -> Layer)
parseTumbaoCongasGolpesSel'' = tumbaoCongasGolpesSel <$ (reserved "tumbao" <|> reserved "marcha")

tumbaoCongasGolpesSel :: [N] -> Layer -> Layer
tumbaoCongasGolpesSel xs c = c {style = nuevoE}
  where
    -- ns = fmap nSample xs --[nSample x1, nSample x2 ...] = [0, 1, ...]
    -- nPat = fmap (\x -> ("quinto", x)) ns -- [("quinto", 0), ("quinto", 1) ...]
    rPat = take (length xs) $ congasRhythmPattern0 (style c)
    pPat = take (length xs) $ snd $ congasPitchPattern0 (style c)
    nuevoE = (style c) {
      congasRhythmPattern0 = rPat,-- congasRhythmPattern0 (style c), -- [(1, 0), (1, 0.25), (1, 0.5), (1, 0.75)],
      congasSampleNPattern0 = NPattern2 xs, -- [("quinto", 0), ("quinto", 1) ...]
      congasPitchPattern0 = ("midinote", pPat) -- ("midinote", take 4 $ cycle [("mn", 60, 0)])
    }

--tumbao ("p" "t" "p" "a") (1 2 3 4 4.5) $ cumbia congas
parseTumbaoCongasGolpesYRitmoSel :: H Layer
parseTumbaoCongasGolpesYRitmoSel = parseTumbaoCongasGolpesYRitmoSel' <*> parseLayer

parseTumbaoCongasGolpesYRitmoSel' :: H (Layer -> Layer)
parseTumbaoCongasGolpesYRitmoSel' = parseTumbaoCongasGolpesYRitmoSel'' <*> parseAtaquesAListaDeAtaques-- parseListasDeListasDeAtaques -- rationalList -- ["a" "t"] = ["a", "t"]-- congasN

parseTumbaoCongasGolpesYRitmoSel'' :: H ([Rational] -> Layer -> Layer)
parseTumbaoCongasGolpesYRitmoSel'' = parseTumbaoCongasGolpesYRitmoSel''' <*> parseNAListaDeN -- ["a" "t"] = ["a", "t"]-- congasN

parseTumbaoCongasGolpesYRitmoSel''' :: H ([N] -> [Rational] -> Layer -> Layer)
parseTumbaoCongasGolpesYRitmoSel''' = tumbaoCongasGolpesYRitmoSel <$ (reserved "tumbao" <|> reserved "marcha")

tumbaoCongasGolpesYRitmoSel :: [N] -> [Rational] -> Layer -> Layer
tumbaoCongasGolpesYRitmoSel xs rs c = c {style = nuevoE}
  where
    metre = 1
    rPat = cambiarRitmo'' metre rs
    nPat = take (length rPat) xs
    pPat = take (length rPat) $ snd $ congasPitchPattern0 (style c)
    nuevoE = (style c) {
      congasRhythmPattern0 = rPat, -- [(1, 0), (1, 0.25), (1, 0.5), (1, 0.75)],
      congasSampleNPattern0 = NPattern2 nPat, -- [("quinto", 0), ("quinto", 1) ...]
      congasPitchPattern0 = ("midinote", pPat) -- ("midinote", take 4 $ cycle [("mn", 60, 0)])
    }


-- tumbao ["p" "s" "p" (q "a"), "p" "s" "p" (t "a")] $ cumbia congas; -- pendiente
parseTumbaoCongasListaDeGolpesSel :: H Layer
parseTumbaoCongasListaDeGolpesSel = parseTumbaoCongasListaDeGolpesSel' <*> parseLayer

parseTumbaoCongasListaDeGolpesSel' :: H (Layer -> Layer)
parseTumbaoCongasListaDeGolpesSel' = parseTumbaoCongasListaDeGolpesSel'' <*> parseListaDeNAListaDeListaDeN -- ["a" "t"] = ["a", "t"]-- congasN

parseTumbaoCongasListaDeGolpesSel'' :: H ([[N]] -> Layer -> Layer)
parseTumbaoCongasListaDeGolpesSel'' = tumbaoCongasListaDeGolpesSel <$ (reserved "tumbao" <|> reserved "marcha")

-- ahora es  tumbao ("p" "t" "p" "a") $ cumbia congas pero debe ser  tumbao ("p" "t" "p" "a") $ cumbia congas;
tumbaoCongasListaDeGolpesSel :: [[N]] -> Layer -> Layer
tumbaoCongasListaDeGolpesSel xs c = c {style = nuevoE}
  where
    -- metre = 1 * (length xs) -- eg. 1 * 2 => [..., ...]
    -- rs' = fmap (\(m, n) -> if (n == 0) then 0 else (abs $ n - 1)) rs -- [1, 2, 3, 4] a [0, 1, 2, 3]
    -- rPat = fmap (\(m, n) -> (metre, (realToFrac n) /4)) rs'-- [(1, (realToFrac n') / 4)]

    nuevoE = (style c) {
      congasRhythmPattern0 = congasRhythmPattern0 (style c), -- [(1, 0), (1, 0.25), (1, 0.5), (1, 0.75)],
      congasSampleNPattern0 = NPattern2 $ concat xs, -- [("quinto", 0), ("quinto", 1) ...]
      congasPitchPattern0 = congasPitchPattern0 (style c) -- ("midinote", take 4 $ cycle [("mn", 60, 0)])
    }

-- marcha ["p" "t" "p" (q "a"), "p" "t" "p" (t "a") (t "a")] [1 2 3 4, 1 2 3 4 4.5] $ cumbia congas;
parseTumbaoCongasListaDeGolpesYRitmoSel :: H Layer
parseTumbaoCongasListaDeGolpesYRitmoSel = parseTumbaoCongasListaDeGolpesYRitmoSel' <*> parseLayer

parseTumbaoCongasListaDeGolpesYRitmoSel' :: H (Layer -> Layer)
parseTumbaoCongasListaDeGolpesYRitmoSel' = parseTumbaoCongasListaDeGolpesYRitmoSel'' <*> parseListasDeListasDeAtaques -- rationalList

parseTumbaoCongasListaDeGolpesYRitmoSel'' :: H ([[Rational]] -> Layer -> Layer)
parseTumbaoCongasListaDeGolpesYRitmoSel'' = parseTumbaoCongasListaDeGolpesYRitmoSel''' <*> parseListaDeNAListaDeListaDeN

parseTumbaoCongasListaDeGolpesYRitmoSel''' :: H ([[N]] -> [[Rational]] -> Layer -> Layer)
parseTumbaoCongasListaDeGolpesYRitmoSel''' = tumbaoCongasListaDeGolpesYRitmoSel <$  (reserved "tumbao" <|> reserved "marcha")

tumbaoCongasListaDeGolpesYRitmoSel :: [[N]] -> [[Rational]] -> Layer -> Layer
tumbaoCongasListaDeGolpesYRitmoSel xs rs c = c {style = nuevoE}
  where
    metre = toRational $ length rs -- [[Nothing], [1, 2, 3]] = metre 2 -- (realToFrac $ floor rs') + 1
    rPat = cambiarRitmo'''' metre rs
    nPat = take (length rPat) $ concat xs
    pPat = take (length rPat) $ snd $ congasPitchPattern0 (style c)
    nuevoE = (style c) {
      congasRhythmPattern0 = rPat, -- [(1, 0), (1, 0.25), (1, 0.5), (1, 0.75)],
      congasSampleNPattern0 = NPattern2 nPat, -- [("quinto", 0), ("quinto", 1) ...]
      congasPitchPattern0 = ("midinote", pPat) -- ("midinote", take 4 $ cycle [("mn", 60, 0)])
    }

parseUnNAuto :: H (Maybe N)
parseUnNAuto = do
  s <- string
  return $ unNAuto s

unNAuto :: String -> Maybe N
unNAuto "p" = Just ("quinto", 0)
unNAuto "t" = Just ("quinto", 1)
unNAuto "a" = Just ("quinto", 2)
unNAuto _ = Nothing

parseUnN :: H (Maybe N)
parseUnN = parseUnN' <*> string

parseUnN' :: H (String -> Maybe N)
parseUnN' = do
  s1 <- string
  return $ \s2 -> unN s2 s2

parseQuinto :: H (Maybe N)
parseQuinto = parseQuinto' <*> string

parseQuinto' :: H (String -> Maybe N)
parseQuinto' = (unN "quinto") <$ reserved "q"

parseTumba :: H (Maybe N)
parseTumba = parseTumba' <*> string

parseTumba' :: H (String -> Maybe N)
parseTumba' = (unN "tumba") <$ reserved "t"

unN :: String -> String -> Maybe N
unN f "p" =  Just (f, 0)
unN f "t" =  Just (f, 1)
unN f "a" = Just (f, 2)
unN _ _ =   Nothing

parseCongasN :: H (Maybe N) -- [("quinto", 0), ("quinto", 1) ...]
parseCongasN = parseUnNAuto
            <|> parseQuinto --parseUnN
            <|> parseTumba

parseListaDeNAListaDeListaDeN :: H [[N]]
parseListaDeNAListaDeListaDeN = list parseNAListaDeN

parseNAListaDeN :: H [N]
parseNAListaDeN = parseUnNAListaDeN
               <|> parseDosNAListaDeN
               <|> parseTresNAListaDeN
               <|> parseCuatroNAListaDeN
               <|> parseCincoNAListaDeN
               <|> parseSeisNAListaDeN
               <|> parseSieteNAListaDeN
               <|> parseOchoNAListaDeN
               <|> parseNueveNAListaDeN
               <|> parseDiezNAListaDeN
               <|> parseOnceNAListaDeN
               <|> parseDoceNAListaDeN
               <|> parseTreceNAListaDeN
               <|> parseCatorceNAListaDeN
               <|> parseQuinceNAListaDeN
               <|> parseDieciseisNAListaDeN

-- ("p" "t" "p" (t "a") ...)
parseDieciseisNAListaDeN :: H [N]
parseDieciseisNAListaDeN = parseDieciseisNAListaDeN' <*> parseCongasN

parseDieciseisNAListaDeN' :: H (Maybe N -> [N])
parseDieciseisNAListaDeN' = parseDieciseisNAListaDeN'' <*> parseCongasN

parseDieciseisNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN'' = parseDieciseisNAListaDeN''' <*> parseCongasN

parseDieciseisNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN''' = parseDieciseisNAListaDeN'''' <*> parseCongasN

parseDieciseisNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN'''' = parseDieciseisNAListaDeN''''' <*> parseCongasN

parseDieciseisNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN''''' = parseDieciseisNAListaDeN'''''' <*> parseCongasN

parseDieciseisNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN'''''' = parseDieciseisNAListaDeN''''''' <*> parseCongasN

parseDieciseisNAListaDeN''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN''''''' = parseDieciseisNAListaDeN'''''''' <*> parseCongasN

parseDieciseisNAListaDeN'''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN'''''''' = parseDieciseisNAListaDeN''''''''' <*> parseCongasN

parseDieciseisNAListaDeN''''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN''''''''' = parseDieciseisNAListaDeN'''''''''' <*> parseCongasN

parseDieciseisNAListaDeN'''''''''' :: H (Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN'''''''''' = parseDieciseisNAListaDeN''''''''''' <*> parseCongasN

parseDieciseisNAListaDeN''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN''''''''''' = parseDieciseisNAListaDeN'''''''''''' <*> parseCongasN

parseDieciseisNAListaDeN'''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN'''''''''''' = parseDieciseisNAListaDeN''''''''''''' <*> parseCongasN

parseDieciseisNAListaDeN''''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN''''''''''''' = parseDieciseisNAListaDeN'''''''''''''' <*> parseCongasN

parseDieciseisNAListaDeN'''''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN'''''''''''''' = parseDieciseisNAListaDeN''''''''''''''' <*> parseCongasN

parseDieciseisNAListaDeN''''''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDieciseisNAListaDeN''''''''''''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 -> dieciseisNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16

dieciseisNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N]
dieciseisNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 = catMaybes [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16]

-- ("p" "t" "p" (t "a") ...)
parseQuinceNAListaDeN :: H [N]
parseQuinceNAListaDeN = parseQuinceNAListaDeN' <*> parseCongasN

parseQuinceNAListaDeN' :: H (Maybe N -> [N])
parseQuinceNAListaDeN' = parseQuinceNAListaDeN'' <*> parseCongasN

parseQuinceNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN'' = parseQuinceNAListaDeN''' <*> parseCongasN

parseQuinceNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN''' = parseQuinceNAListaDeN'''' <*> parseCongasN

parseQuinceNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN'''' = parseQuinceNAListaDeN''''' <*> parseCongasN

parseQuinceNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN''''' = parseQuinceNAListaDeN'''''' <*> parseCongasN

parseQuinceNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN'''''' = parseQuinceNAListaDeN''''''' <*> parseCongasN

parseQuinceNAListaDeN''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN''''''' = parseQuinceNAListaDeN'''''''' <*> parseCongasN

parseQuinceNAListaDeN'''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN'''''''' = parseQuinceNAListaDeN''''''''' <*> parseCongasN

parseQuinceNAListaDeN''''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN''''''''' = parseQuinceNAListaDeN'''''''''' <*> parseCongasN

parseQuinceNAListaDeN'''''''''' :: H (Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN'''''''''' = parseQuinceNAListaDeN''''''''''' <*> parseCongasN

parseQuinceNAListaDeN''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN''''''''''' = parseQuinceNAListaDeN'''''''''''' <*> parseCongasN

parseQuinceNAListaDeN'''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN'''''''''''' = parseQuinceNAListaDeN''''''''''''' <*> parseCongasN

parseQuinceNAListaDeN''''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN''''''''''''' = parseQuinceNAListaDeN'''''''''''''' <*> parseCongasN

parseQuinceNAListaDeN'''''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseQuinceNAListaDeN'''''''''''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 -> quinceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15

quinceNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N]
quinceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 = catMaybes [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15]

-- ("p" "t" "p" (t "a") ...)
parseCatorceNAListaDeN :: H [N]
parseCatorceNAListaDeN = parseCatorceNAListaDeN' <*> parseCongasN

parseCatorceNAListaDeN' :: H (Maybe N -> [N])
parseCatorceNAListaDeN' = parseCatorceNAListaDeN'' <*> parseCongasN

parseCatorceNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN'' = parseCatorceNAListaDeN''' <*> parseCongasN

parseCatorceNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN''' = parseCatorceNAListaDeN'''' <*> parseCongasN

parseCatorceNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN'''' = parseCatorceNAListaDeN''''' <*> parseCongasN

parseCatorceNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN''''' = parseCatorceNAListaDeN'''''' <*> parseCongasN

parseCatorceNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN'''''' = parseCatorceNAListaDeN''''''' <*> parseCongasN

parseCatorceNAListaDeN''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN''''''' = parseCatorceNAListaDeN'''''''' <*> parseCongasN

parseCatorceNAListaDeN'''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN'''''''' = parseCatorceNAListaDeN''''''''' <*> parseCongasN

parseCatorceNAListaDeN''''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN''''''''' = parseCatorceNAListaDeN'''''''''' <*> parseCongasN

parseCatorceNAListaDeN'''''''''' :: H (Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN'''''''''' = parseCatorceNAListaDeN''''''''''' <*> parseCongasN

parseCatorceNAListaDeN''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN''''''''''' = parseCatorceNAListaDeN'''''''''''' <*> parseCongasN

parseCatorceNAListaDeN'''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN'''''''''''' = parseCatorceNAListaDeN''''''''''''' <*> parseCongasN

parseCatorceNAListaDeN''''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCatorceNAListaDeN''''''''''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 -> catorceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14

catorceNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N]
catorceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 = catMaybes [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14]

-- ("p" "t" "p" (t "a") ...)
parseTreceNAListaDeN :: H [N]
parseTreceNAListaDeN = parseTreceNAListaDeN' <*> parseCongasN

parseTreceNAListaDeN' :: H (Maybe N -> [N])
parseTreceNAListaDeN' = parseTreceNAListaDeN'' <*> parseCongasN

parseTreceNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN'' = parseTreceNAListaDeN''' <*> parseCongasN

parseTreceNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN''' = parseTreceNAListaDeN'''' <*> parseCongasN

parseTreceNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN'''' = parseTreceNAListaDeN''''' <*> parseCongasN

parseTreceNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN''''' = parseTreceNAListaDeN'''''' <*> parseCongasN

parseTreceNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN'''''' = parseTreceNAListaDeN''''''' <*> parseCongasN

parseTreceNAListaDeN''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN''''''' = parseTreceNAListaDeN'''''''' <*> parseCongasN

parseTreceNAListaDeN'''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN'''''''' = parseTreceNAListaDeN''''''''' <*> parseCongasN

parseTreceNAListaDeN''''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN''''''''' = parseTreceNAListaDeN'''''''''' <*> parseCongasN

parseTreceNAListaDeN'''''''''' :: H (Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN'''''''''' = parseTreceNAListaDeN''''''''''' <*> parseCongasN

parseTreceNAListaDeN''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN''''''''''' = parseTreceNAListaDeN'''''''''''' <*> parseCongasN

parseTreceNAListaDeN'''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseTreceNAListaDeN'''''''''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 -> treceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13

treceNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N]
treceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 = catMaybes [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13]

-- ("p" "t" "p" (t "a") ...)
parseDoceNAListaDeN :: H [N]
parseDoceNAListaDeN = parseDoceNAListaDeN' <*> parseCongasN

parseDoceNAListaDeN' :: H (Maybe N -> [N])
parseDoceNAListaDeN' = parseDoceNAListaDeN'' <*> parseCongasN

parseDoceNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN'' = parseDoceNAListaDeN''' <*> parseCongasN

parseDoceNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN''' = parseDoceNAListaDeN'''' <*> parseCongasN

parseDoceNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN'''' = parseDoceNAListaDeN''''' <*> parseCongasN

parseDoceNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN''''' = parseDoceNAListaDeN'''''' <*> parseCongasN

parseDoceNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN'''''' = parseDoceNAListaDeN''''''' <*> parseCongasN

parseDoceNAListaDeN''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN''''''' = parseDoceNAListaDeN'''''''' <*> parseCongasN

parseDoceNAListaDeN'''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN'''''''' = parseDoceNAListaDeN''''''''' <*> parseCongasN

parseDoceNAListaDeN''''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN''''''''' = parseDoceNAListaDeN'''''''''' <*> parseCongasN

parseDoceNAListaDeN'''''''''' :: H (Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN'''''''''' = parseDoceNAListaDeN''''''''''' <*> parseCongasN

parseDoceNAListaDeN''''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDoceNAListaDeN''''''''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 -> doceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12

doceNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N]
doceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 = catMaybes [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12]

-- ("p" "t" "p" (t "a") ...)
parseOnceNAListaDeN :: H [N]
parseOnceNAListaDeN = parseOnceNAListaDeN' <*> parseCongasN

parseOnceNAListaDeN' :: H (Maybe N -> [N])
parseOnceNAListaDeN' = parseOnceNAListaDeN'' <*> parseCongasN

parseOnceNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseOnceNAListaDeN'' = parseOnceNAListaDeN''' <*> parseCongasN

parseOnceNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseOnceNAListaDeN''' = parseOnceNAListaDeN'''' <*> parseCongasN

parseOnceNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOnceNAListaDeN'''' = parseOnceNAListaDeN''''' <*> parseCongasN

parseOnceNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOnceNAListaDeN''''' = parseOnceNAListaDeN'''''' <*> parseCongasN

parseOnceNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOnceNAListaDeN'''''' = parseOnceNAListaDeN''''''' <*> parseCongasN

parseOnceNAListaDeN''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOnceNAListaDeN''''''' = parseOnceNAListaDeN'''''''' <*> parseCongasN

parseOnceNAListaDeN'''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOnceNAListaDeN'''''''' = parseOnceNAListaDeN''''''''' <*> parseCongasN

parseOnceNAListaDeN''''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOnceNAListaDeN''''''''' = parseOnceNAListaDeN'''''''''' <*> parseCongasN

parseOnceNAListaDeN'''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOnceNAListaDeN'''''''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 -> onceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11

onceNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N]
onceNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 = catMaybes [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11]

-- ("p" "t" "p" (t "a") ...)
parseDiezNAListaDeN :: H [N]
parseDiezNAListaDeN = parseDiezNAListaDeN' <*> parseCongasN

parseDiezNAListaDeN' :: H (Maybe N -> [N])
parseDiezNAListaDeN' = parseDiezNAListaDeN'' <*> parseCongasN

parseDiezNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseDiezNAListaDeN'' = parseDiezNAListaDeN''' <*> parseCongasN

parseDiezNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseDiezNAListaDeN''' = parseDiezNAListaDeN'''' <*> parseCongasN

parseDiezNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDiezNAListaDeN'''' = parseDiezNAListaDeN''''' <*> parseCongasN

parseDiezNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDiezNAListaDeN''''' = parseDiezNAListaDeN'''''' <*> parseCongasN

parseDiezNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDiezNAListaDeN'''''' = parseDiezNAListaDeN''''''' <*> parseCongasN

parseDiezNAListaDeN''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDiezNAListaDeN''''''' = parseDiezNAListaDeN'''''''' <*> parseCongasN

parseDiezNAListaDeN'''''''' :: H (Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDiezNAListaDeN'''''''' = parseDiezNAListaDeN''''''''' <*> parseCongasN

parseDiezNAListaDeN''''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseDiezNAListaDeN''''''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 -> diezNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10

diezNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> Maybe N -> [N]
diezNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 = catMaybes [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10]

-- ("p" "t" "p" (t "a") ...)
parseNueveNAListaDeN :: H [N]
parseNueveNAListaDeN = parseNueveNAListaDeN' <*> parseCongasN

parseNueveNAListaDeN' :: H (Maybe N -> [N])
parseNueveNAListaDeN' = parseNueveNAListaDeN'' <*> parseCongasN

parseNueveNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseNueveNAListaDeN'' = parseNueveNAListaDeN''' <*> parseCongasN

parseNueveNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseNueveNAListaDeN''' = parseNueveNAListaDeN'''' <*> parseCongasN

parseNueveNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseNueveNAListaDeN'''' = parseNueveNAListaDeN''''' <*> parseCongasN

parseNueveNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseNueveNAListaDeN''''' = parseNueveNAListaDeN'''''' <*> parseCongasN

parseNueveNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseNueveNAListaDeN'''''' = parseNueveNAListaDeN''''''' <*> parseCongasN

parseNueveNAListaDeN''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseNueveNAListaDeN''''''' = parseNueveNAListaDeN'''''''' <*> parseCongasN

parseNueveNAListaDeN'''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseNueveNAListaDeN'''''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 -> nueveNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9

nueveNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> Maybe N -> [N]
nueveNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 n9 = catMaybes [n1, n2, n3, n4, n5, n6, n7, n8, n9]

-- ("p" "t" "p" (t "a") ...)
parseOchoNAListaDeN :: H [N]
parseOchoNAListaDeN = parseOchoNAListaDeN' <*> parseCongasN

parseOchoNAListaDeN' :: H (Maybe N -> [N])
parseOchoNAListaDeN' = parseOchoNAListaDeN'' <*> parseCongasN

parseOchoNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseOchoNAListaDeN'' = parseOchoNAListaDeN''' <*> parseCongasN

parseOchoNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseOchoNAListaDeN''' = parseOchoNAListaDeN'''' <*> parseCongasN

parseOchoNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOchoNAListaDeN'''' = parseOchoNAListaDeN''''' <*> parseCongasN

parseOchoNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOchoNAListaDeN''''' = parseOchoNAListaDeN'''''' <*> parseCongasN

parseOchoNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOchoNAListaDeN'''''' = parseOchoNAListaDeN''''''' <*> parseCongasN

parseOchoNAListaDeN''''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseOchoNAListaDeN''''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 n8 -> ochoNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8

ochoNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N ->  Maybe N -> [N]
ochoNAListaDeN n1 n2 n3 n4 n5 n6 n7 n8 = catMaybes [n1, n2, n3, n4, n5, n6, n7, n8]

-- ("p" "t" "p" (t "a") ...)
parseSieteNAListaDeN :: H [N]
parseSieteNAListaDeN = parseSieteNAListaDeN' <*> parseCongasN

parseSieteNAListaDeN' :: H (Maybe N -> [N])
parseSieteNAListaDeN' = parseSieteNAListaDeN'' <*> parseCongasN

parseSieteNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseSieteNAListaDeN'' = parseSieteNAListaDeN''' <*> parseCongasN

parseSieteNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseSieteNAListaDeN''' = parseSieteNAListaDeN'''' <*> parseCongasN

parseSieteNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseSieteNAListaDeN'''' = parseSieteNAListaDeN''''' <*> parseCongasN

parseSieteNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseSieteNAListaDeN''''' = parseSieteNAListaDeN'''''' <*> parseCongasN

parseSieteNAListaDeN'''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseSieteNAListaDeN'''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 n7 -> sieteNAListaDeN n1 n2 n3 n4 n5 n6 n7

sieteNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N]
sieteNAListaDeN n1 n2 n3 n4 n5 n6 n7 = catMaybes [n1, n2, n3, n4, n5, n6, n7]

-- ("p" "t" "p" (t "a") ...)
parseSeisNAListaDeN :: H [N]
parseSeisNAListaDeN = parseSeisNAListaDeN' <*> parseCongasN

parseSeisNAListaDeN' :: H (Maybe N -> [N])
parseSeisNAListaDeN' = parseSeisNAListaDeN'' <*> parseCongasN

parseSeisNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseSeisNAListaDeN'' = parseSeisNAListaDeN''' <*> parseCongasN

parseSeisNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseSeisNAListaDeN''' = parseSeisNAListaDeN'''' <*> parseCongasN

parseSeisNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseSeisNAListaDeN'''' = parseSeisNAListaDeN''''' <*> parseCongasN

parseSeisNAListaDeN''''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseSeisNAListaDeN''''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 n6 -> seisNAListaDeN n1 n2 n3 n4 n5 n6

seisNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N]
seisNAListaDeN n1 n2 n3 n4 n5 n6 = catMaybes [n1, n2, n3, n4, n5, n6]

-- ("p" "t" "p" (t "a") ...)
parseCincoNAListaDeN :: H [N]
parseCincoNAListaDeN = parseCincoNAListaDeN' <*> parseCongasN

parseCincoNAListaDeN' :: H (Maybe N -> [N])
parseCincoNAListaDeN' = parseCincoNAListaDeN'' <*> parseCongasN

parseCincoNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseCincoNAListaDeN'' = parseCincoNAListaDeN''' <*> parseCongasN

parseCincoNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseCincoNAListaDeN''' = parseCincoNAListaDeN'''' <*> parseCongasN

parseCincoNAListaDeN'''' :: H (Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N])
parseCincoNAListaDeN'''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 n5 -> cincoNAListaDeN n1 n2 n3 n4 n5

cincoNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N -> Maybe N -> [N]
cincoNAListaDeN n1 n2 n3 n4 n5 = catMaybes [n1, n2, n3, n4, n5]


-- ("p" "t" "p" $ t "a")
parseCuatroNAListaDeN :: H [N]
parseCuatroNAListaDeN = parseCuatroNAListaDeN' <*> parseCongasN

parseCuatroNAListaDeN' :: H (Maybe N -> [N])
parseCuatroNAListaDeN' = parseCuatroNAListaDeN'' <*> parseCongasN

parseCuatroNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseCuatroNAListaDeN'' = parseCuatroNAListaDeN''' <*> parseCongasN

parseCuatroNAListaDeN''' :: H (Maybe N -> Maybe N -> Maybe N -> [N])
parseCuatroNAListaDeN''' = do
  n1 <- parseCongasN
  return $ \n2 n3 n4 -> cuatroNAListaDeN n1 n2 n3 n4

cuatroNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> Maybe N ->  [N]
cuatroNAListaDeN n1 n2 n3 n4 = catMaybes [n1, n2, n3, n4]

-- ("a" t "t" "a")
parseTresNAListaDeN :: H [N]
parseTresNAListaDeN = parseTresNAListaDeN' <*> parseCongasN

parseTresNAListaDeN' :: H (Maybe N -> [N])
parseTresNAListaDeN' = parseTresNAListaDeN'' <*> parseCongasN

parseTresNAListaDeN'' :: H (Maybe N -> Maybe N -> [N])
parseTresNAListaDeN'' = do
  n1 <- parseCongasN
  return $ \n2 n3 -> tresNAListaDeN n1 n2 n3

tresNAListaDeN :: Maybe N -> Maybe N -> Maybe N -> [N]
tresNAListaDeN n1 n2 n3 = catMaybes [n1, n2, n3]

-- ("a", t "a")
parseDosNAListaDeN :: H [N]
parseDosNAListaDeN = parseDosNAListaDeN' <*> parseCongasN

parseDosNAListaDeN' :: H (Maybe N -> [N])
parseDosNAListaDeN' = do
  n1 <- parseCongasN
  return $ \n2 -> dosNAListaDeN n1 n2

dosNAListaDeN :: Maybe N -> Maybe N -> [N]
dosNAListaDeN n1 n2 = catMaybes [n1, n2]

-- ("a")
parseUnNAListaDeN :: H [N]
parseUnNAListaDeN = do
  n <- parseCongasN
  return $ unNAListaDeN n

unNAListaDeN :: Maybe N -> [N]
unNAListaDeN n = catMaybes [n]

parseTumbao :: H Layer
parseTumbao = parseTumbao' <*> parseLayer

parseTumbao' :: H (Layer -> Layer) -- (tonicaYquinta cumbia) bajo
parseTumbao' = parseTumbao'' <*> int

parseTumbao'' :: H (Int -> Layer -> Layer) -- (tonicaYquinta cumbia) bajo
parseTumbao'' = tumbao <$ reserved "tumbao"

tumbao :: Int -> Layer -> Layer -- ?

tumbao 0 c = c {style = nuevoE}
  where nuevoE = (style c) {
  bassPitchPattern0 = bassPitchPattern0 (style c),
  bassRhythmPattern0 = bassRhythmPattern0 (style c),

  congasRhythmPattern0 = [(1, 0), (1, 0.25), (1, 0.5), (1, 0.75)],
  congasSampleNPattern0 = NPattern2 [("quinto", 0), ("quinto", 1), ("quinto", 0), ("quinto", 1)],
  congasPitchPattern0 = ("midinote", take 4 $ cycle [("mn", 60, 0)]),

  tecladoPitchPattern0 = tecladoPitchPattern2 (style c),
  tecladoRhythmPattern0 = tecladoRhythmPattern2 (style c)
  -- tecladoSampleNPattern0 = tecladoSampleNPattern2 (style c)
}

-- tonicaQuinta
tumbao 1 c = c {style = nuevoE}
  where nuevoE = (style c) {
                            bassPitchPattern0 =  ("intervalo", [(intervalo "unisono" 0), (intervalo "5a" 0)]), -- index from list of pitches i.e. [60, 67]
                            bassRhythmPattern0 = [(1, 0), (1, 0.5)],  --i.e. [♩ 𝄽  ♩ 𝄽 ],

                            congasRhythmPattern0 = [(1, 0), (1, 0.25), (1, 0.5), (1, 0.75)],
                            congasSampleNPattern0 = NPattern2 [("quinto", 2), ("quinto", 1), ("quinto", 0), ("quinto", 1)],
                            congasPitchPattern0 = ("midinote", take 4 $ cycle [("mn", 60, 0)]),

                            tecladoPitchPattern0 = tecladoPitchPattern3 (style c),
                            tecladoRhythmPattern0 = tecladoRhythmPattern3 (style c)
                            -- tecladoSampleNPattern0 = tecladoSampleNPattern3 (style c)
                            }
-- tonicaQuinta2
tumbao 2 c = c {style = nuevoE}
  where nuevoE = (style c) {
                            bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],
                            bassPitchPattern0 = ("intervalo", [intervalo "unisono" 0, intervalo "5a" 0, intervalo "5a" (-1)]), -- index from list of pitches i.e. [60, 64, 67]

                            congasRhythmPattern0 = [(1, 0), (1, 0.125),  (1, 0.25), (1, 0.5), (1, 0.75)],
                            congasSampleNPattern0 = NPattern2 [("quinto", 2), ("quinto", 2), ("quinto", 1), ("quinto", 0), ("quinto", 1)],
                            congasPitchPattern0 = ("midinote", take 5 $ cycle [("mn", 60, 0)])

                          }
-- tonicaAuintaOctava
tumbao 3 c = c {style = nuevoE}
  where nuevoE = (style c) {
                            bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],
                            bassPitchPattern0 = ("intervalo", [intervalo "unisono" 0, intervalo "5a" 0, intervalo "8a" 0]), -- index from list of pitches i.e. [60, 64, 67]

                            congasRhythmPattern0 = [(1, 0), (1, 0.125),  (1, 0.25), (1, 0.5), (1, 0.75)],
                            congasSampleNPattern0 = NPattern2 [("quinto", 0), ("quinto", 2), ("quinto", 1), ("quinto", 0), ("quinto", 1)],
                            congasPitchPattern0 = ("midinote", take 5 $ cycle [("mn", 60, 0)])

                          }
-- tonicaQuintaTercera
tumbao 4 c = c {style = nuevoE}
  where nuevoE = (style c) {
                            bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],
                            bassPitchPattern0 = ("intervalo", [intervalo "unisono" 0, intervalo "5a" 0, intervalo "3a" 0]), -- index from list of pitches i.e. [60, 64, 67]

                            congasRhythmPattern0 = [(1, 0), (1, 0.125),  (1, 0.25), (1, 0.5), (1, 0.625), (1, 0.75)],
                            congasSampleNPattern0 = NPattern2 [("quinto", 0), ("quinto", 2), ("quinto", 1), ("quinto", 0), ("tumba", 0), ("quinto", 1)],
                            congasPitchPattern0 = ("midinote", take 6 $ cycle [("mn", 60, 0)])
                          }

tumbao _ c = c

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
--

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
                            acordeonPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            zampoñaPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            tecladoPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            bassPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            efectoPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            altavozPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            guiraPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            tarolaPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            contrasPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            extrasPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            clavePitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            jamblockPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps),
                            congasPitchPattern0 = ("midinote", listDeNotasConRelacion "mn" ps)
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
                            acordeonPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            zampoñaPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            tecladoPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            bassPitchPattern0= ("midinote", [("mn", ps, 0)]),
                            efectoPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            altavozPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            tarolaPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            contrasPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            guiraPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            extrasPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            clavePitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            jamblockPitchPattern0 = ("midinote", [("mn", ps, 0)]),
                            congasPitchPattern0 = ("midinote", [("mn", ps, 0)])
                             }

-- provee una lista de intervalos con Double y una octava seleccionable, e.g. intervalo [0 1, "5a" 2]

parseCambiarIntervalosDoubleConOctava :: H Layer
parseCambiarIntervalosDoubleConOctava = parseCambiarIntervalosDoubleConOctava' <*> parseLayer

parseCambiarIntervalosDoubleConOctava' :: H (Layer -> Layer)
parseCambiarIntervalosDoubleConOctava' = parseCambiarIntervalosDoubleConOctava'' <*> listofTupleofdouble

parseCambiarIntervalosDoubleConOctava'' :: H ([(Double, Double)] -> Layer -> Layer)
parseCambiarIntervalosDoubleConOctava'' = cambiarIntervalosDoubleConOctava <$ reserved "intervalo"

listofTupleofdouble :: H [(Double, Double)]
listofTupleofdouble = list parseTupleofdouble

parseTupleofdouble :: H (Double, Double)
parseTupleofdouble = parseTupleofdouble' <*> double

parseTupleofdouble' :: H (Double -> (Double, Double))
parseTupleofdouble' = do
  i <- double
  return $ \o -> tupleofdouble i o

tupleofdouble :: Double -> Double -> (Double, Double)
tupleofdouble index octava = (index, octava)

cambiarIntervalosDoubleConOctava :: [(Double, Double)] -> Layer -> Layer
cambiarIntervalosDoubleConOctava is c = c {style = nuevoE}
  where
    is' = fmap (\(index, octava) -> intervaloDouble index octava) is
    nuevoE = (style c) {
                              cuerdaPitchPattern0 = ("intervalo", is'),
                              acordeonPitchPattern0 = ("intervalo", is'),
                              zampoñaPitchPattern0 = ("intervalo", is'),
                              tecladoPitchPattern0 = ("intervalo", is'),
                              bassPitchPattern0= ("intervalo", is'),
                              efectoPitchPattern0 = ("intervalo", is'),
                              altavozPitchPattern0 = ("intervalo", is'),
                              tarolaPitchPattern0 = ("intervalo", is'),
                              guiraPitchPattern0 = ("intervalo", is'),
                              contrasPitchPattern0 = ("intervalo", is'),
                              extrasPitchPattern0 = ("intervalo", is'),
                              congasPitchPattern0 = ("intervalo", is'),
                              jamblockPitchPattern0 = ("intervalo", is'),
                              clavePitchPattern0 = ("intervalo", is')
                              }
--
parseCambiarIntervaloDoubleConOctava :: H Layer
parseCambiarIntervaloDoubleConOctava = parseCambiarIntervaloDoubleConOctava' <*> parseLayer

parseCambiarIntervaloDoubleConOctava' :: H (Layer -> Layer)
parseCambiarIntervaloDoubleConOctava' = parseCambiarIntervaloDoubleConOctava'' <*> double

parseCambiarIntervaloDoubleConOctava'' :: H (Double -> Layer -> Layer)
parseCambiarIntervaloDoubleConOctava'' = parseCambiarIntervaloDoubleConOctava''' <*> double

parseCambiarIntervaloDoubleConOctava''' :: H (Double -> Double -> Layer -> Layer)
parseCambiarIntervaloDoubleConOctava''' = cambiarIntervaloDoubleConOctava <$ reserved "intervalo"

cambiarIntervaloDoubleConOctava :: Double -> Double -> Layer -> Layer
cambiarIntervaloDoubleConOctava index octava c = c {style = nuevoE}
  where
    nuevoE = (style c) {
                            cuerdaPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            acordeonPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            zampoñaPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            tecladoPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            bassPitchPattern0= ("intervalo", [intervaloDouble index octava]),
                            efectoPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            altavozPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            tarolaPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            guiraPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            contrasPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            extrasPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            congasPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            jamblockPitchPattern0 = ("intervalo", [intervaloDouble index octava]),
                            clavePitchPattern0 = ("intervalo", [intervaloDouble index octava])

                            }
--
parseCambiarIntervalosDouble :: H Layer
parseCambiarIntervalosDouble = parseCambiarIntervalosDouble' <*> parseLayer

parseCambiarIntervalosDouble' :: H (Layer -> Layer)
parseCambiarIntervalosDouble' = parseCambiarIntervalosDouble'' <*> doubleList

parseCambiarIntervalosDouble'' :: H ([Double] -> Layer -> Layer)
parseCambiarIntervalosDouble'' = cambiarIntervalosDouble <$ reserved "intervalo"

cambiarIntervalosDouble :: [Double] -> Layer -> Layer
cambiarIntervalosDouble indices c = c {style = nuevoE}
  where
    indices' = fmap (\index -> intervaloDouble index 0) indices
    nuevoE = (style c) {
                              cuerdaPitchPattern0 = ("intervalo", indices'),
                              acordeonPitchPattern0 = ("intervalo", indices'),
                              zampoñaPitchPattern0 = ("intervalo", indices'),
                              tecladoPitchPattern0 = ("intervalo", indices'),
                              bassPitchPattern0= ("intervalo", indices'),
                              efectoPitchPattern0 = ("intervalo", indices'),
                              altavozPitchPattern0 = ("intervalo", indices'),
                              guiraPitchPattern0 = ("intervalo", indices'),
                              contrasPitchPattern0 = ("intervalo", indices'),
                              tarolaPitchPattern0 = ("intervalo", indices'),
                              extrasPitchPattern0 = ("intervalo", indices'),
                              clavePitchPattern0 = ("intervalo", indices'),
                              jamblockPitchPattern0 = ("intervalo", indices'),
                              congasPitchPattern0 = ("intervalo", indices')
                              }

parseCambiarIntervaloDouble :: H Layer
parseCambiarIntervaloDouble = parseCambiarIntervaloDouble' <*> parseLayer

parseCambiarIntervaloDouble' :: H (Layer -> Layer)
parseCambiarIntervaloDouble' = parseCambiarIntervaloDouble'' <*> double

parseCambiarIntervaloDouble'' :: H (Double -> Layer -> Layer)
parseCambiarIntervaloDouble'' = cambiarIntervaloDouble <$ reserved "intervalo"

cambiarIntervaloDouble :: Double -> Layer -> Layer
cambiarIntervaloDouble index c = c {style = nuevoE}
  where
    nuevoE = (style c) {
                            cuerdaPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            acordeonPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            tecladoPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            zampoñaPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            bassPitchPattern0= ("intervalo", [intervaloDouble index 0]),
                            efectoPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            tarolaPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            guiraPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            contrasPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            altavozPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            extrasPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            congasPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            jamblockPitchPattern0 = ("intervalo", [intervaloDouble index 0]),
                            clavePitchPattern0 = ("intervalo", [intervaloDouble index 0])
                            }
-- provee una lista de intervalos con una octava seleccionable, e.g. intervalo ["3a" 1, "5a" 2]

parseCambiarIntervalosConOctava :: H Layer
parseCambiarIntervalosConOctava = parseCambiarIntervalosConOctava' <*> parseLayer

parseCambiarIntervalosConOctava' :: H (Layer -> Layer)
parseCambiarIntervalosConOctava' = parseCambiarIntervalosConOctava'' <*> listofTupleofStringDouble

parseCambiarIntervalosConOctava'' :: H ([Index] -> Layer -> Layer)
parseCambiarIntervalosConOctava'' = cambiarIntervalosConOctava <$ reserved "intervalo"

data Index = IndexOctavaDef String | IndexConOctava {indice :: String, octava :: Double}
-- --
-- listOfIndex :: H [Index]
-- listOfIndex = list parseTupleofStringDouble

listofTupleofStringDouble :: H [Index]
listofTupleofStringDouble = list parseTupleofStringDouble

parseTupleofStringDouble :: H Index
parseTupleofStringDouble = parseTupleofStringDouble' <*> double

parseTupleofStringDouble' :: H (Double -> Index)
parseTupleofStringDouble' = do
  s <- string
  return $ \o -> tupleofStringDouble s o

tupleofStringDouble :: String -> Double -> Index --(String, Double)
tupleofStringDouble index octava = IndexConOctava index octava

cambiarIntervalosConOctava :: [Index] -> Layer -> Layer
cambiarIntervalosConOctava is c = c {style = nuevoE}
  where
    is' = fmap (\x -> intervalo (indice x) (octava x)) is
    nuevoE = (style c) {
                              cuerdaPitchPattern0 = ("intervalo", is'),
                              acordeonPitchPattern0 = ("intervalo", is'),
                              zampoñaPitchPattern0 = ("intervalo", is'),
                              tecladoPitchPattern0 = ("intervalo", is'),
                              bassPitchPattern0= ("intervalo", is'),
                              efectoPitchPattern0 = ("intervalo", is'),
                              altavozPitchPattern0 = ("intervalo", is'),
                              guiraPitchPattern0 = ("intervalo", is'),
                              tarolaPitchPattern0 = ("intervalo", is'),
                              contrasPitchPattern0 = ("intervalo", is'),
                              extrasPitchPattern0 = ("intervalo", is'),
                              congasPitchPattern0 = ("intervalo", is'),
                              jamblockPitchPattern0 = ("intervalo", is'),
                              clavePitchPattern0 = ("intervalo", is')
                              }

-- provee un intervalo con una octava seleccionable, e.g. intervalo "3a" 1
parseCambiarIntervaloConOctava :: H Layer
parseCambiarIntervaloConOctava = parseCambiarIntervaloConOctava' <*> parseLayer

parseCambiarIntervaloConOctava' :: H (Layer -> Layer)
parseCambiarIntervaloConOctava' = parseCambiarIntervaloConOctava'' <*> double

parseCambiarIntervaloConOctava'' :: H (Double -> Layer -> Layer)
parseCambiarIntervaloConOctava'' = parseCambiarIntervaloConOctava''' <*> string

parseCambiarIntervaloConOctava''' :: H (String -> Double -> Layer -> Layer)
parseCambiarIntervaloConOctava''' = cambiarIntervaloConOctava <$ reserved "intervalo"

cambiarIntervaloConOctava :: String -> Double -> Layer -> Layer
cambiarIntervaloConOctava index octava c = c {style = nuevoE}
  where
    nuevoE = (style c) {
                            cuerdaPitchPattern0 = ("intervalo", [intervalo index octava]),
                            acordeonPitchPattern0 = ("intervalo", [intervalo index octava]),
                            zampoñaPitchPattern0 = ("intervalo", [intervalo index octava]),
                            tecladoPitchPattern0 = ("intervalo", [intervalo index octava]),
                            bassPitchPattern0= ("intervalo", [intervalo index octava]),
                            efectoPitchPattern0 = ("intervalo", [intervalo index octava]),
                            tarolaPitchPattern0 = ("intervalo", [intervalo index octava]),
                            guiraPitchPattern0 = ("intervalo", [intervalo index octava]),
                            contrasPitchPattern0 = ("intervalo", [intervalo index octava]),
                            altavozPitchPattern0 = ("intervalo", [intervalo index octava]),
                            extrasPitchPattern0 = ("intervalo", [intervalo index octava]),
                            congasPitchPattern0 = ("intervalo", [intervalo index octava]),
                            jamblockPitchPattern0 = ("intervalo", [intervalo index octava]),
                            clavePitchPattern0 = ("intervalo", [intervalo index octava])
                            }

-- provee los intervalos de una lista
-- ("intervalo", [intervalo "unisono" 0, intervalo "3a" 0, intervalo "5a" 0])
parseCambiarIntervalos :: H Layer
parseCambiarIntervalos = parseCambiarIntervalos' <*> parseLayer

parseCambiarIntervalos' :: H (Layer -> Layer)
parseCambiarIntervalos' = parseCambiarIntervalos'' <*> stringList

parseCambiarIntervalos'' :: H ([String] -> Layer -> Layer)
parseCambiarIntervalos'' = cambiarIntervalos <$ reserved "intervalo"

cambiarIntervalos :: [String] -> Layer -> Layer
cambiarIntervalos indices c = c {style = nuevoE}
  where
    indices' = fmap (\index -> intervalo index 0) indices
    nuevoE = (style c) {
                              cuerdaPitchPattern0 = ("intervalo", indices'),
                              acordeonPitchPattern0 = ("intervalo", indices'),
                              zampoñaPitchPattern0 = ("intervalo", indices'),
                              tecladoPitchPattern0 = ("intervalo", indices'),
                              bassPitchPattern0= ("intervalo", indices'),
                              efectoPitchPattern0 = ("intervalo", indices'),
                              tarolaPitchPattern0 = ("intervalo", indices'),
                              guiraPitchPattern0 = ("intervalo", indices'),
                              contrasPitchPattern0 = ("intervalo", indices'),
                              altavozPitchPattern0 = ("intervalo", indices'),
                              extrasPitchPattern0 = ("intervalo", indices'),
                              congasPitchPattern0 = ("intervalo", indices'),
                              jamblockPitchPattern0 = ("intervalo", indices'),
                              clavePitchPattern0 = ("intervalo", indices')
                              }
-- provee el intervalo con respecto a la tonica y cualidad del acorde
parseCambiarIntervalo :: H Layer
parseCambiarIntervalo = parseCambiarIntervalo' <*> parseLayer

parseCambiarIntervalo' :: H (Layer -> Layer)
parseCambiarIntervalo' = parseCambiarIntervalo'' <*> string

parseCambiarIntervalo'' :: H (String -> Layer -> Layer)
parseCambiarIntervalo'' = cambiarIntervalo <$ reserved "intervalo"

cambiarIntervalo :: String -> Layer -> Layer
cambiarIntervalo index c = c {style = nuevoE}
  where nuevoE = (style c) {
                            cuerdaPitchPattern0 = ("intervalo", [intervalo index 0]),
                            acordeonPitchPattern0 = ("intervalo", [intervalo index 0]),
                            zampoñaPitchPattern0 = ("intervalo", [intervalo index 0]),
                            tecladoPitchPattern0 = ("intervalo", [intervalo index 0]),
                            bassPitchPattern0= ("intervalo", [intervalo index 0]),
                            efectoPitchPattern0 = ("intervalo", [intervalo index 0]),
                            altavozPitchPattern0 = ("intervalo", [intervalo index 0]),
                            guiraPitchPattern0 = ("intervalo", [intervalo index 0]),
                            contrasPitchPattern0 = ("intervalo", [intervalo index 0]),
                            tarolaPitchPattern0 = ("intervalo", [intervalo index 0]),
                            extrasPitchPattern0 = ("intervalo", [intervalo index 0]),
                            congasPitchPattern0 = ("intervalo", [intervalo index 0]),
                            jamblockPitchPattern0 = ("intervalo", [intervalo index 0]),
                            clavePitchPattern0 = ("intervalo", [intervalo index 0])
                            }
-- ritmo [1 2, 1 2 3 4]
parseCambiarRitmosAuto :: H Layer
parseCambiarRitmosAuto =  parseCambiarRitmosAuto' <*> parseLayer

parseCambiarRitmosAuto' :: H (Layer -> Layer)
parseCambiarRitmosAuto' =  parseCambiarRitmosAuto'' <*> parseListasDeListasDeAtaques -- rationalList

parseCambiarRitmosAuto'' :: H ([[Rational]] -> Layer -> Layer)
parseCambiarRitmosAuto'' = cambiarRitmosAuto <$ reserved "ritmo"

cambiarRitmosAuto :: [[Rational]] -> Layer -> Layer
cambiarRitmosAuto [x] c = c {style = nuevoE}
  where
    -- [x'] = concat [x]
    -- attack' = (x' - 1) / 4 -- metre? [1, 2, 3, 4]  => 1 [0, 0.25, 0.5, 0.75]
    -- metre |x' <= 0 = 1
          -- |otherwise = (realToFrac $ floor attack') + 1
    metre = toRational $ length [x] -- [[Nothing], [1, 2, 3]] = metre 2 -- (realToFrac $ floor rs') + 1
    rPat = cambiarRitmo'''' metre [x]
    nuevoE = (style c) {
                            cuerdaRhythmPattern0 = rPat,
                            acordeonRhythmPattern0 = rPat,
                            zampoñaRhythmPattern0 = rPat,
                            tecladoRhythmPattern0 = rPat,
                            bassRhythmPattern0 = rPat,
                            guiraRhythmPattern0 = rPat,
                            contrasRhythmPattern0 = rPat,
                            tarolaRhythmPattern0 = rPat,
                            efectoRhythmPattern0 = rPat,
                            altavozRhythmPattern0 = rPat,
                            extrasRhythmPattern0 = rPat,
                            claveRhythmPattern0 = rPat,
                            jamblockRhythmPattern0 = rPat,
                            congasRhythmPattern0 = rPat
                            }

cambiarRitmosAuto attacks c = c {style = nuevoE}
  where
    -- attacks' = concat attacks
    -- attack' = ((maximum attacks') - 1) / 4 -- metre? [1, 2, 3, 4]  => 1 [0, 0.25, 0.5, 0.75]
    -- metre = (realToFrac $ floor attack') + 1
    metre = toRational $ length attacks -- [[Nothing], [1, 2, 3]] = metre 2 -- (realToFrac $ floor rs') + 1
    rPat = cambiarRitmo'''' metre attacks

    nuevoE = (style c) {
                            cuerdaRhythmPattern0 = rPat,
                            acordeonRhythmPattern0 = rPat,
                            zampoñaRhythmPattern0 = rPat,
                            tecladoRhythmPattern0 = rPat,
                            bassRhythmPattern0 = rPat,
                            guiraRhythmPattern0 = rPat,
                            contrasRhythmPattern0 = rPat,
                            tarolaRhythmPattern0 = rPat,
                            efectoRhythmPattern0 = rPat,
                            altavozRhythmPattern0 = rPat,
                            extrasRhythmPattern0 = rPat,
                            claveRhythmPattern0 = rPat,
                            jamblockRhythmPattern0 = rPat,
                            congasRhythmPattern0 = rPat
                            }

-- ritmo 4  cumbia cuerda, ritmo 1
parseCambiarRitmoAuto :: H Layer
parseCambiarRitmoAuto =  parseCambiarRitmoAuto' <*> parseLayer

parseCambiarRitmoAuto' :: H (Layer -> Layer)
parseCambiarRitmoAuto' = parseCambiarRitmoAuto'' <*> rationalOrInteger

parseCambiarRitmoAuto'' :: H (Rational -> Layer -> Layer)
parseCambiarRitmoAuto'' = cambiarRitmoMetreAuto <$ reserved "ritmo"

cambiarRitmoMetreAuto :: Rational -> Layer -> Layer
cambiarRitmoMetreAuto attack c = c {style = nuevoE}
  where
    attack' = (attack - 1) / 4
    metre |attack <= 0 = 1 -- avoid metre of 0
          |otherwise = (realToFrac $ floor attack') + 1 -- metre? [1, 2, 3, 4]  => 1 [0, 0.25, 0.5, 0.75]

    nuevoE = (style c) {
                            cuerdaRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            acordeonRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            zampoñaRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            tecladoRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            bassRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            guiraRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            contrasRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            tarolaRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            efectoRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            altavozRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            extrasRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            congasRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            jamblockRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            claveRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)]
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
cambiarRitmo metre attack c = c {style = nuevoE}
  where nuevoE = (style c) {
                            cuerdaRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            acordeonRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            zampoñaRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            tecladoRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            bassRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            guiraRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            contrasRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            tarolaRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            efectoRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            altavozRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            extrasRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            congasRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            jamblockRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)],
                            claveRhythmPattern0 = catMaybes [cambiarRitmo' (metre) (attack)]
                            }

--e.g. ritmo 1 [0.125, 0.25] cumbia cuerda, deberia ser ritmo 1 [1 1.25 1.5 1.75], ritmo 2 [1 2]
parseCambiarRitmos :: H Layer
parseCambiarRitmos =  parseCambiarRitmos' <*> parseLayer

parseCambiarRitmos' :: H (Layer -> Layer)
parseCambiarRitmos' =  parseCambiarRitmos'' <*> parseListasDeListasDeAtaques -- rationalList

parseCambiarRitmos'' :: H ([[Rational]] -> Layer -> Layer)
parseCambiarRitmos'' = parseCambiarRitmos''' <*> rationalOrInteger

parseCambiarRitmos''' :: H (Rational -> [[Rational]] -> Layer -> Layer)
parseCambiarRitmos''' = cambiarRitmos <$ reserved "ritmo"

cambiarRitmos :: Rational -> [[Rational]] -> Layer -> Layer
cambiarRitmos metre rs c = c {style = nuevoE}
  where
    -- attacks' = concat attacks
    nuevoE = (style c) {
                            cuerdaRhythmPattern0 = cambiarRitmo'''' metre rs,
                            acordeonRhythmPattern0 = cambiarRitmo'''' metre rs,
                            zampoñaRhythmPattern0 = cambiarRitmo'''' metre rs,
                            tecladoRhythmPattern0 = cambiarRitmo'''' metre rs,
                            bassRhythmPattern0 = cambiarRitmo'''' metre rs,
                            guiraRhythmPattern0 = cambiarRitmo'''' metre rs,
                            contrasRhythmPattern0 = cambiarRitmo'''' metre rs,
                            tarolaRhythmPattern0 = cambiarRitmo'''' metre rs,
                            efectoRhythmPattern0 = cambiarRitmo'''' metre rs,
                            altavozRhythmPattern0 = cambiarRitmo'''' metre rs,
                            extrasRhythmPattern0 = cambiarRitmo'''' metre rs,
                            claveRhythmPattern0 = cambiarRitmo'''' metre rs,
                            jamblockRhythmPattern0 = cambiarRitmo'''' metre rs,
                            congasRhythmPattern0 = cambiarRitmo'''' metre rs
                            }

-- a function to change the attacks
cambiarRitmo' :: Rational -> Rational -> Maybe (Rational, Rational)
cambiarRitmo' metre attack = metreAndAttack
  where
    cuartosPorCompas = 4 -- * metre
    metreAndAttack | (attack >= 1) && (attack < (cuartosPorCompas + 1)) = Just (metre, attack') -- e.g. ritmo 1 [1 2 3 4] => ritmo 1 [0, 0.25, 0.5, 0.75], 2 [1 2 3 4, 5 6 7 8] => ritmo 2 [0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75]
                   | otherwise = Nothing
                   where attack' = (attack - 1) / 4
                   -- where attack' = ((attack - 1) / 4) + indice

cambiarRitmo'' :: Rational -> [Rational] -> [(Rational, Rational)]
cambiarRitmo'' metre  attacks = catMaybes $ fmap (cambiarRitmo' metre) attacks


cambiarRitmoSinMetre' :: Rational -> Rational -> Maybe Rational
cambiarRitmoSinMetre' metre attack = metreAndAttack
  where
    cuartosPorCompas = 4 -- * metre
    metreAndAttack | (attack >= 1) && (attack < (cuartosPorCompas + 1)) = Just attack' -- e.g. ritmo 1 [1 2 3 4] => ritmo 1 [0, 0.25, 0.5, 0.75], 2 [1 2 3 4, 5 6 7 8] => ritmo 2 [0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75]
                   | otherwise = Nothing
                   where attack' = (attack - 1) / 4

cambiarRitmoSinMetre''' :: Rational -> [Rational] -> [Maybe Rational]
cambiarRitmoSinMetre''' metre  attacks = fmap (cambiarRitmoSinMetre' metre) attacks

-- nota: falta dividir en /4 y luego sumar, ahora suma primero y divide despues pero no es lo mismo
cambiarRitmo'''' :: Rational -> [[Rational]] -> [(Rational, Rational)] -- [(metre, attack)]
cambiarRitmo'''' metre attacks = do
  let dividirAttacks = fmap (cambiarRitmoSinMetre''' metre) attacks -- [[Maybe Rational]]
  let zipIAttacks = zip [toRational 0 .. (metre - 1)] dividirAttacks -- [(0, [Just 1, Just 2.. ])), ...)]
  let sumarIaAttacks = fmap (\(i, xs) -> fmap (\x ->  (+) <$> x <*> Just i) xs) zipIAttacks -- [[1,2, 3], [4, 5, 6]]
  let attacks' = catMaybes $ concat sumarIaAttacks
  fmap (\attack -> (metre, attack)) attacks'

-- cambia el gain
parseCambiarGain :: H Layer
parseCambiarGain = parseCambiarGain' <*> parseLayer

parseCambiarGain' :: H (Layer -> Layer)
parseCambiarGain' = parseCambiarGain'' <*> double

parseCambiarGain'':: H (Double -> Layer -> Layer)
parseCambiarGain'' = cambiarGain <$ reserved "vol"

cambiarGain :: Double -> Layer -> Layer
cambiarGain gain c = c {style = nuevoE}
  where nuevoE = (style c) {
                            cuerdaGainPattern0 = gain,
                            acordeonGainPattern0 = gain,
                            zampoñaGainPattern0 = gain,
                            tecladoGainPattern0 = gain,
                            bassGainPattern0 = gain,
                            guiraGainPattern0 = gain,
                            contrasGainPattern0 = gain,
                            tarolaGainPattern0 = gain,
                            efectoGainPattern0 = gain,
                            altavozGainPattern0 = gain,
                            extrasGainPattern0 = gain,
                            congasGainPattern0 = gain,
                            jamblockGainPattern0 = gain,
                            claveGainPattern0 = gain
                            }

--cambia el paneo
parseCambiarPaneo :: H Layer
parseCambiarPaneo = parseCambiarPaneo' <*> parseLayer

parseCambiarPaneo' :: H (Layer -> Layer)
parseCambiarPaneo' = parseCambiarPaneo'' <*> double

parseCambiarPaneo'':: H (Double -> Layer -> Layer)
parseCambiarPaneo'' = cambiarPaneo <$ reserved "pan"

cambiarPaneo :: Double -> Layer -> Layer
cambiarPaneo pan c = c {style = nuevoE}
  where nuevoE = (style c) {
                            cuerdaPanPattern0 = pan,
                            acordeonPanPattern0 = pan,
                            zampoñaPanPattern0 = pan,
                            tecladoPanPattern0 = pan,
                            bassPanPattern0 = pan,
                            guiraPanPattern0 = pan,
                            contrasPanPattern0 = pan,
                            tarolaPanPattern0 = pan,
                            efectoPanPattern0 = pan,
                            altavozPanPattern0 = pan,
                            extrasPanPattern0 = pan,
                            congasPanPattern0 = pan,
                            jamblockPanPattern0 = pan,
                            clavePanPattern0 = pan
                            }

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
                            tecladoRhythmPattern0 = tecladoRhythmPattern0 (style c), -- ie.  [𝄽  𝄽  𝄽  ♩],
                            tecladoSampleNPattern0 = tecladoSampleNPattern0 (style c),
                            tecladoPitchPattern0 = tecladoPitchPattern0 (style c),

                            cuerdaRhythmPattern0 = cuerdaRhythmPattern0 (style c),
                            cuerdaSampleNPattern0 = cuerdaSampleNPattern0 (style c),
                            cuerdaPitchPattern0 = cuerdaPitchPattern0 (style c), -- or double? (nota [0, 2, 3]

                            acordeonRhythmPattern0 = acordeonRhythmPattern0 (style c),
                            acordeonSampleNPattern0 = acordeonSampleNPattern0 (style c),
                            acordeonPitchPattern0 = acordeonPitchPattern0 (style c), -- or double? (nota [0, 2, 3] cumbia) cuerda

                            zampoñaRhythmPattern0 = zampoñaRhythmPattern0 (style c),
                            zampoñaSampleNPattern0 = zampoñaSampleNPattern0 (style c),
                            zampoñaPitchPattern0 = zampoñaPitchPattern0 (style c), -- or double? (nota [0, 2, 3]

                            bassRhythmPattern0 = bassRhythmPattern0 (style c),  --i.e. [♩ 𝄽  ♩ ♩],
                            bassSampleNPattern0 = bassSampleNPattern0 (style c),
                            bassPitchPattern0 = bassPitchPattern0 (style c), -- index from list of pitches i.e. [60, 64, 67]

                            guiraRhythmPattern0 = guiraRhythmPattern0 (style c), --i.e. [♪♫ ♪♫ ♪♫ ♪♫]
                            guiraSampleNPattern0 = guiraSampleNPattern0 (style c),
                            guiraPitchPattern0 = guiraPitchPattern0 (style c),


                            contrasRhythmPattern0 = contrasRhythmPattern0 (style c),
                            contrasSampleNPattern0 = contrasSampleNPattern0 (style c),
                            contrasPitchPattern0 = contrasPitchPattern0 (style c),


                            tarolaRhythmPattern0 = tarolaRhythmPattern0 (style c),
                            tarolaSampleNPattern0 =tarolaSampleNPattern0 (style c),
                            tarolaPitchPattern0 = tarolaPitchPattern0 (style c),

                            efectoRhythmPattern0 = efectoRhythmPattern0 (style c),
                            efectoSampleNPattern0 = efectoSampleNPattern0 (style c),
                            efectoPitchPattern0 = efectoPitchPattern0 (style c),

                            extrasRhythmPattern0 = extrasRhythmPattern0 (style c),
                            extrasSampleNPattern0 = extrasSampleNPattern0 (style c),
                            extrasPitchPattern0 = extrasPitchPattern0 (style c),

                            congasRhythmPattern0 = congasRhythmPattern0 (style c),
                            congasSampleNPattern0 = congasSampleNPattern0 (style c),
                            congasPitchPattern0 = congasPitchPattern0 (style c),

                            claveRhythmPattern0 = claveRhythmPattern0 (style c),
                            claveSampleNPattern0 = claveSampleNPattern0 (style c),
                            clavePitchPattern0 = clavePitchPattern0 (style c)

                          }

preset 1 c = c {style = nuevoE}
  where nuevoE = (style c) {
                            tecladoRhythmPattern0 = tecladoRhythmPattern1 (style c), -- ie. [𝄽 ♩ 𝄽 ♩],
                            tecladoSampleNPattern0 = tecladoSampleNPattern1 (style c),

                            bassRhythmPattern0 = bassRhythmPattern1 (style c),  --i.e. [♩ 𝄽  ♩ 𝄽 ],
                            bassSampleNPattern0 = tecladoSampleNPattern0 (style c),
                            bassPitchPattern0 = bassPitchPattern1 (style c),

                            claveRhythmPattern0 = claveRhythmPattern1 (style c),
                            claveSampleNPattern0 = claveSampleNPattern1 (style c),
                            clavePitchPattern0 = clavePitchPattern1 (style c)
                          }

preset 2 c = c {style = nuevoE}
   where nuevoE = (style c) {
                            bassRhythmPattern0 = bassRhythmPattern2 (style c),  --i.e. [♩ 𝄽  ♩ 𝄽 ],
                            bassSampleNPattern0 = tecladoSampleNPattern0 (style c),
                            bassPitchPattern0 = bassPitchPattern2 (style c),

                            claveRhythmPattern0 = claveRhythmPattern2 (style c),
                            claveSampleNPattern0 = claveSampleNPattern2 (style c),
                            clavePitchPattern0 = clavePitchPattern2 (style c)
                            }
preset _ c = preset 0 c

-- funcion que modifica los acordes del teclado -- acompanamiento 2 =>  no más de 4.99
parseacompanamiento :: H Layer
parseacompanamiento = parseacompanamiento' <*> parseLayer

parseacompanamiento' :: H (Layer -> Layer)
parseacompanamiento' = parseacompanamiento'' <*> rationalOrInteger

parseacompanamiento'' :: H (Rational -> Layer -> Layer)
parseacompanamiento'' = acompanamiento <$ (reserved "acompañamiento" <|> reserved "acompanamiento")

acompanamiento :: Rational -> Layer -> Layer
acompanamiento attack c = c {style = nuevoE}
  where
    -- n' | n == 0 = 0
       -- |otherwise = abs $ n - 1
    attack' = (attack - 1) / 4
    metre = (realToFrac $ floor attack') + 1 -- metre? [1, 2, 3, 4]  => 1 [0, 0.25, 0.5, 0.75]
    rPat = catMaybes [cambiarRitmo' metre attack] -- fmap (\n -> (metre, (realToFrac n) /4)) ns'
    nuevoE = (style c) {
                            tecladoRhythmPattern0 = rPat, -- [(1, (realToFrac n') / 4)],
                            tecladoSampleNPattern0 = tecladoSampleNPattern0 (style c),
                            tecladoPitchPattern0 = tecladoPitchPattern0 (style c) -- ("acorde", [note])
                          }

-- acompanamientoTest :: Double -> Layer -> Layer
acompanamientoTest n notes = do

  let  n' | n == 0 = 0
          |otherwise = abs $ n - 1
  let tecladoRhythmPattern = [(1, (realToFrac n') / 4)]
  let tecladoSampleNPattern = [0]
  let tecladoPitchPattern = notes
  (show tecladoRhythmPattern, show tecladoSampleNPattern, show tecladoPitchPattern)


-- funcion que modifica los acordes del teclado -- acompanamiento (2 4)

parseacompanamientos :: H Layer
parseacompanamientos = parseacompanamientos' <*> parseLayer

parseacompanamientos' :: H (Layer -> Layer)
parseacompanamientos' = parseacompanamientos'' <*> parseAtaquesAListaDeAtaques -- rationalList

parseacompanamientos'' :: H ([Rational] -> Layer -> Layer)
parseacompanamientos'' = acompanamientos <$ (reserved "acompañamiento" <|> reserved "acompanamiento")

acompanamientos :: [Rational] -> Layer -> Layer
acompanamientos ns c = c {style = nuevoE}
  where
    -- ns' = fmap (\n -> if (n == 0) then 0 else (abs $ n - 1)) ns -- [1, 2, 3, 4] a [0, 1, 2, 3]
    -- rs' = ((maximum ns') - 1) / 4 -- metre? [1, 2, 3, 4]  => 1 [0, 0.25, 0.5, 0.75]
    ns' = ((maximum ns) - 1) / 4 -- metre? [1, 2, 3, 4]  => 1 [0, 0.25, 0.5, 0.75]
    metre = (realToFrac $ floor ns') + 1
    -- metre = 1
    rPat = cambiarRitmo'' metre ns -- fmap (\n -> (metre, (realToFrac n) /4)) ns'
    nPat (NPattern1 xs) = NPattern1 $ concat $ replicate (length ns) xs
    nuevoE = (style c) {
                            tecladoRhythmPattern0 = rPat, -- listaDeStringsARhythmicPattern rPat notes,
                            tecladoSampleNPattern0 = nPat $ tecladoSampleNPattern0 (style c), -- listaDeStringsANPattern nPat notes,
                            tecladoPitchPattern0 = tecladoPitchPattern0 (style c) -- ("acorde", concat $ notes) -- (PitchType, [Note])
                          }

acompanamientosTest :: [Double] -> [Note] -> (String, String, String)
acompanamientosTest ns notes = do
  let ns' = fmap (\n -> if (n == 0) then 0 else (abs $ n - 1)) ns -- [1, 2, 3, 4] a [0, 1, 2, 3]
  let metre = 1
  let notes' = replicate (length ns)  [intervalo "unisono" 0, intervalo "3a" 0, intervalo "5a" 0] -- [[Note]]
  let rPat = fmap (\n -> (metre, (realToFrac n) /4)) ns'
  let nPat = NPattern1 $ concat $ fmap (\x -> replicate (length ns) x) [0]
  let tecladoRhythmPattern =  listaDeStringsARhythmicPattern rPat notes'
  let tecladoSampleNPattern = listaDeStringsANPattern nPat notes'
  let tecladoPitchPattern = ("acorde", concat $ notes')
  (show tecladoRhythmPattern, show tecladoSampleNPattern, show tecladoPitchPattern)

-- funcion que modifica los acordes del teclado -- acompanamiento 2 ("f" "3a" "5a") o acompanamiento 2 $ "f" ("3a" (-1)) "5a"
parseAcompanamientoConVoicingSel :: H Layer
parseAcompanamientoConVoicingSel = parseAcompanamientoConVoicingSel' <*> parseLayer

parseAcompanamientoConVoicingSel' :: H (Layer -> Layer)
parseAcompanamientoConVoicingSel' = parseAcompanamientoConVoicingSel'' <*> parseStringsAListaDeNotes --

parseAcompanamientoConVoicingSel'' :: H ([Note] -> Layer -> Layer)
parseAcompanamientoConVoicingSel'' = parseAcompanamientoConVoicingSel''' <*> rationalOrInteger

parseAcompanamientoConVoicingSel''' :: H (Rational -> [Note] -> Layer -> Layer)
parseAcompanamientoConVoicingSel''' = acompanamientoConVoicingSel <$ (reserved "acompañamiento" <|> reserved "acompanamiento")

acompanamientoConVoicingSel :: Rational -> [Note] -> Layer -> Layer
acompanamientoConVoicingSel attack notes c = c {style = nuevoE}
  where
    attack' = (attack - 1) / 4
    metre = (realToFrac $ floor attack') + 1 -- metre? [1, 2, 3, 4]  => 1 [0, 0.25, 0.5, 0.75]
    rPat = catMaybes [cambiarRitmo' metre attack] -- fmap (\n -> (metre, (realToFrac n) /4)) ns'

    nPat = tecladoSampleNPattern0 (style c)
    nuevoE = (style c) {
                            tecladoRhythmPattern0 = rPat, -- listaDeStringsARhythmicPattern rPat notes,
                            tecladoSampleNPattern0 = nPat, -- listaDeStringsANPattern nPat notes,
                            tecladoPitchPattern0 = ("acorde", notes)-- ("acorde", [note])
                          }

---- test
-- p test this by taking the layer out
-- acompanamientoConVoicingSel :: Double -> [[Note]] -> Layer -> Layer
acompanamientoConVoicingSelTest n notes = do
    let  n' | n == 0 = 1
            |otherwise = abs $ n - 1
    let rPat = [(1, (realToFrac n') / 4)]
    let nPat = NPattern1 [0]
    let tecladoRhythmPattern = listaDeStringsARhythmicPattern rPat notes
    let tecladoSampleNPattern = listaDeStringsANPattern nPat notes
    let tecladoPitchPattern = ("intervalo", listaDeStringsANote notes)
    (show tecladoRhythmPattern, show tecladoSampleNPattern, show tecladoPitchPattern)

-- acompanamiento (2 4) ("f" "3a" $ "5a" (-1))
parseAcompanamientosConVoicingSel :: H Layer
parseAcompanamientosConVoicingSel = parseAcompanamientosConVoicingSel' <*> parseLayer

parseAcompanamientosConVoicingSel' :: H (Layer -> Layer)
parseAcompanamientosConVoicingSel' = parseAcompanamientosConVoicingSel'' <*> parseStringsAListaDeNotes -- parseStringsAListaDeNotes -- praseListaDeListaStringAListaDeAcordes

parseAcompanamientosConVoicingSel'' :: H ([Note] -> Layer -> Layer)
parseAcompanamientosConVoicingSel'' = parseAcompanamientosConVoicingSel''' <*> parseAtaquesAListaDeAtaques -- rationalList

parseAcompanamientosConVoicingSel''' :: H ([Rational] -> [Note] -> Layer -> Layer)
parseAcompanamientosConVoicingSel''' = acompanamientosConVoicingSel <$ (reserved "acompañamiento" <|> reserved "acompanamiento")

acompanamientosConVoicingSel :: [Rational] -> [Note] -> Layer -> Layer
acompanamientosConVoicingSel rs notes c = c {style = nuevoE}
  where
    -- metre = 1
    -- ns' = fmap (\n -> if (n == 0) then 0 else (abs $ n - 1)) ns -- [1, 2, 3, 4] a [0, 1, 2, 3]
    -- rPat = fmap (\n -> (metre, (realToFrac n) /4)) ns'-- [(1, (realToFrac n') / 4)]
    rs' = ((maximum rs) - 1) / 4 -- metre? [1, 2, 3, 4]  => 1 [0, 0.25, 0.5, 0.75]
    metre = (realToFrac $ floor rs') + 1
    rPat = cambiarRitmo'' metre rs

    nPat (NPattern1 xs) = NPattern1 $ concat $ replicate (length rs) xs
    nuevoE = (style c) {
                            tecladoRhythmPattern0 = rPat, -- listaDeStringsARhythmicPattern rPat notes,
                            tecladoSampleNPattern0 =  nPat $ tecladoSampleNPattern0 (style c), -- listaDeStringsANPattern nPat notes,
                            tecladoPitchPattern0 = ("acorde", notes)-- ("intervalo", listaDeStringsANote notes)
                          }

parseNote :: H Note --(Relacion, Double, Octava)
parseNote = parseNoteConOctava
         <|> parseNoteConOctavaAuto


parseNoteConOctava :: H Note
parseNoteConOctava = parseNoteConOctava' <*> double

parseNoteConOctava' :: H (Octava -> Note)
parseNoteConOctava' = do
  i <- string
  return $ \o -> intervalo i o


parseNoteConOctavaAuto :: H Note
parseNoteConOctavaAuto = do
  i <- string
  return $ intervalo i 0


-- listaDeStringsANote ::  [[String]] -> [Note]
-- listaDeStringsANote xs = listaDeListaStringAListaDeNota xs -- [Note]
listaDeStringsANote ::   [[Note]] -> [Note]
listaDeStringsANote xs = concat xs -- [Note]

-- listaDeStringsARhythmicPattern :: RhythmicPattern -> [[String]] -> RhythmicPattern
listaDeStringsARhythmicPattern :: RhythmicPattern -> [[Note]] -> RhythmicPattern
listaDeStringsARhythmicPattern rs xs = do
  let z = zip xs rs -- [([String], INt)]
  listaDeListaDeStringARhythmicP z --
-- let rPat' = listaDeListaDeStringARhythmicP xs [(1, (realToFrac n) / 4)]  -- [(String, RhythmicPosition)]

-- listaDeStringsANPattern :: NPattern -> [[String]] -> NPattern
listaDeStringsANPattern :: NPattern -> [[Note]] -> NPattern
listaDeStringsANPattern (NPattern1 ns) xs = do
  let z = zip xs ns -- [([String], Int)]
  NPattern1 $ listaDeListaDeStringAN z --[Int]

-- listaDeListaDeStringARhythmicP :: [([String], RhythmicPosition)] -> RhythmicPattern
listaDeListaDeStringARhythmicP :: [([Note], RhythmicPosition)] -> RhythmicPattern
listaDeListaDeStringARhythmicP xs = do
  let a = concat $ fmap (\x -> listaDeStringARhythmicP x) xs
  fmap snd a

-- listaDeStringARhythmicP :: ([String], RhythmicPosition) -> [(String, RhythmicPosition)] -- e.g. [((0,1),"f")]
listaDeStringARhythmicP :: ([Note], RhythmicPosition) -> [(Note, RhythmicPosition)] -- e.g. [((0,1),"f")]
listaDeStringARhythmicP (xs, rs) = fmap (\x -> (x, rs)) xs

listaDeListaStringAListaDeNota :: [[String]] -> [Note]
listaDeListaStringAListaDeNota xs = concat $ fmap listaDeStringAListaDeNota xs

listaDeStringAListaDeNota :: [String] -> [Note] -- [intervalo "unisono" 0, intervalo "3a" 0, intervalo "5a" 0]
listaDeStringAListaDeNota xs = fmap stringANote xs

-- listaDeListaDeStringAN :: [([String], Int)] -> [Int]
listaDeListaDeStringAN :: [([Note], Int)] -> [Int]
listaDeListaDeStringAN xs = do
  let a = concat $ fmap (\x -> listaDeStringAN x) xs
  fmap snd a

listaDeStringAN :: ([Note], Int) -> [(Note, Int)]-- e.g. [("f", 0)]
listaDeStringAN (xs, n) = fmap (\x -> (x, n)) xs

stringANote :: String -> Note
stringANote s = intervalo s 0

--
praseListaDeListaStringAListaDeAcordes :: H [[Note]]
praseListaDeListaStringAListaDeAcordes = list parseStringsAListaDeNotes

-- acompanamiento 2 ["f" "3a" "5a", ]
-- parseStringsAListaDeNotes :: H [String]
parseStringsAListaDeNotes :: H [Note]
parseStringsAListaDeNotes = parseUnStringAListadeNotas
                          <|> parseDosStringsAListadeNotas
                          <|> parseTresStringsAListadeNotas
                          <|> parseCuatroStringsAListadeNotas
                          <|> parseCincoStringsAListadeNotas
                          <|> parseSeisStringsAListadeNotas
                          <|> parseSieteStringsAListadeNotas
                          <|> parseOchoStringsAListadeNotas
                          <|> parseNueveStringsAListadeNotas
                          <|> parseDiezStringsAListadeNotas
                          <|> parseOnceStringsAListadeNotas
                          <|> parseDoceStringsAListadeNotas



parseDoceStringsAListadeNotas :: H [Note]
parseDoceStringsAListadeNotas = parseDoceStringsAListadeNotas' <*> parseNote

parseDoceStringsAListadeNotas' :: H (Note -> [Note])
parseDoceStringsAListadeNotas' = parseDoceStringsAListadeNotas'' <*> parseNote

parseDoceStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseDoceStringsAListadeNotas'' = parseDoceStringsAListadeNotas''' <*> parseNote

parseDoceStringsAListadeNotas''' :: H (Note -> Note -> Note -> [Note])
parseDoceStringsAListadeNotas''' = parseDoceStringsAListadeNotas'''' <*> parseNote

parseDoceStringsAListadeNotas'''' :: H (Note -> Note -> Note -> Note -> [Note])
parseDoceStringsAListadeNotas'''' = parseDoceStringsAListadeNotas''''' <*> parseNote

parseDoceStringsAListadeNotas''''' :: H (Note -> Note -> Note -> Note -> Note -> [Note])
parseDoceStringsAListadeNotas''''' = parseDoceStringsAListadeNotas'''''' <*> parseNote

parseDoceStringsAListadeNotas'''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseDoceStringsAListadeNotas'''''' = parseDoceStringsAListadeNotas''''''' <*> parseNote

parseDoceStringsAListadeNotas''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseDoceStringsAListadeNotas''''''' = parseDoceStringsAListadeNotas'''''''' <*> parseNote

parseDoceStringsAListadeNotas'''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note ->  Note -> [Note])
parseDoceStringsAListadeNotas'''''''' = parseDoceStringsAListadeNotas''''''''' <*> parseNote

parseDoceStringsAListadeNotas''''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note ->  Note -> Note -> [Note])
parseDoceStringsAListadeNotas''''''''' = parseDoceStringsAListadeNotas'''''''''' <*> parseNote

parseDoceStringsAListadeNotas'''''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note ->  Note -> Note -> Note -> [Note])
parseDoceStringsAListadeNotas'''''''''' = parseDoceStringsAListadeNotas''''''''''' <*> parseNote

parseDoceStringsAListadeNotas''''''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note ->  [Note])
parseDoceStringsAListadeNotas''''''''''' = do
  s1 <- parseNote
  return $ \s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 -> stringsAListadeDoceNotas s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12

--
parseOnceStringsAListadeNotas :: H [Note]
parseOnceStringsAListadeNotas = parseOnceStringsAListadeNotas' <*> parseNote

parseOnceStringsAListadeNotas' :: H (Note -> [Note])
parseOnceStringsAListadeNotas' = parseOnceStringsAListadeNotas'' <*> parseNote

parseOnceStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseOnceStringsAListadeNotas'' = parseOnceStringsAListadeNotas''' <*> parseNote

parseOnceStringsAListadeNotas''' :: H (Note -> Note -> Note -> [Note])
parseOnceStringsAListadeNotas''' = parseOnceStringsAListadeNotas'''' <*> parseNote

parseOnceStringsAListadeNotas'''' :: H (Note -> Note -> Note -> Note -> [Note])
parseOnceStringsAListadeNotas'''' = parseOnceStringsAListadeNotas''''' <*> parseNote

parseOnceStringsAListadeNotas''''' :: H (Note -> Note -> Note -> Note -> Note -> [Note])
parseOnceStringsAListadeNotas''''' = parseOnceStringsAListadeNotas'''''' <*> parseNote

parseOnceStringsAListadeNotas'''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseOnceStringsAListadeNotas'''''' = parseOnceStringsAListadeNotas''''''' <*> parseNote

parseOnceStringsAListadeNotas''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseOnceStringsAListadeNotas''''''' = parseOnceStringsAListadeNotas'''''''' <*> parseNote

parseOnceStringsAListadeNotas'''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note ->  Note -> [Note])
parseOnceStringsAListadeNotas'''''''' = parseOnceStringsAListadeNotas''''''''' <*> parseNote

parseOnceStringsAListadeNotas''''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note ->  Note -> Note -> [Note])
parseOnceStringsAListadeNotas''''''''' = parseOnceStringsAListadeNotas'''''''''' <*> parseNote

parseOnceStringsAListadeNotas'''''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note ->  [Note])
parseOnceStringsAListadeNotas'''''''''' = do
  s1 <- parseNote
  return $ \s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 -> stringsAListadeOnceNotas s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11

  --

parseDiezStringsAListadeNotas :: H [Note]
parseDiezStringsAListadeNotas = parseDiezStringsAListadeNotas' <*> parseNote

parseDiezStringsAListadeNotas' :: H (Note -> [Note])
parseDiezStringsAListadeNotas' = parseDiezStringsAListadeNotas'' <*> parseNote

parseDiezStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseDiezStringsAListadeNotas'' = parseDiezStringsAListadeNotas''' <*> parseNote

parseDiezStringsAListadeNotas''' :: H (Note -> Note -> Note -> [Note])
parseDiezStringsAListadeNotas''' = parseDiezStringsAListadeNotas'''' <*> parseNote

parseDiezStringsAListadeNotas'''' :: H (Note -> Note -> Note -> Note -> [Note])
parseDiezStringsAListadeNotas'''' = parseDiezStringsAListadeNotas''''' <*> parseNote

parseDiezStringsAListadeNotas''''' :: H (Note -> Note -> Note -> Note -> Note -> [Note])
parseDiezStringsAListadeNotas''''' = parseDiezStringsAListadeNotas'''''' <*> parseNote

parseDiezStringsAListadeNotas'''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseDiezStringsAListadeNotas'''''' = parseDiezStringsAListadeNotas''''''' <*> parseNote

parseDiezStringsAListadeNotas''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseDiezStringsAListadeNotas''''''' = parseDiezStringsAListadeNotas'''''''' <*> parseNote

parseDiezStringsAListadeNotas'''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note ->  Note -> [Note])
parseDiezStringsAListadeNotas'''''''' = parseDiezStringsAListadeNotas''''''''' <*> parseNote

parseDiezStringsAListadeNotas''''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note ->  [Note])
parseDiezStringsAListadeNotas''''''''' = do
  s1 <- parseNote
  return $ \s2 s3 s4 s5 s6 s7 s8 s9 s10 -> stringsAListadeDiezNotas s1 s2 s3 s4 s5 s6 s7 s8 s9 s10

--
parseNueveStringsAListadeNotas :: H [Note]
parseNueveStringsAListadeNotas = parseNueveStringsAListadeNotas' <*> parseNote

parseNueveStringsAListadeNotas' :: H (Note -> [Note])
parseNueveStringsAListadeNotas' = parseNueveStringsAListadeNotas'' <*> parseNote

parseNueveStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseNueveStringsAListadeNotas'' = parseNueveStringsAListadeNotas''' <*> parseNote

parseNueveStringsAListadeNotas''' :: H (Note -> Note -> Note -> [Note])
parseNueveStringsAListadeNotas''' = parseNueveStringsAListadeNotas'''' <*> parseNote

parseNueveStringsAListadeNotas'''' :: H (Note -> Note -> Note -> Note -> [Note])
parseNueveStringsAListadeNotas'''' = parseNueveStringsAListadeNotas''''' <*> parseNote

parseNueveStringsAListadeNotas''''' :: H (Note -> Note -> Note -> Note -> Note -> [Note])
parseNueveStringsAListadeNotas''''' = parseNueveStringsAListadeNotas'''''' <*> parseNote

parseNueveStringsAListadeNotas'''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseNueveStringsAListadeNotas'''''' = parseNueveStringsAListadeNotas''''''' <*> parseNote

parseNueveStringsAListadeNotas''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseNueveStringsAListadeNotas''''''' = parseNueveStringsAListadeNotas'''''''' <*> parseNote

parseNueveStringsAListadeNotas'''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseNueveStringsAListadeNotas'''''''' = do
  s1 <- parseNote
  return $ \s2 s3 s4 s5 s6 s7 s8 s9 -> stringsAListadeNueveNotas s1 s2 s3 s4 s5 s6 s7 s8 s9

--

parseOchoStringsAListadeNotas :: H [Note]
parseOchoStringsAListadeNotas = parseOchoStringsAListadeNotas' <*> parseNote

parseOchoStringsAListadeNotas' :: H (Note -> [Note])
parseOchoStringsAListadeNotas' = parseOchoStringsAListadeNotas'' <*> parseNote

parseOchoStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseOchoStringsAListadeNotas'' = parseOchoStringsAListadeNotas''' <*> parseNote

parseOchoStringsAListadeNotas''' :: H (Note -> Note -> Note -> [Note])
parseOchoStringsAListadeNotas''' = parseOchoStringsAListadeNotas'''' <*> parseNote

parseOchoStringsAListadeNotas'''' :: H (Note -> Note -> Note -> Note -> [Note])
parseOchoStringsAListadeNotas'''' = parseOchoStringsAListadeNotas''''' <*> parseNote

parseOchoStringsAListadeNotas''''' :: H (Note -> Note -> Note -> Note -> Note -> [Note])
parseOchoStringsAListadeNotas''''' = parseOchoStringsAListadeNotas'''''' <*> parseNote

parseOchoStringsAListadeNotas'''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseOchoStringsAListadeNotas'''''' = parseOchoStringsAListadeNotas''''''' <*> parseNote

parseOchoStringsAListadeNotas''''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> Note ->[Note])
parseOchoStringsAListadeNotas''''''' = do
  s1 <- parseNote
  return $ \s2 s3 s4 s5 s6 s7 s8 -> stringsAListadeOchoNotas s1 s2 s3 s4 s5 s6 s7 s8

--

parseSieteStringsAListadeNotas :: H [Note]
parseSieteStringsAListadeNotas = parseSieteStringsAListadeNotas' <*> parseNote

parseSieteStringsAListadeNotas' :: H (Note -> [Note])
parseSieteStringsAListadeNotas' = parseSieteStringsAListadeNotas'' <*> parseNote

parseSieteStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseSieteStringsAListadeNotas'' = parseSieteStringsAListadeNotas''' <*> parseNote

parseSieteStringsAListadeNotas''' :: H (Note -> Note -> Note -> [Note])
parseSieteStringsAListadeNotas''' = parseSieteStringsAListadeNotas'''' <*> parseNote

parseSieteStringsAListadeNotas'''' :: H (Note -> Note -> Note -> Note -> [Note])
parseSieteStringsAListadeNotas'''' = parseSieteStringsAListadeNotas''''' <*> parseNote

parseSieteStringsAListadeNotas''''' :: H (Note -> Note -> Note -> Note -> Note -> [Note])
parseSieteStringsAListadeNotas''''' = parseSieteStringsAListadeNotas'''''' <*> parseNote

parseSieteStringsAListadeNotas'''''' :: H (Note -> Note -> Note -> Note -> Note -> Note -> [Note])
parseSieteStringsAListadeNotas'''''' = do
  s1 <- parseNote
  return $ \s2 s3 s4 s5 s6 s7 -> stringsAListadeSieteNotas s1 s2 s3 s4 s5 s6 s7

--
parseSeisStringsAListadeNotas :: H [Note]
parseSeisStringsAListadeNotas = parseSeisStringsAListadeNotas' <*> parseNote

parseSeisStringsAListadeNotas' :: H (Note -> [Note])
parseSeisStringsAListadeNotas' = parseSeisStringsAListadeNotas'' <*> parseNote

parseSeisStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseSeisStringsAListadeNotas'' = parseSeisStringsAListadeNotas''' <*> parseNote

parseSeisStringsAListadeNotas''' :: H (Note -> Note -> Note -> [Note])
parseSeisStringsAListadeNotas''' = parseSeisStringsAListadeNotas'''' <*> parseNote

parseSeisStringsAListadeNotas'''' :: H (Note -> Note -> Note -> Note -> [Note])
parseSeisStringsAListadeNotas'''' = parseSeisStringsAListadeNotas''''' <*> parseNote

parseSeisStringsAListadeNotas''''' :: H (Note -> Note -> Note -> Note -> Note -> [Note])
parseSeisStringsAListadeNotas''''' = do
  s1 <- parseNote
  return $ \s2 s3 s4 s5 s6 -> stringsAListadeSeisNotas s1 s2 s3 s4 s5 s6

--

parseCincoStringsAListadeNotas :: H [Note]
parseCincoStringsAListadeNotas = parseCuatroStringsAListadeNotas' <*> parseNote

parseCincoStringsAListadeNotas' :: H (Note -> [Note])
parseCincoStringsAListadeNotas' = parseCincoStringsAListadeNotas'' <*> parseNote

parseCincoStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseCincoStringsAListadeNotas'' = parseCincoStringsAListadeNotas''' <*> parseNote

parseCincoStringsAListadeNotas''' :: H (Note -> Note -> Note -> [Note])
parseCincoStringsAListadeNotas''' = parseCincoStringsAListadeNotas'''' <*> parseNote

parseCincoStringsAListadeNotas'''' :: H (Note -> Note -> Note -> Note -> [Note])
parseCincoStringsAListadeNotas'''' = do
  s1 <- parseNote
  return $ \s2 s3 s4 s5 -> stringsAListadeCincoNotas s1 s2 s3 s4 s5
  --

parseCuatroStringsAListadeNotas :: H [Note]
parseCuatroStringsAListadeNotas = parseCuatroStringsAListadeNotas' <*> parseNote

parseCuatroStringsAListadeNotas' :: H (Note -> [Note])
parseCuatroStringsAListadeNotas' = parseCuatroStringsAListadeNotas'' <*> parseNote

parseCuatroStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseCuatroStringsAListadeNotas'' = parseCuatroStringsAListadeNotas''' <*> parseNote

parseCuatroStringsAListadeNotas''' :: H (Note -> Note -> Note -> [Note])
parseCuatroStringsAListadeNotas''' = do
  s1 <- parseNote
  return $ \s2 s3 s4 -> stringsAListadeCuatroNotas s1 s2 s3 s4

--
parseTresStringsAListadeNotas :: H [Note]
parseTresStringsAListadeNotas = parseTresStringsAListadeNotas' <*> parseNote

parseTresStringsAListadeNotas' :: H (Note -> [Note])
parseTresStringsAListadeNotas' = parseTresStringsAListadeNotas'' <*> parseNote

parseTresStringsAListadeNotas'' :: H (Note -> Note -> [Note])
parseTresStringsAListadeNotas'' = do
  s1 <- parseNote
  return $ \s2 s3 -> stringsAListadeTresNotas s1 s2 s3

--
parseDosStringsAListadeNotas :: H  [Note]
parseDosStringsAListadeNotas = parseDosStringsAListadeNotas' <*> parseNote

parseDosStringsAListadeNotas' :: H (Note -> [Note])
parseDosStringsAListadeNotas' = do
  s1 <- parseNote
  return $ \s2 -> stringsAListadeDosNotas s1 s2

--
parseUnStringAListadeNotas :: H [Note]
parseUnStringAListadeNotas = do
  -- s1 <- string
  s1 <- parseNote -- Note
  return $ stringAListadeUnaNota s1

-- helper funcs para acompanamiento
-- stringAListadeUnaNota :: String -> [String]
stringAListadeUnaNota :: Note -> [Note]
stringAListadeUnaNota s1 = [s1]

stringsAListadeDosNotas :: Note -> Note -> [Note]
stringsAListadeDosNotas s1 s2 = [s1, s2]

stringsAListadeTresNotas :: Note -> Note -> Note -> [Note]
stringsAListadeTresNotas s1 s2 s3 = [s1, s2, s3]

stringsAListadeCuatroNotas :: Note -> Note -> Note -> Note -> [Note]
stringsAListadeCuatroNotas s1 s2 s3 s4 = [s1, s2, s3, s4]

stringsAListadeCincoNotas :: Note -> Note -> Note -> Note -> Note -> [Note]
stringsAListadeCincoNotas s1 s2 s3 s4 s5 = [s1, s2, s3, s4, s5]

stringsAListadeSeisNotas :: Note -> Note -> Note -> Note -> Note -> Note -> [Note]
stringsAListadeSeisNotas s1 s2 s3 s4 s5 s6 = [s1, s2, s3, s4, s5, s6]

stringsAListadeSieteNotas :: Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note]
stringsAListadeSieteNotas s1 s2 s3 s4 s5 s6 s7 = [s1, s2, s3, s4, s5, s6, s7]

stringsAListadeOchoNotas :: Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note]
stringsAListadeOchoNotas s1 s2 s3 s4 s5 s6 s7 s8 = [s1, s2, s3, s4, s5, s6, s7, s8]

stringsAListadeNueveNotas :: Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note]
stringsAListadeNueveNotas s1 s2 s3 s4 s5 s6 s7 s8 s9 = [s1, s2, s3, s4, s5, s6, s7, s8, s9]

stringsAListadeDiezNotas :: Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note]
stringsAListadeDiezNotas s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10]

stringsAListadeOnceNotas :: Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note ->  [Note]
stringsAListadeOnceNotas s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11]

stringsAListadeDoceNotas :: Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> Note -> [Note]
stringsAListadeDoceNotas s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12]

-- [60 64 67] [59 62 67]
-- ["f" "3a" "5a", "3a" (-1) "5a" (-1) "f" (-1)]
--
-- helper funcs para acompanamiento con octava. e.g. acompanamiento 2 ["3a" "5a" ]
-- ?

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
                      <|> parseCambiarRitmoAuto'
                      <|> parseCambiarRitmosAuto'
                      <|> parseCambiarRitmo'
                      <|> parseCambiarRitmos'
                      <|> parseCambiarIntervalo'
                      <|> parseCambiarIntervalos'
                      <|> parseCambiarIntervaloConOctava'
                      <|> parseCambiarIntervalosConOctava'
                      <|> parseCambiarIntervaloDouble'
                      <|> parseCambiarIntervaloDoubleConOctava'
                      <|> parseCambiarIntervalosDouble'
                      <|> parseCambiarIntervalosDoubleConOctava'
                      <|> parsePreset'
                      <|> parseAlternar'
                      <|> parseCambiarGain'
                      <|> parseCambiarPaneo'
                      <|> parsePunteo'
                      <|> parsePunteos'
                      <|> parseTumbao'
                      <|> parseaTumbaoBajoVoicingSel'
                      <|> parseaTumbaoBajoVoicingYRitmoSel'
                      <|> parseaTumbaoBajoVoicingsYRitmoSel'
                      <|> parseTumbaoCongasGolpesSel'
                      <|> parseTumbaoCongasGolpesYRitmoSel'
                      <|> parseTumbaoCongasListaDeGolpesSel'
                      <|> parseTumbaoCongasListaDeGolpesYRitmoSel'
                      <|> parseacompanamiento'
                      <|> parseacompanamientos'
                      <|> parseAcompanamientoConVoicingSel'
                      <|> parseAcompanamientosConVoicingSel'


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

    updatedEv gm s t iw ew = liftM ordernarEv $ liftM concat $ liftM2 (++) es0  es1
      where
        iw' = timeToCount t iw -- Rational
        ew' = timeToCount t ew --

        (w0, w1) = alternarWindows'' (toRational n) (toRational $ compas gm) iw' ew' -- find all the windows that comply with certain condition within the provided window

        es0 = mapM (\(i,e) -> f0 gm s0 t (countToTime t i) (countToTime t e)) w0 -- State LayerState [[Events]]
        es1 = mapM (\(i,e) -> f1 gm s1 t (countToTime t i) (countToTime t e)) w1

data Event' = Event' (UTCTime, Map.Map T.Text H.Datum) deriving (Show, Eq)

instance Ord Event' where
   Event' (u1, _) `compare` Event' (u2, _) = u1 `compare` u2

ordernarEv :: [(UTCTime, Map.Map T.Text H.Datum)] -> [(UTCTime, Map.Map T.Text H.Datum)]
ordernarEv evs = do
  let evsToEv = List.sort $ fmap (\e -> Event' e) evs
  fmap (\e -> evToEv e) evsToEv

evToEv :: Event'-> (UTCTime, Map.Map T.Text H.Datum)
evToEv (Event' (utc, d)) = (utc, d)

-- myEvent' :: Event'
-- myEvent' = Event' ((mytime 0.75), Map.fromList [("s", H.string "test")])
-- --
-- myEvents :: [Event']
-- myEvents = [Event' ((mytime 1.0), Map.fromList [("s", H.string "tres")]), Event' ((mytime 0.75), Map.fromList [("s", H.string "dos")]), Event' ((mytime 0.25), Map.fromList [("s", H.string "uno")])]

alternarWindows n compas iw ew = do
  let n' = n * compas
  let iw' = realToFrac $ floor iw
  let ew' = realToFrac $ floor ew
  let lista = [iw', iw' + compas .. ew']
  let lista' = fmap (\e -> (e, e + (1 * compas))) lista
  let lista'' = drop 1 $ init lista'
  let lista''' = firstItem : lista'' ++ lastItem
      firstItem | (realToFrac $ floor iw) == (realToFrac $ floor ew) = (realToFrac iw, realToFrac ew)
                | otherwise = (realToFrac iw, (realToFrac $ floor iw) + (1 * compas))

      lastItem | (realToFrac $ floor ew) == (realToFrac $ floor iw) = []
               | (realToFrac ew) > (realToFrac $ floor ew) = [(realToFrac $ floor ew, realToFrac ew)]
               | otherwise = []
  let x = catMaybes $ fmap (\(x, y) -> if (mod' (realToFrac x) (realToFrac n')) /= ((realToFrac n') - compas) then Just (x , y) else Nothing) lista'''
  let fx = catMaybes $ fmap (\(x,y) -> if (mod' (realToFrac x) (realToFrac n')) == ((realToFrac n') - compas) then Just (x , y) else Nothing ) lista'''
  (x, fx)

-- alternarWindows' :: Rational -> Rational -> Rational -> Rational -> ([(Rational, Rational)], [(Rational, Rational)])
alternarWindows' n compas iw ew = do
  let n' = n * compas
  let iw' = realToFrac $ floor iw
  let ew' = realToFrac $ floor ew
  let lista = [iw', iw' + compas .. ew']
  let lista' = fmap (\e -> (e, e + (1 * compas))) lista
  let lista'' = drop 1 $ init lista'
  let lista''' = firstItem ++ lista'' ++ lastItem
      firstItem | (realToFrac $ floor iw) == (realToFrac $ floor ew) = [(realToFrac iw, realToFrac ew)]
                | (realToFrac iw) == ((realToFrac $ floor iw) + (1 * compas)) = []
                | otherwise = [(realToFrac iw, (realToFrac $ floor iw) + (1 * compas))]

      lastItem | (realToFrac $ floor ew) == (realToFrac $ floor iw) = []
               | (realToFrac ew) > (realToFrac $ floor ew) = [(realToFrac $ floor ew, realToFrac ew)]
               | otherwise = []
  let x = catMaybes $ fmap (\(x, y) -> if (mod' (realToFrac x) (realToFrac n')) /= ((realToFrac n') - compas) then Just (x , y) else Nothing) lista'''
  let fx = catMaybes $ fmap (\(x,y) -> if (mod' (realToFrac x) (realToFrac n')) == ((realToFrac n') - compas) then Just (x , y) else Nothing ) lista'''
  (x, fx)

-- alternarWindows'' :: Double -> Double -> Double -> Double -> ([(Double, Double)], [(Double, Double)])
alternarWindows'' n compas iw ew = do
  let n' = n * compas
  let iw' = realToFrac $ floor iw
  let ew' = realToFrac $ ceiling ew
  let lista = [iw', (iw' + compas).. ew']
  let lowerFilteredList = Prelude.filter  ((<=) iw) lista
  let upwardsFilteredList = Prelude.filter  ((>=) ew) lowerFilteredList
  let lista' = fmap (\e -> (e, e + compas)) upwardsFilteredList
  let confirmHead |((length lista') > 0) && (iw < (fst $ head lista')) = ((fst $ head lista') - ((fst $ head lista') - iw) , fst $ head lista') : lista'
                  | ((length lista') > 0) && (iw == (fst $ head lista')) = lista'
                  | ((length lista') == 0) = [(iw, ew)]
  let confirmLast | (length confirmHead == 1) = [(fst $ head confirmHead, ew)]
                  | (length confirmHead > 1) && (ew == (fst $ last confirmHead)) = init confirmHead
                  | (length confirmHead > 1) && (ew > (fst $ last confirmHead)) = init confirmHead ++ [(snd $ last $ init confirmHead, ew)]
  let x = catMaybes $ fmap (\(a, b) -> compararX (a, b) compas n') confirmLast
  let fx = catMaybes $ fmap (\(a, b) -> compararFx (a, b) compas n') confirmLast
  (x, fx)

-- compararX :: (Double, Double) -> Double -> Double -> Maybe (Double, Double)
compararX (a,b) compas n
  | (a > 0) && (a < (realToFrac $ floor a) + compas) = if (mod' (realToFrac $ floor a) (realToFrac n)) /= ((realToFrac n) - compas) then Just (a , b) else Nothing
  | (a > (realToFrac $ floor a) + compas) && (a < (realToFrac $ ceiling a)) = if (mod' ((realToFrac $ floor a) + compas) (realToFrac n)) /= ((realToFrac n) - compas) then Just (a , b) else Nothing
  | otherwise = if (mod' (realToFrac a) (realToFrac n)) /= ((realToFrac n) - compas) then Just (a , b) else Nothing

-- compararFx :: (Double, Double) -> Double -> Double -> Maybe (Double, Double)
compararFx (a,b) compas n
  | (a > 0) && (a < (realToFrac $ floor a) + compas) = if (mod' (realToFrac $ floor a) (realToFrac n)) == ((realToFrac n) - compas) then Just (a , b) else Nothing
  | (a > (realToFrac $ floor a) + compas) && (a < (realToFrac $ ceiling a)) = if (mod' ((realToFrac $ floor a) + compas) (realToFrac n)) == ((realToFrac n) - compas) then Just (a , b) else Nothing
  | otherwise = if (mod' (realToFrac a) (realToFrac n)) == ((realToFrac n) - compas) then Just (a , b) else Nothing

-- test funcs
-- let l = alternar 2 (tonicaYquinta) (seleccionarEstilo cumbia bajo)
-- (runState  (getEvents l testgmm cumbia  mytempo (mytime 0) (mytime 1)) emptyLayerState)

-- helper functions

stringList :: H [String]
stringList = list string

rationalList :: H [Rational]
rationalList = list $ rationalOrInteger

intList :: H [Int]
intList = list $ fromIntegral <$> integer

int :: H Int
int = fromIntegral <$> integer

parseNPattern1 :: H NPattern
parseNPattern1 = do
  ix <- intList
  return $ NPattern1 ix

double :: H Double
double = fromRational <$> rationalOrInteger

doubleList :: H [Double]
doubleList = list double

--
parseListasDeListasDeAtaques ::  H [[Rational]]
parseListasDeListasDeAtaques = list parseAtaquesAListaDeAtaques

parseAtaquesAListaDeAtaques :: H [Rational]
parseAtaquesAListaDeAtaques = parseUnAtaqueAListDeAtaques
               <|> parseDosAtaquesAListDeAtaques
               <|> parseTresAtaquesAListDeAtaques
               <|> parseCuatroAtaquesAListDeAtaques
               <|> parseCincoAtaquesAListDeAtaques
               <|> parseSeisAtaquesAListDeAtaques
               <|> parseSieteAtaquesAListDeAtaques
               <|> parseOchoAtaquesAListDeAtaques
               <|> parseNueveAtaquesAListDeAtaques
               <|> parseDiezAtaquesAListDeAtaques
               <|> parseOnceAtaquesAListDeAtaques
               <|> parseDoceAtaquesAListDeAtaques
               <|> parseTreceAtaquesAListDeAtaques
               <|> parseCatorceAtaquesAListDeAtaques
               <|> parseQuinceAtaquesAListDeAtaques
               <|> parseDieciseisAtaquesAListDeAtaques

parseDieciseisAtaquesAListDeAtaques :: H [Rational]
parseDieciseisAtaquesAListDeAtaques = parseDieciseisAtaquesAListDeAtaques' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques' = parseDieciseisAtaquesAListDeAtaques'' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques'' = parseDieciseisAtaquesAListDeAtaques''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques''' = parseDieciseisAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques'''' = parseDieciseisAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques''''' = parseDieciseisAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques'''''' = parseDieciseisAtaquesAListDeAtaques''''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques''''''' = parseDieciseisAtaquesAListDeAtaques'''''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques'''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques'''''''' = parseDieciseisAtaquesAListDeAtaques''''''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques''''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques''''''''' = parseDieciseisAtaquesAListDeAtaques'''''''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques'''''''''' :: H (Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques'''''''''' = parseDieciseisAtaquesAListDeAtaques''''''''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques''''''''''' :: H (Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques''''''''''' = parseDieciseisAtaquesAListDeAtaques'''''''''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques'''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques'''''''''''' = parseDieciseisAtaquesAListDeAtaques''''''''''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques''''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques''''''''''''' = parseDieciseisAtaquesAListDeAtaques'''''''''''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques'''''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques'''''''''''''' = parseDieciseisAtaquesAListDeAtaques''''''''''''''' <*> rationalOrInteger

parseDieciseisAtaquesAListDeAtaques''''''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDieciseisAtaquesAListDeAtaques''''''''''''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 -> dieciseisAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16

dieciseisAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
dieciseisAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 =  [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16]

-- ("p" "t" "p" (t "a") ...)
parseQuinceAtaquesAListDeAtaques :: H [Rational]
parseQuinceAtaquesAListDeAtaques = parseQuinceAtaquesAListDeAtaques' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques' = parseQuinceAtaquesAListDeAtaques'' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques'' = parseQuinceAtaquesAListDeAtaques''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques''' = parseQuinceAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques'''' = parseQuinceAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques''''' = parseQuinceAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques'''''' = parseQuinceAtaquesAListDeAtaques''''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques''''''' = parseQuinceAtaquesAListDeAtaques'''''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques'''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques'''''''' = parseQuinceAtaquesAListDeAtaques''''''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques''''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques''''''''' = parseQuinceAtaquesAListDeAtaques'''''''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques'''''''''' :: H (Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques'''''''''' = parseQuinceAtaquesAListDeAtaques''''''''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques''''''''''' :: H (Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques''''''''''' = parseQuinceAtaquesAListDeAtaques'''''''''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques'''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques'''''''''''' = parseQuinceAtaquesAListDeAtaques''''''''''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques''''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques''''''''''''' = parseQuinceAtaquesAListDeAtaques'''''''''''''' <*> rationalOrInteger

parseQuinceAtaquesAListDeAtaques'''''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseQuinceAtaquesAListDeAtaques'''''''''''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 -> quinceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15

quinceAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
quinceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 =  [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15]

-- ("p" "t" "p" (t "a") ...)
parseCatorceAtaquesAListDeAtaques :: H [Rational]
parseCatorceAtaquesAListDeAtaques = parseCatorceAtaquesAListDeAtaques' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques' = parseCatorceAtaquesAListDeAtaques'' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques'' = parseCatorceAtaquesAListDeAtaques''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques''' = parseCatorceAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques'''' = parseCatorceAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques''''' = parseCatorceAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques'''''' = parseCatorceAtaquesAListDeAtaques''''''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques''''''' = parseCatorceAtaquesAListDeAtaques'''''''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques'''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques'''''''' = parseCatorceAtaquesAListDeAtaques''''''''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques''''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques''''''''' = parseCatorceAtaquesAListDeAtaques'''''''''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques'''''''''' :: H (Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques'''''''''' = parseCatorceAtaquesAListDeAtaques''''''''''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques''''''''''' :: H (Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques''''''''''' = parseCatorceAtaquesAListDeAtaques'''''''''''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques'''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques'''''''''''' = parseCatorceAtaquesAListDeAtaques''''''''''''' <*> rationalOrInteger

parseCatorceAtaquesAListDeAtaques''''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseCatorceAtaquesAListDeAtaques''''''''''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 -> catorceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14

catorceAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
catorceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 =  [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14]

-- ("p" "t" "p" (t "a") ...)
parseTreceAtaquesAListDeAtaques :: H [Rational]
parseTreceAtaquesAListDeAtaques = parseTreceAtaquesAListDeAtaques' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseTreceAtaquesAListDeAtaques' = parseTreceAtaquesAListDeAtaques'' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques'' = parseTreceAtaquesAListDeAtaques''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques''' = parseTreceAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques'''' = parseTreceAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques''''' = parseTreceAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques'''''' = parseTreceAtaquesAListDeAtaques''''''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques''''''' = parseTreceAtaquesAListDeAtaques'''''''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques'''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques'''''''' = parseTreceAtaquesAListDeAtaques''''''''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques''''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques''''''''' = parseTreceAtaquesAListDeAtaques'''''''''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques'''''''''' :: H (Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques'''''''''' = parseTreceAtaquesAListDeAtaques''''''''''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques''''''''''' :: H (Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques''''''''''' = parseTreceAtaquesAListDeAtaques'''''''''''' <*> rationalOrInteger

parseTreceAtaquesAListDeAtaques'''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseTreceAtaquesAListDeAtaques'''''''''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 -> treceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13

treceAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
treceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 =  [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13]

-- ("p" "t" "p" (t "a") ...)
parseDoceAtaquesAListDeAtaques :: H [Rational]
parseDoceAtaquesAListDeAtaques = parseDoceAtaquesAListDeAtaques' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseDoceAtaquesAListDeAtaques' = parseDoceAtaquesAListDeAtaques'' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques'' = parseDoceAtaquesAListDeAtaques''' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques''' = parseDoceAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques'''' = parseDoceAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques''''' = parseDoceAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques'''''' = parseDoceAtaquesAListDeAtaques''''''' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques''''''' = parseDoceAtaquesAListDeAtaques'''''''' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques'''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques'''''''' = parseDoceAtaquesAListDeAtaques''''''''' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques''''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques''''''''' = parseDoceAtaquesAListDeAtaques'''''''''' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques'''''''''' :: H (Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques'''''''''' = parseDoceAtaquesAListDeAtaques''''''''''' <*> rationalOrInteger

parseDoceAtaquesAListDeAtaques''''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDoceAtaquesAListDeAtaques''''''''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 -> doceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12

doceAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
doceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 =  [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12]

-- ("p" "t" "p" (t "a") ...)
parseOnceAtaquesAListDeAtaques :: H [Rational]
parseOnceAtaquesAListDeAtaques = parseOnceAtaquesAListDeAtaques' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseOnceAtaquesAListDeAtaques' = parseOnceAtaquesAListDeAtaques'' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseOnceAtaquesAListDeAtaques'' = parseOnceAtaquesAListDeAtaques''' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseOnceAtaquesAListDeAtaques''' = parseOnceAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseOnceAtaquesAListDeAtaques'''' = parseOnceAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseOnceAtaquesAListDeAtaques''''' = parseOnceAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseOnceAtaquesAListDeAtaques'''''' = parseOnceAtaquesAListDeAtaques''''''' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseOnceAtaquesAListDeAtaques''''''' = parseOnceAtaquesAListDeAtaques'''''''' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques'''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseOnceAtaquesAListDeAtaques'''''''' = parseOnceAtaquesAListDeAtaques''''''''' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques''''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseOnceAtaquesAListDeAtaques''''''''' = parseOnceAtaquesAListDeAtaques'''''''''' <*> rationalOrInteger

parseOnceAtaquesAListDeAtaques'''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseOnceAtaquesAListDeAtaques'''''''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 -> onceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11

onceAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> [Rational]
onceAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 =  [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11]

-- ("p" "t" "p" (t "a") ...)
parseDiezAtaquesAListDeAtaques :: H [Rational]
parseDiezAtaquesAListDeAtaques = parseDiezAtaquesAListDeAtaques' <*> rationalOrInteger

parseDiezAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseDiezAtaquesAListDeAtaques' = parseDiezAtaquesAListDeAtaques'' <*> rationalOrInteger

parseDiezAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseDiezAtaquesAListDeAtaques'' = parseDiezAtaquesAListDeAtaques''' <*> rationalOrInteger

parseDiezAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseDiezAtaquesAListDeAtaques''' = parseDiezAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseDiezAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseDiezAtaquesAListDeAtaques'''' = parseDiezAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseDiezAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDiezAtaquesAListDeAtaques''''' = parseDiezAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseDiezAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDiezAtaquesAListDeAtaques'''''' = parseDiezAtaquesAListDeAtaques''''''' <*> rationalOrInteger

parseDiezAtaquesAListDeAtaques''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDiezAtaquesAListDeAtaques''''''' = parseDiezAtaquesAListDeAtaques'''''''' <*> rationalOrInteger

parseDiezAtaquesAListDeAtaques'''''''' :: H (Rational -> Rational ->  Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDiezAtaquesAListDeAtaques'''''''' = parseDiezAtaquesAListDeAtaques''''''''' <*> rationalOrInteger

parseDiezAtaquesAListDeAtaques''''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseDiezAtaquesAListDeAtaques''''''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 n10 -> diezAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10

diezAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> Rational -> [Rational]
diezAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 =  [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10]

-- ("p" "t" "p" (t "a") ...)
parseNueveAtaquesAListDeAtaques :: H [Rational]
parseNueveAtaquesAListDeAtaques = parseNueveAtaquesAListDeAtaques' <*> rationalOrInteger

parseNueveAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseNueveAtaquesAListDeAtaques' = parseNueveAtaquesAListDeAtaques'' <*> rationalOrInteger

parseNueveAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseNueveAtaquesAListDeAtaques'' = parseNueveAtaquesAListDeAtaques''' <*> rationalOrInteger

parseNueveAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseNueveAtaquesAListDeAtaques''' = parseNueveAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseNueveAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseNueveAtaquesAListDeAtaques'''' = parseNueveAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseNueveAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseNueveAtaquesAListDeAtaques''''' = parseNueveAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseNueveAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseNueveAtaquesAListDeAtaques'''''' = parseNueveAtaquesAListDeAtaques''''''' <*> rationalOrInteger

parseNueveAtaquesAListDeAtaques''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseNueveAtaquesAListDeAtaques''''''' = parseNueveAtaquesAListDeAtaques'''''''' <*> rationalOrInteger

parseNueveAtaquesAListDeAtaques'''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseNueveAtaquesAListDeAtaques'''''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 n8 n9 -> nueveAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9

nueveAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> Rational -> [Rational]
nueveAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 n9 =  [n1, n2, n3, n4, n5, n6, n7, n8, n9]

-- ("p" "t" "p" (t "a") ...)
parseOchoAtaquesAListDeAtaques :: H [Rational]
parseOchoAtaquesAListDeAtaques = parseOchoAtaquesAListDeAtaques' <*> rationalOrInteger

parseOchoAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseOchoAtaquesAListDeAtaques' = parseOchoAtaquesAListDeAtaques'' <*> rationalOrInteger

parseOchoAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseOchoAtaquesAListDeAtaques'' = parseOchoAtaquesAListDeAtaques''' <*> rationalOrInteger

parseOchoAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseOchoAtaquesAListDeAtaques''' = parseOchoAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseOchoAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseOchoAtaquesAListDeAtaques'''' = parseOchoAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseOchoAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseOchoAtaquesAListDeAtaques''''' = parseOchoAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseOchoAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseOchoAtaquesAListDeAtaques'''''' = parseOchoAtaquesAListDeAtaques''''''' <*> rationalOrInteger

parseOchoAtaquesAListDeAtaques''''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseOchoAtaquesAListDeAtaques''''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 n8 -> ochoAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8

ochoAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational ->  Rational -> [Rational]
ochoAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 n8 =  [n1, n2, n3, n4, n5, n6, n7, n8]

-- ("p" "t" "p" (t "a") ...)
parseSieteAtaquesAListDeAtaques :: H [Rational]
parseSieteAtaquesAListDeAtaques = parseSieteAtaquesAListDeAtaques' <*> rationalOrInteger

parseSieteAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseSieteAtaquesAListDeAtaques' = parseSieteAtaquesAListDeAtaques'' <*> rationalOrInteger

parseSieteAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseSieteAtaquesAListDeAtaques'' = parseSieteAtaquesAListDeAtaques''' <*> rationalOrInteger

parseSieteAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseSieteAtaquesAListDeAtaques''' = parseSieteAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseSieteAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseSieteAtaquesAListDeAtaques'''' = parseSieteAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseSieteAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseSieteAtaquesAListDeAtaques''''' = parseSieteAtaquesAListDeAtaques'''''' <*> rationalOrInteger

parseSieteAtaquesAListDeAtaques'''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseSieteAtaquesAListDeAtaques'''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 n7 -> sieteAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7

sieteAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
sieteAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 n7 =  [n1, n2, n3, n4, n5, n6, n7]

-- ("p" "t" "p" (t "a") ...)
parseSeisAtaquesAListDeAtaques :: H [Rational]
parseSeisAtaquesAListDeAtaques = parseSeisAtaquesAListDeAtaques' <*> rationalOrInteger

parseSeisAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseSeisAtaquesAListDeAtaques' = parseSeisAtaquesAListDeAtaques'' <*> rationalOrInteger

parseSeisAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseSeisAtaquesAListDeAtaques'' = parseSeisAtaquesAListDeAtaques''' <*> rationalOrInteger

parseSeisAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseSeisAtaquesAListDeAtaques''' = parseSeisAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseSeisAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseSeisAtaquesAListDeAtaques'''' = parseSeisAtaquesAListDeAtaques''''' <*> rationalOrInteger

parseSeisAtaquesAListDeAtaques''''' :: H (Rational -> Rational -> Rational -> Rational -> Rational -> [Rational])
parseSeisAtaquesAListDeAtaques''''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 n6 -> seisAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6

seisAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
seisAtaquesAListDeAtaques n1 n2 n3 n4 n5 n6 =  [n1, n2, n3, n4, n5, n6]

-- ("p" "t" "p" (t "a") ...)
parseCincoAtaquesAListDeAtaques :: H [Rational]
parseCincoAtaquesAListDeAtaques = parseCincoAtaquesAListDeAtaques' <*> rationalOrInteger

parseCincoAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseCincoAtaquesAListDeAtaques' = parseCincoAtaquesAListDeAtaques'' <*> rationalOrInteger

parseCincoAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseCincoAtaquesAListDeAtaques'' = parseCincoAtaquesAListDeAtaques''' <*> rationalOrInteger

parseCincoAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseCincoAtaquesAListDeAtaques''' = parseCincoAtaquesAListDeAtaques'''' <*> rationalOrInteger

parseCincoAtaquesAListDeAtaques'''' :: H (Rational -> Rational -> Rational -> Rational -> [Rational])
parseCincoAtaquesAListDeAtaques'''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 n5 -> cincoAtaquesAListDeAtaques n1 n2 n3 n4 n5

cincoAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
cincoAtaquesAListDeAtaques n1 n2 n3 n4 n5 =  [n1, n2, n3, n4, n5]


-- ("p" "t" "p" $ t "a")
parseCuatroAtaquesAListDeAtaques :: H [Rational]
parseCuatroAtaquesAListDeAtaques = parseCuatroAtaquesAListDeAtaques' <*> rationalOrInteger

parseCuatroAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseCuatroAtaquesAListDeAtaques' = parseCuatroAtaquesAListDeAtaques'' <*> rationalOrInteger

parseCuatroAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseCuatroAtaquesAListDeAtaques'' = parseCuatroAtaquesAListDeAtaques''' <*> rationalOrInteger

parseCuatroAtaquesAListDeAtaques''' :: H (Rational -> Rational -> Rational -> [Rational])
parseCuatroAtaquesAListDeAtaques''' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 n4 -> cuatroAtaquesAListDeAtaques n1 n2 n3 n4

cuatroAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> Rational ->  [Rational]
cuatroAtaquesAListDeAtaques n1 n2 n3 n4 =  [n1, n2, n3, n4]

-- ("a" t "t" "a")
parseTresAtaquesAListDeAtaques :: H [Rational]
parseTresAtaquesAListDeAtaques = parseTresAtaquesAListDeAtaques' <*> rationalOrInteger

parseTresAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseTresAtaquesAListDeAtaques' = parseTresAtaquesAListDeAtaques'' <*> rationalOrInteger

parseTresAtaquesAListDeAtaques'' :: H (Rational -> Rational -> [Rational])
parseTresAtaquesAListDeAtaques'' = do
  n1 <- rationalOrInteger
  return $ \n2 n3 -> tresAtaquesAListDeAtaques n1 n2 n3

tresAtaquesAListDeAtaques :: Rational -> Rational -> Rational -> [Rational]
tresAtaquesAListDeAtaques n1 n2 n3 =  [n1, n2, n3]

-- ("a", t "a")
parseDosAtaquesAListDeAtaques :: H [Rational]
parseDosAtaquesAListDeAtaques = parseDosAtaquesAListDeAtaques' <*> rationalOrInteger

parseDosAtaquesAListDeAtaques' :: H (Rational -> [Rational])
parseDosAtaquesAListDeAtaques' = do
  n1 <- rationalOrInteger
  return $ \n2 -> dosAtaquesAListDeAtaques n1 n2

dosAtaquesAListDeAtaques :: Rational -> Rational -> [Rational]
dosAtaquesAListDeAtaques n1 n2 = [n1, n2]

-- (1)
parseUnAtaqueAListDeAtaques :: H [Rational]
parseUnAtaqueAListDeAtaques = do
  n <- rationalOrInteger
  return $ unAtaqueAListDeAtaques n

unAtaqueAListDeAtaques :: Rational -> [Rational]
unAtaqueAListDeAtaques n = [n]

-- the renderer
--   getEvents :: GlobalMaterial -> Style -> Tempo -> BeginWindowTime -> EndWindowTime -> State InstrumentState [Event]
-- render :: (GlobalMaterial,Style,Instrument) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime,Map Text Datum)]
-- runState :: State s a -> s -> (a, s) -- as soon as the state is meaningful I should stop discarding it.
--check Tidal.params for looking at the available params for webdirt
render :: ([Layer], GlobalMaterial) -> Tempo -> UTCTime -> UTCTime -> [Event]
render (ls, gm) tempo iw ew = Prelude.concat $ fmap (\l -> render' (l, gm) tempo iw ew) ls


render' :: (Layer, GlobalMaterial) -> Tempo -> UTCTime -> UTCTime -> [Event]
render' (layer, gm) tempo iw ew = do
   fst $ runState x emptyLayerState --this should be another argument to my render function
    where
      x = getEvents layer gm (style layer) tempo iw ew



renderForStandalone :: ([Layer], GlobalMaterial)-> UTCTime -> UTCTime -> ([Event], Tempo)
renderForStandalone (ls, gm) iw ew = ((Prelude.concat $ fmap (\l -> renderForStandalone' (l, gm) iw ew) ls), (tempoForStandalone gm))


renderForStandalone' :: (Layer, GlobalMaterial) -> UTCTime -> UTCTime -> [Event]
renderForStandalone' (layer, gm) iw ew = do
   fst $ runState x emptyLayerState--this should be another argument to my render function
    where
      x = getEvents layer gm (style layer) (tempoForStandalone gm) iw ew
