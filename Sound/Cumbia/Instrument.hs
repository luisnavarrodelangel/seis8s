{-# LANGUAGE OverloadedStrings #-}

module Sound.Cumbia.Instrument where

import Data.Time
import Data.Fixed
import Data.Tempo
import Sound.OSC as H
import qualified Data.Map as M
import Data.Tuple.Select
import Control.Monad.State
import qualified Data.Text  as T
import qualified Data.List as List

import Sound.Cumbia.Style
import Sound.Cumbia.Harmony
import Sound.Cumbia.Generic
import Sound.Cumbia.InstrumentState
import Sound.Cumbia.GlobalMaterial


-- what can be transformed from the instrument based on the Global and Instrument material
data Instrument = Instrument {
  getEvents :: GlobalMaterial -> Style -> Tempo -> BeginWindowTime -> EndWindowTime -> State InstrumentState [Event]
}

type BeginWindowTime = UTCTime
type EndWindowTime = UTCTime
-- type Event = (UTCTime, Map Text Datum)
type Event = (UTCTime, M.Map T.Text Datum)

emptyInstrument :: Instrument
emptyInstrument = Instrument {getEvents = emptyEvents}

emptyEvents _ _ _ _ _ = do
  return $ []

-- function that generates an instrument
cuerda :: Instrument
cuerda = Instrument {getEvents = cuerdaEvents}

piano :: Instrument
piano = Instrument { getEvents = pianoEvents}

bajo :: Instrument
bajo = Instrument {getEvents = bajoEvents}

guira :: Instrument
guira = Instrument {getEvents = guiraEvents}

contras :: Instrument
contras = Instrument {getEvents = contrasEvents}

tarola :: Instrument
tarola = Instrument {getEvents = tarolaEvents}

efecto :: Instrument
efecto = Instrument {getEvents = efectoEvents}

cuerdaEvents gmm style tempo iw ew = do
  let equateLists' = equateLists (cuerdaRhythmPattern0 style) (cuerdaSampleNPattern0 style) (cuerdaPitchPattern0 style)
  let cuerdaRhythmPattern = sel1 equateLists'
  let cuerdaSampleNPattern = sel2 equateLists'
  let cuerdaPitchPattern = sel3 equateLists'
  let nPat = List.zip cuerdaRhythmPattern cuerdaSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip cuerdaRhythmPattern cuerdaPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let cuerdaline = generateLine pitchPat (harmony gmm) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) cuerdaline  -- [UTCTime]
  let instCmap = cmap'' "cuerdas" samplePat cuerdaline--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events


pianoEvents gmm style tempo iw ew = do
  let nPat = List.zip (pianoRhythmPattern0 style) (pianoSampleNPattern0 style) --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let attacks = rhythmicPattern (pianoRhythmPattern0 style) tempo iw ew  -- [Rational] [(1, 0), (1, 0.5)]
  let chordPattern = generatechords attacks (harmony gmm) -- [(Rational, [Pitch])]
  let pitchPattern = concatChords chordPattern -- [(Rational, Pitch)]
  let time = fmap (\c -> countToTime tempo (fst c)) pitchPattern  -- [UTCTime]
  let instCmap = cmap'' "piano" (cycle samplePat) pitchPattern--Map Text Datum
  let events =  List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

-- myharmony = [Harmony (Chord 60 major) (2, 0) (2, 1), Harmony (Chord 62 minor) (2, 1) (2, 2)]
-- testgmm = GlobalMaterial {harmony = myharmony }

bajoEvents gmm style tempo iw ew = do
  let equateLists' = equateLists (bassRhythmPattern0 style) (bassSampleNPattern0 style) (bassPitchPattern0 style)
  let bassRhythmPattern = sel1 equateLists'
  let bassSampleNPattern = sel2 equateLists'
  let bassPitchPattern = sel3 equateLists'
  let nPat = List.zip bassRhythmPattern bassSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip bassRhythmPattern bassPitchPattern --[(RhythmicPattern, Double)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let bassline = generateLine pitchPat (harmony gmm) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) bassline  -- [UTCTime]
  let instCmap = cmap'' "bajo" samplePat bassline--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

guiraEvents gmm style tempo iw ew = do
  let pat = List.zip (guiraRhythmPattern0 style) (guiraSampleNPattern0 style) --[(RhythmicPattern, Int)]
  let hitPat = samplePattern pat tempo iw ew --[(Rational, Int)]
  let time = fmap (\c -> countToTime tempo (fst c)) hitPat  -- [UTCTime]
  let instCmap = fmap (\p -> cmap' "guira" (snd p) 60) hitPat --Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

contrasEvents gmm style tempo iw ew = do
  let pat = List.zip (contrasRhythmPattern0 style) (contrasSampleNPattern0 style) --[(RhythmicPattern, Int)]
  let hitPat = samplePattern pat tempo iw ew --[(Rational, Int)]
  let time = fmap (\c -> countToTime tempo (fst c)) hitPat  -- [UTCTime]
  let instCmap = fmap (\p -> cmap' "contratiempos" (snd p) 60) hitPat --Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

tarolaEvents gmm style tempo iw ew = do
  let pat = List.zip (tarolaRhythmPattern0 style) (tarolaSampleNPattern0 style) --[(RhythmicPattern, Int)]
  let hitPat = samplePattern pat tempo iw ew --[(Rational, Int)]
  let time = fmap (\c -> countToTime tempo (fst c)) hitPat  -- [UTCTime]
  let instCmap = fmap (\p -> cmap' "tarola" (snd p) 60) hitPat --Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

efectoEvents gmm style tempo iw ew = do
  let equateLists' = equateLists (efectoRhythmPattern0 style) (efectoSampleNPattern0 style) (efectoPitchPattern0 style)
  let efectoRhythmPattern = sel1 equateLists'
  let efectoSampleNPattern = sel2 equateLists'
  let efectoPitchPattern = sel3 equateLists'
  let nPat = List.zip efectoRhythmPattern efectoSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip efectoRhythmPattern efectoPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let cuerdaline = generateLine pitchPat (harmony gmm) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) cuerdaline  -- [UTCTime]
  let instCmap = cmap'' "efecto" samplePat cuerdaline--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events



cmap'' :: String -> [(Rational, Int)] ->  [(Rational, Pitch)] -> [M.Map T.Text Datum]
cmap'' sampleName is ps = do
  let is' = fmap (\i -> snd i) is
  let ps' = fmap (\p -> snd p) ps
  fmap (\(i, p) -> cmap' sampleName i p) $ zip is' ps'

cmap' :: String -> Int -> Pitch -> M.Map T.Text Datum
cmap' sampleName sampleIndex pitch = M.fromList [("s", string sampleName), ("n", int32 sampleIndex), ("note", double pitchAdjustedOctave)]
  where pitchAdjustedOctave = pitch - 60

cmap :: String -> Pitch -> M.Map T.Text Datum
cmap sampleName pitch = M.fromList [("s", string sampleName), ("note", double pitchAdjustedOctave)]
  where pitchAdjustedOctave = pitch - 60

-- returns the the rhythm, pitch and n lists with equal number of indices
equateLists :: [(Rational, Rational)] -> [Int] -> [Int] -> ([(Rational, Rational)], [Int], [Int])
equateLists attacks ns chi
  | (length attacks == length ns) && (length ns == length chi) = (attacks, ns, chi)
  | (length attacks > length ns) && (length attacks == length chi) = (attacks, take (length attacks) $ cycle ns, chi)
  | (length attacks > length ns) && (length attacks > length chi) = (attacks, take (length attacks) $ cycle ns, take (length attacks) $ cycle chi)
  | (length attacks == length ns) && (length ns > length chi)  = (attacks, ns, take (length ns) $ cycle chi)
  | (length attacks < length ns) && (length ns == length chi)  = (take (length ns) $ cycle attacks, ns, chi)
  | (length attacks < length ns) && (length ns > length chi) = (take (length ns) $ cycle attacks, ns, take (length ns) $ cycle chi)
  | (length attacks == length ns) && (length ns < length chi) = (take (length chi) $ cycle attacks, take (length chi) $ cycle ns, chi)
  | otherwise = error "case not expected"
-- [0.5, 0.25] [1, 1] [0,1]
-- [0.5, 0.25] [1] [0,1]
-- [0.5, 0.25] [1] [0]
-- [0.25, 0.25] [1,1] [0]
-- [0.5] [1, 1] [0, 1]
-- [0.5] [1, 1] [0]
-- [0.5] [1] [0,1]
