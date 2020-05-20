{-# LANGUAGE OverloadedStrings #-}

module Sound.Cumbia.Instrument where

import Data.Tempo
import Data.Time
import Data.Map as Map
import Data.Fixed
import Control.Monad.State
import Sound.OSC as H
import Data.Text as T
import qualified Data.List as List

import Sound.Cumbia.InstrumentState
import Sound.Cumbia.GlobalMaterial
import Sound.Cumbia.Style
import Sound.Cumbia.Harmony
import Sound.Cumbia.Generic


-- what can be transformed from the instrument based on the Global and Instrument material
data Instrument = Instrument {
  getEvents :: GlobalMaterial -> Style -> Tempo -> BeginWindowTime -> EndWindowTime -> State InstrumentState [Event]
}

type BeginWindowTime = UTCTime
type EndWindowTime = UTCTime
-- type Event = (UTCTime, Map Text Datum)
type Event = (UTCTime, Map Text Datum)

emptyInstrument :: Instrument
emptyInstrument = Instrument {getEvents = emptyEvents}

emptyEvents _ _ _ _ _ = do
  return $ []

-- function that generates an instrument
piano :: Instrument
piano = Instrument { getEvents = pianoEvents}

bajo :: Instrument
bajo = Instrument {getEvents = bajoEvents}

guira :: Instrument
guira = Instrument {getEvents = guiraEvents}

pianoEvents gmm style tempo iw ew = do
  let attacks = rhythmicPattern (pianoRhythmPattern0 style) tempo iw ew  -- [Rational] [(1, 0), (1, 0.5)]
  let chordPattern = generatechords attacks (harmony gmm) -- [(Rational, [Pitch])]
  let pitchPattern = concatChords chordPattern -- [(Rational, Pitch)]
  let time = fmap (\c -> countToTime tempo (fst c)) pitchPattern  -- [UTCTime]
  let instCmap = fmap (\p -> cmap "piano" (snd p)) pitchPattern --Map Text Datum
  let events =  List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

bajoEvents gmm style tempo iw ew = do
  let pat = List.zip (bassRhythmPattern0 style) (bassPitchPattern0 style) --[(RhythmicPattern, Double)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let bassline = generateLine pitchPat (harmony gmm) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) bassline  -- [UTCTime]
  let instCmap = fmap (\p -> cmap "bajo" (snd p)) bassline --Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

guiraEvents gmm style tempo iw ew = do
  let pat = List.zip (guiraRhythmPattern0 style) (guiraSampleNPattern0 style) --[(RhythmicPattern, Double)]
  let hitPat = percHitsPattern pat tempo iw ew --[(Rational, Int)]
  let time = fmap (\c -> countToTime tempo (fst c)) hitPat  -- [UTCTime]
  let instCmap = fmap (\p -> cmap' "guira" (snd p) 60) hitPat --Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

cmap' :: String -> Int -> Pitch -> Map Text Datum
cmap' sampleName sampleIndex pitch = fromList [("s", string sampleName), ("n", int32 sampleIndex), ("note", double pitchAdjustedOctave)]
  where pitchAdjustedOctave = pitch - 60

cmap :: String -> Pitch -> Map Text Datum
cmap sampleName pitch = fromList [("s", string sampleName), ("note", double pitchAdjustedOctave)]
  where pitchAdjustedOctave = pitch - 60
