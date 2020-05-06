module Sound.Cumbia.Harmony where

import Sound.Cumbia.Generic
import Sound.Cumbia.Rhythm

import Data.Time
import Data.Tempo
import Data.Maybe
import Data.List

data Chord = Chord Pitch ChordType deriving (Show)


type Pitch = Double
type ChordType = [Double]
-- data Harmony = Harmony Chord RhythmicPosition RhythmicPosition
data Harmony = Harmony Chord RhythmicPosition RhythmicPosition deriving (Show)-- :: (Rational,Rational)
-- data Harmony = Harmony Chord Begin
type Progression = [Harmony]

cmajor = Chord 60 major
eminor = Chord 60 minor

myharmony = [Harmony (Chord 60 major) (2,1.5) (2, 2), Harmony (Chord 64 minor) (2, 0) (2, 1)] :: [Harmony]

generatechords :: [Rational] -> [Harmony] ->  [(Rational, [Pitch])]
generatechords attacks chords = concat $ fmap (generateChordsFromRational chords) attacks

generateChordsFromRational :: [Harmony] -> Rational -> [(Rational, [Pitch])]
generateChordsFromRational chords attack   = concat $ fmap (generateSingleChordFromRational attack) chords

generateSingleChordFromRational :: Rational -> Harmony -> [(Rational, [Pitch])]
generateSingleChordFromRational attack (Harmony c start end)
  | compareRationalWChordRange attack start end = [(attack, generateChord c)]
  | otherwise = []

compareRationalWChordRange :: Rational -> RhythmicPosition -> RhythmicPosition -> Bool
compareRationalWChordRange attack (metre,startOffset) (_,endOffset) = do
  let attackInMetre = attack / metre
  let attackInMetre' = fract attackInMetre
  let startInMetre = startOffset / metre
  let endInMetre = endOffset / metre
  let b | startOffset <= endOffset = (attackInMetre' >= startInMetre) && (attackInMetre' < endInMetre)
        | otherwise = (attackInMetre' < startInMetre) && (attackInMetre' >= endInMetre)
  b

-- return a list of pitches from one chord
generateChord :: Chord -> [Pitch]
generateChord (Chord root chordType)  = fmap ((+) root) chordType

concatChord :: (Rational, [Pitch]) -> [(Rational, Pitch)]
concatChord (attack, ps) =  fmap (\p -> (attack, p)) (snd (attack, ps))

concatChords :: [(Rational, [Pitch])] -> [(Rational, Pitch)] -- eg. for [60, 64, 67] three events all whith the same time all with different pitches
concatChords attackAndChords = concat $ fmap (\x -> concatChord x) attackAndChords



-- return a list of lists of pitches from a list of Chords
generateChords :: [Chord] -> [[Pitch]]
generateChords cs = fmap (\x -> generateChord x) cs


major :: ChordType
major = [0, 4, 7]

minor:: ChordType
minor = [0, 3, 7]

major7 :: ChordType
major7 = [0, 4, 7, 11]

minor7 :: ChordType
minor7 = [0,3,7,10]

dom7 :: ChordType
dom7 = [0,4,7,10]

fifth :: ChordType
fifth = [0, 7]

sus4 :: ChordType
sus4 = [0,5,7]

sus2 :: ChordType
sus2 = [0, 2, 7]

aug :: ChordType
aug = [0, 4, 8]

dim :: ChordType
dim = [0, 3, 6]

--test functions
-- myTempo  = Tempo {freq = 1, time = myTime 0, count = 0}
--
-- myDay :: Day
-- myDay = fromGregorian 2020 4 4
--
-- myTime :: Rational -> UTCTime
-- myTime s = UTCTime myDay (realToFrac s)
