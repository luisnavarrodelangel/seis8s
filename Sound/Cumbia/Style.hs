module Sound.Cumbia.Style where

import Sound.Cumbia.Generic
import Sound.Cumbia.Harmony
import Sound.Cumbia.Rhythm

import Data.Maybe
import Data.Tempo
import Data.Time
import qualified Data.List

-- style should be a collection of individual records that might be accessed or not by the context.
-- The style could have many different fields to access at a time. pianoRhythmPattern4 -- they are defined but no universal. The style is the rhythm (and other stuff) of all the instruments
-- 1) A style is a library of musical information, a style has a pianoRhyhtmPattern

data Style  = Style  {
 pianoRhythmPattern0 :: RhythmicPattern, -- or could this be accompaniment (used for various instruments)?-- [(Rational, Rational)] -- not universal value, but semiuniversal values.
 pianoRhythmPattern1 :: RhythmicPattern,
 bassRhythmPattern0 ::  RhythmicPattern,
 bassRhythmPattern1 ::  RhythmicPattern,
 bassPitchPattern0 :: [Int],  --interval
 guiraRhythmPattern0 :: RhythmicPattern,
 guiraSampleNPattern0 ::[Int] -- this should not take the harmony

 } deriving (Show)


 -- type PitchPostion = (Rational, Pitch)
 -- type PitchPattern = [PitchPostion]
 -- type RhythmicPosition = (Rational,Rational)
 -- type RhythmicPattern = [RhythmicPosition]

defaultStyle :: Style
defaultStyle = Style {
   pianoRhythmPattern0 = [(1, 0)], -- ie.  [𝄽  𝄽  𝄽  ♩],
   pianoRhythmPattern1 = [], -- ie. [𝄽 ♩ 𝄽 ♩],
   bassRhythmPattern0 = [(1, 0)],  --i.e. [♩ 𝄽  ♩ 𝄽 ],
   bassRhythmPattern1 = [],  --i.e. [♩ 𝄽  ♩ ♩],
   bassPitchPattern0 = [0, 1, 2], -- int
   guiraRhythmPattern0 = [(1,0)],
   guiraSampleNPattern0 = [0]
}

-- where cumbia is a collection of all , but no information from the gmm harmonies or pitches. Might be information on how to pick harmonies. resist the temptation to customize, if I need new information I should make a new field to it.
-- cumbia is a collection of knowledges (this would change). Try not to be too specific or universal

-- user to access the presets or only have one preset that can be modified with functions
-- maybe have two, functions that transform them
-- options: override information, field tranformation, tranform all the rhythm at the same time?,
-- eg. remove any attacks but the fundamental, where multiple fields get transformed,
-- but still be particular to the style, so it will still be specific

-- supercollide is modular that expects information from the user.

cumbia :: Style
cumbia = Style {
    pianoRhythmPattern0 = [(1, 0.75)], -- ie.  [𝄽  𝄽  𝄽  ♩],
    pianoRhythmPattern1 = [(1,0.25), (1, 0.75)], -- ie. [𝄽 ♩ 𝄽 ♩],
    bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],  --i.e. [♩ 𝄽  ♩ ♩],
    bassPitchPattern0 = [0, 1, 2], -- index from list of pitches i.e. [60, 64, 67]
    bassRhythmPattern1 = [(1, 0), (1, 0.5)],  --i.e. [♩ 𝄽  ♩ 𝄽 ],
    guiraRhythmPattern0 = [(1, 0), (1, 0.25), (1, 0.375), (1, 0.5), (1, 0.75), (1, 0.875)], --i.e. [♪♫ ♪♫ ♪♫ ♪♫]
    guiraSampleNPattern0 = [0, 1, 2, 0, 1, 2]
  }


-- 1. When to play

rhythmicPattern :: RhythmicPattern -> (Tempo -> UTCTime -> UTCTime ->  [Rational])
rhythmicPattern xs t iw ew  = Data.List.sort $ concat $ fmap (\(x, y) -> findBeats t iw ew x y) xs

percHitsPattern :: [(RhythmicPosition, Int)] -> Tempo -> UTCTime -> UTCTime -> [(Rational, Int)]
percHitsPattern xs t iw ew =  Data.List.sort $ concat $ fmap (\x -> percHitsPattern' x t iw ew) xs

percHitsPattern' :: (RhythmicPosition, Int) -> Tempo -> UTCTime -> UTCTime -> [(Rational, Int)]
percHitsPattern' (xs, sampleNumber) t iw ew  = fmap (\attack -> (attack, sampleNumber)) attacks
  where
    attacks = findBeats t iw ew (fst xs) (snd xs)

pitchPattern :: [(RhythmicPosition, Int)] -> Tempo -> UTCTime -> UTCTime -> [(Rational, Int)]
pitchPattern xs t iw ew =  Data.List.sort $ concat $ fmap (\x -> pitchPattern' x t iw ew) xs

pitchPattern' :: (RhythmicPosition, Int) -> Tempo -> UTCTime -> UTCTime -> [(Rational, Int)]
pitchPattern' (xs, interval) t iw ew  = fmap (\attack -> (attack, interval)) attacks
  where
    attacks = findBeats t iw ew (fst xs) (snd xs)

--webdirt uses end uses the % dependant on the length of the sample, cut is used for drums (might be useful)
--we might need to add the duration parameters
