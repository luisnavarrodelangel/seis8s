module Style where


import Generic
import Harmony
import Rhythm
-- import GlobalMaterial

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

 pitchPattern :: [Pitch] --PitchPattern -- i.e. [(Rational, Pitch)]? ? Q
 } deriving (Show)

 -- type PitchPostion = (Rational, Pitch)
 -- type PitchPattern = [PitchPostion]
 -- type RhythmicPosition = (Rational,Rational)
 -- type RhythmicPattern = [RhythmicPosition]

-- default :: Style
-- default = Style {
--   pianoRhythmPattern0 = [(1, 0)],  -- (metre, offset)
--   pianoRhythmPattern1 = [],
--   bassRhythmPattern0 = [(1, 0)],
--   bassRhythmPattern1 = [],
--   pitchPattern = []
-- }

-- where cumbia is a collection of all , but no information from the gmm harmonies or pitches. Might be information on how to pick harmonies. resist the temptation to customize, if I need new information I should make a new field to it.
-- cumbia is a collection of knowledges (this would change). Try not to be too specific or universal

cumbia :: Style
cumbia = Style {
    pianoRhythmPattern0 = [(1, 0.75)], -- ie.  [𝄽  𝄽  𝄽  ♩],
    pianoRhythmPattern1 = [(1,0.25), (1, 0.75)], -- ie. [𝄽 ♩ 𝄽 ♩],
    bassRhythmPattern0 = [(1, 0), (1, 0.5)],  --i.e. [♩ 𝄽  ♩ 𝄽 ],
    bassRhythmPattern1 = [(1, 0), (1, 0.5), (1, 0.75)],  --i.e. [♩ 𝄽  ♩ ♩],
    pitchPattern = []
}

-- 1. When to play

rhythmicPattern :: RhythmicPattern -> (Tempo -> UTCTime -> UTCTime ->  [Rational])
rhythmicPattern xs t iw ew  = Data.List.sort $ concat $ fmap (\(x, y) -> findBeats t iw ew x y) xs



--test functions
-- myTempo  = Tempo {freq = 1, time = myTime 0, count = 0}
--
-- myDay :: Day
-- myDay = fromGregorian 2020 4 4
--
-- myTime :: Rational -> UTCTime
-- myTime s = UTCTime myDay (realToFrac s)
