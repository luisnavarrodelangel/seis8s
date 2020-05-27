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
type NIndex = Int
type ChordIndex = Int

data Style  = Style  {
 pianoSampleNPattern0 :: [NIndex],
 pianoSampleNPattern1 :: [NIndex],
 pianoPitchPattern :: [ChordIndex],
 pianoRhythmPattern0 :: RhythmicPattern, -- or could this be accompaniment (used for various instruments)?-- [(Rational, Rational)] -- not universal value, but semiuniversal values.
 pianoRhythmPattern1 :: RhythmicPattern,

 cuerdaRhythmPattern0 :: RhythmicPattern,
 cuerdaSampleNPattern0 :: [NIndex],
 cuerdaPitchPattern0 :: [ChordIndex],

 efectoRhythmPattern0 :: RhythmicPattern,
 efectoSampleNPattern0 :: [NIndex],
 efectoPitchPattern0 :: [ChordIndex],

 bassSampleNPattern0 :: [NIndex],
 bassSampleNPattern1 :: [NIndex],
 bassSampleNPattern2 :: [NIndex],
 bassRhythmPattern0 ::  RhythmicPattern,
 bassRhythmPattern1 ::  RhythmicPattern,
 bassRhythmPattern2 ::  RhythmicPattern,
 bassPitchPattern0 :: [ChordIndex],  --index
 bassPitchPattern1 :: [ChordIndex],  --index
 bassPitchPattern2 :: [ChordIndex],  --index

 guiraRhythmPattern0 :: RhythmicPattern,
 guiraSampleNPattern0 ::[NIndex], -- this should not take the harmony

 contrasRhythmPattern0 :: RhythmicPattern,
 contrasSampleNPattern0 :: [NIndex],

 tarolaRhythmPattern0 :: RhythmicPattern,
 tarolaSampleNPattern0 :: [NIndex]

 } deriving (Show)


 -- type PitchPostion = (Rational, Pitch)
 -- type PitchPattern = [PitchPostion]
 -- type RhythmicPosition = (Rational,Rational)
 -- type RhythmicPattern = [RhythmicPosition]

defaultStyle :: Style
defaultStyle = Style {
   pianoSampleNPattern0 = [0],
   pianoRhythmPattern0 = [(1, 0)], -- ie.  [𝄽  𝄽  𝄽  ♩],
   pianoRhythmPattern1 = [], -- ie. [𝄽 ♩ 𝄽 ♩],
   pianoPitchPattern = [0],

   cuerdaRhythmPattern0 = [(1,0)],
   cuerdaSampleNPattern0 = [0],
   cuerdaPitchPattern0 = [0], -- or double?
   bassSampleNPattern0 = [0],
   bassRhythmPattern0 = [(1, 0)],  --i.e. [♩ 𝄽  ♩ 𝄽 ],
   bassRhythmPattern1 = [],  --i.e. [♩ 𝄽  ♩ ♩],
   bassPitchPattern0 = [0, 1, 2], -- int
   bassPitchPattern1 = [],  --interval
   bassRhythmPattern2 =  [],
   bassSampleNPattern2 = [],  --index
   bassPitchPattern2 = [],  --index

   guiraRhythmPattern0 = [(1,0)],
   guiraSampleNPattern0 = [0],

   contrasRhythmPattern0 = [(1, 0)],
   contrasSampleNPattern0 = [0],

   tarolaRhythmPattern0 = [(1, 0)],
   tarolaSampleNPattern0 = [0],

   efectoRhythmPattern0 = [(1, 0)],
   efectoSampleNPattern0 = [0],
   efectoPitchPattern0 = [0]

}

--hh
--metal

-- where cumbia is a collection of all , but no information from the gmm harmonies or pitches. Might be information on how to pick harmonies. resist the temptation to customize, if I need new information I should make a new field to it.
-- cumbia is a collection of knowledges (this would change). Try not to be too specific or universal

-- user to access the presets or only have one preset that can be modified with functions
-- maybe have two, functions that transform them
-- options: override information, field tranformation, tranform all the rhythm at the same time?,
-- eg. remove any attacks but the fundamental, where multiple fields get transformed,
-- but still be particular to the style, so it will still be specific

-- supercollider is modular that expects information from the user.

cumbia :: Style
cumbia = Style {
    pianoRhythmPattern0 = [(1, 0.75)], -- ie.  [𝄽  𝄽  𝄽  ♩],
    pianoSampleNPattern0 = [0],
    pianoRhythmPattern1 = [(1,0.25), (1, 0.75)], -- ie. [𝄽 ♩ 𝄽 ♩],
    pianoSampleNPattern1 = [0, 0],
    pianoPitchPattern = [0, 1, 2], -- not used yet

    cuerdaRhythmPattern0 = [(1,0)],
    cuerdaSampleNPattern0 = [0],
    cuerdaPitchPattern0 = [0], -- or double? (nota [0, 2, 3] cumbia) cuerda

    bassRhythmPattern0 = [(1, 0), (1, 0.5), (1, 0.75)],  --i.e. [♩ 𝄽  ♩ ♩],
    bassSampleNPattern0 = [0, 0, 0],
    bassPitchPattern0 = [0, 1, 2], -- index from list of pitches i.e. [60, 64, 67]
    bassRhythmPattern1 = [(1, 0), (1, 0.5)],  --i.e. [♩ 𝄽  ♩ 𝄽 ],
    bassSampleNPattern1 = [0, 0],
    bassPitchPattern1 = [0, 2],

    bassRhythmPattern2 = [(8, 0), (8, 0.5), (8, 0.75), (8, 1), (8, 1.5), (8, 1.75), (8, 2), (1, 2.5), (8, 2.75), (8, 3), (8, 3.5), (8, 3.75), (8, 4), (8, 4.5), (8, 4.75), (8, 5), (8, 5.5), (8, 5.75), (8, 6), (8, 6.5), (8, 6.75), (8, 7), (8, 7.25), (8, 7.5), (8, 7.75)],  --i.e. [♩ 𝄽  ♩ 𝄽
    bassSampleNPattern2 = take 25 $ cycle [0],
    bassPitchPattern2 = [0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 2, 0, 2],


    guiraRhythmPattern0 = [(1, 0), (1, 0.25), (1, 0.375), (1, 0.5), (1, 0.75), (1, 0.875)], --i.e. [♪♫ ♪♫ ♪♫ ♪♫]
    guiraSampleNPattern0 = [0, 1, 2, 0, 1, 2],

    contrasRhythmPattern0 = [(1, 0), (1, 0.25), (1, 0.5), (1, 0.75)],
    contrasSampleNPattern0 = [0, 1, 0, 1, 0, 1, 0, 1],

    tarolaRhythmPattern0 = [(1, 0.1875), (1, 0.375)],
    tarolaSampleNPattern0 = [0, 0],

    efectoRhythmPattern0 = [(1, 0)],
    efectoSampleNPattern0 = [0],
    efectoPitchPattern0 = [0]


  }


-- 1. When to play

rhythmicPattern :: RhythmicPattern -> (Tempo -> UTCTime -> UTCTime ->  [Rational])
rhythmicPattern xs t iw ew  = Data.List.sort $ concat $ fmap (\(x, y) -> findBeats t iw ew x y) xs

samplePattern :: [(RhythmicPosition, Int)] -> Tempo -> UTCTime -> UTCTime -> [(Rational, Int)]
samplePattern xs t iw ew =  Data.List.sort $ concat $ fmap (\x -> samplePattern' x t iw ew) xs

samplePattern' :: (RhythmicPosition, Int) -> Tempo -> UTCTime -> UTCTime -> [(Rational, Int)]
samplePattern' (xs, sampleNumber) t iw ew  = fmap (\attack -> (attack, sampleNumber)) attacks
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
