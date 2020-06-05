module Sound.Cumbia.Harmony where

import Sound.Cumbia.Generic
import Sound.Cumbia.Rhythm

import Data.Maybe
import Data.Time
import Data.Tempo
import Data.Maybe
import Data.List

data Chord = Chord Pitch ChordType deriving (Show)
type Pitch = Double
type ChordType = [Double]
data Harmony = Harmony Chord RhythmicPosition RhythmicPosition deriving (Show)-- :: (Rational,Rational)
type Progression = [Harmony]

generateLine' :: [(Rational, (String, Double))] -> [(Rational, Pitch)]
generateLine' xs = fmap generateLine'' xs

generateLine'' :: (Rational, (String, Double)) -> (Rational, Pitch)
generateLine'' (attack, (midiIdentifier, midinote)) = (attack, midinote)

generateLine :: [(Rational, (String, Double))] -> [Harmony] -> [(Rational, Pitch)]
generateLine attacksAndIntervals chords =  concat $ fmap (generateNotesFromChord attacksAndIntervals) chords

generateNotesFromChord :: [(Rational, (String, Double))] -> Harmony -> [(Rational, Pitch)]
generateNotesFromChord attacksAndIntervals chord = concat $ fmap (\x -> generateSingleNoteFromChord x chord) attacksAndIntervals

getNoteInChord :: Chord -> (String, Double) -> Maybe Pitch
getNoteInChord (Chord root chordType) (tipo, interval)
       | tipo == "segunda" = (+) <$> Just root <*> (intervaloDisponible chordType (tipo, interval))
       | tipo == "tercera" = (+) <$> Just root <*> (intervaloDisponible chordType (tipo, interval))
       | tipo == "cuarta" = (+) <$> Just root <*> (intervaloDisponible chordType (tipo, interval))
       | tipo == "quinta" = (+) <$> Just root <*> (intervaloDisponible chordType (tipo, interval))
       | tipo == "sexta" = (+) <$> Just root <*> (intervaloDisponible chordType (tipo, interval))
       | tipo == "septima" = (+) <$> Just root <*> (intervaloDisponible chordType (tipo, interval))
       | tipo == "novena" = (+) <$> Just root <*> (intervaloDisponible chordType (tipo, interval))
       | tipo == "oncena" = (+) <$> Just root <*> (intervaloDisponible chordType (tipo, interval))
       | tipo == "trecena" = (+) <$> Just root <*> (intervaloDisponible chordType (tipo, interval))
       | otherwise = (+) <$> Just root <*> Just interval

 -- [Double]
intervaloDisponible :: ChordType -> (String, Double) -> Maybe Double
intervaloDisponible cht (tipo, _)
      |cht == major && tipo == "segunda" = Just $ snd (intervalo "2M")
      |cht == major7 && tipo == "segunda" = Just $ snd (intervalo "2M")
      |cht == minor && tipo == "segunda" = Nothing
      |cht == minor7 && tipo == "segunda" = Nothing
      |cht == dom && tipo == "segunda" = Just $snd (intervalo "2M")
      |cht == sus2 && tipo == "segunda" = Just $snd (intervalo "2M")
      |cht == sus4 && tipo == "segunda" = Nothing
      |cht == fifths && tipo == "segunda" = Just $ snd (intervalo "2M")
      |cht == dim && tipo == "segunda" = Nothing
      |cht == aug && tipo == "segunda" = Nothing

      |cht == major && tipo == "tercera" = Just $ snd (intervalo "3M")
      |cht == major7 && tipo == "tercera" = Just $ snd (intervalo "3M")
      |cht == minor && tipo == "tercera" = Just $ snd (intervalo "3M")
      |cht == minor7 && tipo == "tercera" = Just $ snd (intervalo "3m")
      |cht == dom && tipo == "tercera" = Just $ snd (intervalo "3M")
      |cht == sus2 && tipo == "tercera" = Nothing
      |cht == sus4 && tipo == "tercera" = Nothing
      |cht == fifths && tipo == "tercera" = Nothing
      |cht == dim && tipo == "tercera" = Just $ snd (intervalo "3m")
      |cht == dim7 && tipo == "tercera"  = Just $ snd (intervalo "3m")
      |cht == semidim && tipo == "tercera" = Just $ snd (intervalo "3m")
      |cht == aug && tipo == "tercera" = Just $ snd (intervalo "3M")

      |cht == major && tipo == "cuarta" = Just $ snd (intervalo "4J")
      |cht == major7 && tipo == "cuarta" = Just $ snd (intervalo "4J")
      |cht == minor && tipo == "cuarta" = Just $ snd (intervalo "4J")
      |cht == minor7 && tipo == "cuarta" = Just $ snd (intervalo "4J")
      |cht == dom && tipo == "cuarta" = Nothing
      |cht == sus2 && tipo == "cuarta" = Nothing -- ?
      |cht == sus4 && tipo == "cuarta" = Nothing -- ?
      |cht == fifths && tipo == "cuarta" = Nothing
      |cht == dim && tipo == "cuarta" = Just $ snd (intervalo "4J")
      |cht == dim7 && tipo == "cuarta" = Just $ snd (intervalo "4J")
      |cht == semidim && tipo == "cuarta" = Just $ snd (intervalo "4J")
      |cht == aug && tipo == "cuarta" = Just $ snd (intervalo "4J")

      |cht == major && tipo == "quinta" = Just $ snd (intervalo "5J")
      |cht == major7 && tipo == "quinta" = Just $ snd (intervalo "5J")
      |cht == minor && tipo == "quinta" = Just $ snd (intervalo "5J")
      |cht == minor7 && tipo == "quinta" = Just $ snd (intervalo "5J")
      |cht == dom && tipo == "quinta" = Just $ snd (intervalo "5J")
      |cht == sus2 && tipo == "quinta" = Just $ snd (intervalo "5J")
      |cht == sus4 && tipo == "quinta" = Just $ snd (intervalo "5J")
      |cht == fifths && tipo == "quinta" = Just $ snd (intervalo "5J")
      |cht == dim && tipo == "quinta" = Just $ snd (intervalo "5b")
      |cht == dim7 && tipo == "quinta" =  Just $ snd (intervalo "5b")
      |cht == semidim && tipo == "quinta" =  Just $ snd (intervalo "5b")
      |cht == aug && tipo == "quinta" =  Just $ snd (intervalo "5#")

      |cht == major && tipo == "sexta" = Just $ snd (intervalo "6M")
      |cht == major7 && tipo == "sexta" = Just $ snd (intervalo "6M")
      |cht == minor && tipo == "sexta" = Nothing
      |cht == minor7 && tipo == "sexta" = Nothing
      |cht == dom && tipo == "sexta" = Just $ snd (intervalo "6M")
      |cht == sus2 && tipo == "sexta" = Nothing -- ?
      |cht == sus4 && tipo == "sexta" = Nothing -- ?
      |cht == fifths && tipo == "sexta" = Nothing
      |cht == dim && tipo == "sexta" = Just $ snd (intervalo "6b")
      |cht == dim7 && tipo == "sexta" = Just $ snd (intervalo "6b")
      |cht == semidim && tipo == "sexta" = Just $ snd (intervalo "6b")
      |cht == aug && tipo == "sexta" = Nothing

      |cht == major && tipo == "septima" = Just $ snd (intervalo "7M")
      |cht == major7 && tipo == "septima" = Just $ snd (intervalo "7M")
      |cht == minor && tipo == "septima" = Just $ snd (intervalo "7m")
      |cht == minor7 && tipo == "septima" = Just $ snd (intervalo "7m")
      |cht == dom && tipo == "septima" = Just $ snd (intervalo "7m")
      |cht == sus2 && tipo == "septima" = Nothing -- ?
      |cht == sus4 && tipo == "septima" = Nothing -- ?
      |cht == fifths && tipo == "septima" = Nothing
      |cht == dim && tipo == "septima" = Just $ snd (intervalo "7bb") -- m7b5
      |cht == dim7 && tipo == "septima"  = Just $ snd (intervalo "7bb")
      |cht == semidim && tipo == "septima" = Just $ snd (intervalo "7m")
      |cht == aug && tipo == "septima" = Just $ snd (intervalo "7m")

      |cht == major && tipo == "novena" = Just $ snd (intervalo "9M")
      |cht == major7 && tipo == "novena" = Just $ snd (intervalo "9M")
      |cht == minor && tipo == "novena" = Just $ snd (intervalo "9M")
      |cht == minor7 && tipo == "novena" = Just $ snd (intervalo "9M")
      |cht == dom && tipo == "novena" = Just $ snd (intervalo "9M")
      |cht == sus2 && tipo == "novena" = Just $ snd (intervalo "9M") -- ?
      |cht == sus4 && tipo == "novena" = Just $ snd (intervalo "9M") -- ?
      |cht == fifths && tipo == "novena" = Just $ snd (intervalo "9M")
      |cht == dim && tipo == "novena" = Just $ snd (intervalo "9M") -- m7b5
      |cht == dim7 && tipo == "novena"  = Just $ snd (intervalo "9M")
      |cht == semidim && tipo == "novena" = Just $ snd (intervalo "9M")
      |cht == aug && tipo == "novena" = Just $ snd (intervalo "9M")

      |cht == major && tipo == "oncena" = Just $ snd (intervalo "11#")
      |cht == major7 && tipo == "oncena" = Just $ snd (intervalo "11#")
      |cht == minor && tipo == "oncena" = Just $ snd (intervalo "11J")
      |cht == minor7 && tipo == "oncena" = Just $ snd (intervalo "11J")
      |cht == dom && tipo == "oncena" = Just $ snd (intervalo "11#")
      |cht == sus2 && tipo == "oncena" = Just $ snd (intervalo "11b")
      |cht == sus4 && tipo == "oncena" = Just $ snd (intervalo "11b")
      |cht == fifths && tipo == "oncena" = Just $ snd (intervalo "11J") -- ?
      |cht == dim && tipo == "oncena" = Just $ snd (intervalo "11J") -- m7b5
      |cht == dim7 && tipo == "oncena"  = Just $ snd (intervalo "11J")
      |cht == semidim && tipo == "oncena" = Just $ snd (intervalo "11J")
      |cht == aug && tipo == "oncena" = Just $ snd (intervalo "11#")

      |cht == major && tipo == "trecena" = Just $ snd (intervalo "13M" )
      |cht == major7 && tipo == "trecena" = Just $ snd (intervalo "13M")
      |cht == minor && tipo == "trecena" = Nothing
      |cht == minor7 && tipo == "trecena" = Nothing
      |cht == dom && tipo == "trecena" = Just $ snd (intervalo "13b")
      |cht == sus2 && tipo == "trecena" = Nothing
      |cht == sus4 && tipo == "trecena" = Just $ snd (intervalo "13b")
      |cht == fifths && tipo == "trecena" = Nothing -- ?
      |cht == dim && tipo == "trecena" = Just $ snd (intervalo "13b") -- m7b5
      |cht == dim7 && tipo == "trecena"  = Just $ snd (intervalo "13b")
      |cht == semidim && tipo == "trecena" = Just $ snd (intervalo "13b")
      |cht == aug && tipo == "trecena" = Nothing

      |otherwise = Nothing

  -- (!!) (generateChord (Chord root chordType)) (round interval)

generateSingleNoteFromChord :: (Rational, (String, Double)) -> Harmony ->  [(Rational, Pitch)]
generateSingleNoteFromChord (attack, (tipo, interval)) (Harmony c start end)
  |compareRationalWChordRange attack start end = attackAndNote attack note
  | otherwise = []
  where
    note = getNoteInChord c (tipo, interval) -- Maybe Pitch

attackAndNote :: Rational -> Maybe Pitch -> [(Rational, Pitch)]
attackAndNote attack (Just note) = [(attack, note)]
attackAndNote _ Nothing = []

-- generates a list of chords according to a given harmony and a given rhythm pattern
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


-- pitches

c :: Double
c = 60

cs :: Double
cs = 61

d :: Double
d = 62

ds :: Double
ds = 63

e :: Double
e = 64

f :: Double
f = 65

fs :: Double
fs = 66

g :: Double
g = 67

gs :: Double
gs = 68

a :: Double
a = 69

as :: Double
as = 70

b :: Double
b = 71

-- intervalos para generar acordes

major :: ChordType
major = [0, 4, 7]

minor:: ChordType
minor = [0, 3, 7]

major7 :: ChordType
major7 = [0, 4, 7, 11]

minor7 :: ChordType
minor7 = [0,3,7,10]

dom :: ChordType
dom = [0,4,7,10]

fifths :: ChordType
fifths = [0, 7]

sus4 :: ChordType
sus4 = [0,5,7]

sus2 :: ChordType
sus2 = [0, 2, 7]

aug :: ChordType
aug = [0, 4, 8]

dim :: ChordType
dim = [0, 3, 6]

semidim :: ChordType --m7b5
semidim = [0, 3, 6, 10]

dim7 :: ChordType
dim7 = [0, 3, 6, 9]

-- una lista de intervalos
intervalo :: String -> (String, Double)
intervalo "unisono" = ("unisono", 0)

intervalo "2a" = ("segunda", 0)

intervalo "2m" = ("segundaMenor",  1)

intervalo "2M" = ("segundaMayor",  2)

intervalo "3a" = ("tercera", 0)

intervalo "3m" = ("terceraMenor",  3)

intervalo "3M" = ("terceraMayor",  4)

intervalo "4a" = ("cuarta", 0)

intervalo "4J" = ("cuartaJusta",  5)

intervalo "4#" = ("cuartaAug",  6)

intervalo "5a" = ("quinta", 0)

intervalo "5b" = ("quintaBemol",  6)

intervalo "5J" = ("quintaJusta",  7)

intervalo "5#" = ("quintaAug",  8)

intervalo "6a" = ("sexta", 0)

intervalo "6m" = ("sextaMenor",  8)

intervalo "6M" = ("sextaMayor",  9)

intervalo "7a" = ("septima", 0)

intervalo "7m" = ("septimaMenor",  10)

intervalo "7M" = ("septimaMayor",  11)

intervalo "8a" = ("octava",  12)

intervalo "9a" = ("novena", 0)

intervalo "9m" = ("novenaMenor",  13)

intervalo "9M" = ("novenaMayor",  14)

intervalo "11a" = ("oncena", 0)

intervalo "11b" = ("oncenaBemol",  16)

intervalo "11J" = ("oncenaJusta",  17)

intervalo "11#" = ("oncenaAug",  18)

intervalo "13a" = ("trecena", 0)

intervalo "13b" = ("trecenaBemol",  20)

intervalo "13M" = ("trecenaMayor",  21)

intervalo _ = ("nada", 0)
