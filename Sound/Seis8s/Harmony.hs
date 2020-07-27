module Sound.Seis8s.Harmony where

import Sound.Seis8s.Generic
import Sound.Seis8s.Rhythm

import Data.Time
import Data.Tempo
import Data.Maybe
import Data.List
import Data.Tuple.Select

type Relacion = String -- "segunda" "absoluto"
type Octava = Double
type Note = (Relacion, Double, Octava)
type PitchType = String -- intervalo o midinote
type PitchPattern = (PitchType, [Note])

data Chord = Chord Pitch ChordType ChordPosition deriving (Show)
type Pitch = Double
type ChordType = [Double]
-- data Harmony = Harmony Chord ChordPosition ChordPosition deriving (Show)-- :: (Rational,Rational)
data Progression = Progression Metre [Chord] deriving (Show)

generateLineFromMidi :: [(Rational, (String, Double, Double))] -> [(Rational, Pitch)]
generateLineFromMidi xs = fmap generateLineFromMidi' xs

generateLineFromMidi' :: (Rational, (String, Double, Double)) -> (Rational, Pitch)
generateLineFromMidi' (attack, (midiIdentifier, midinote, octava)) = (attack, midinote)

generateLine :: [(Rational, (String, Double, Octava))] -> Progression -> [(Rational, Pitch)]
generateLine attacksAndIntervals (Progression metre chords) =  sort $ concat $ fmap (generateNotesFromChord attacksAndIntervals metre) chords

generateNotesFromChord :: [(Rational, (String, Double, Octava))] -> Metre -> Chord -> [(Rational, Pitch)]
generateNotesFromChord attacksAndIntervals metre chord = concat $ fmap (\x -> generateSingleNoteFromChord x metre chord) attacksAndIntervals

generateSingleNoteFromChord :: (Rational, (String, Double, Octava)) -> Rational -> Chord ->  [(Rational, Pitch)]
generateSingleNoteFromChord (attack, (tipo, interval, octava)) metre (Chord p t (start, end))
  |compareRationalWChordRange attack metre (start, end) = attackAndNote attack note
  | otherwise = []
  where
    note = getNoteInChord (Chord p t (start, end)) (tipo, interval, octava) -- Maybe Pitch

attackAndNote :: Rational -> Maybe Pitch -> [(Rational, Pitch)]
attackAndNote attack (Just note) = [(attack, note)]
attackAndNote _ Nothing = []

getNoteInChord :: Chord -> (String, Double, Octava) -> Maybe Pitch
getNoteInChord (Chord root chordType (start, end)) (tipo, interval, octava)
       | tipo == "segunda" = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> (intervaloDisponible chordType (tipo, interval, octava)))
       | tipo == "tercera" = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> (intervaloDisponible chordType (tipo, interval, octava)))
       | tipo == "cuarta" = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> (intervaloDisponible chordType (tipo, interval, octava)))
       | tipo == "quinta" = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> (intervaloDisponible chordType (tipo, interval, octava)))
       | tipo == "sexta" = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> (intervaloDisponible chordType (tipo, interval, octava)))
       | tipo == "septima" = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> (intervaloDisponible chordType (tipo, interval, octava)))
       | tipo == "novena" = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> (intervaloDisponible chordType (tipo, interval, octava)))
       | tipo == "oncena" = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> (intervaloDisponible chordType (tipo, interval, octava)))
       | tipo == "trecena" = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> (intervaloDisponible chordType (tipo, interval, octava)))
       | otherwise = (+) <$> Just root <*> ((+) <$> Just (12 * octava) <*> Just interval)

 -- [Double]
intervaloDisponible :: ChordType -> (String, Double, Octava) -> Maybe Double
intervaloDisponible cht (tipo, _, _)
      |cht == major && tipo == "segunda" = Just $ sel2 (intervalo "2M" 0)
      |cht == major7 && tipo == "segunda" = Just $ sel2 (intervalo "2M" 0)
      |cht == minor && tipo == "segunda" = Nothing
      |cht == minor7 && tipo == "segunda" = Nothing
      |cht == dom && tipo == "segunda" = Just $sel2 (intervalo "2M" 0)
      |cht == sus2 && tipo == "segunda" = Just $sel2 (intervalo "2M" 0)
      |cht == sus4 && tipo == "segunda" = Nothing
      |cht == fifths && tipo == "segunda" = Just $ sel2 (intervalo "2M" 0)
      |cht == dim && tipo == "segunda" = Nothing
      |cht == aug && tipo == "segunda" = Nothing

      |cht == major && tipo == "tercera" = Just $ sel2 (intervalo "3M" 0)
      |cht == major7 && tipo == "tercera" = Just $ sel2 (intervalo "3M" 0)
      |cht == minor && tipo == "tercera" = Just $ sel2 (intervalo "3m" 0)
      |cht == minor7 && tipo == "tercera" = Just $ sel2 (intervalo "3m" 0)
      |cht == dom && tipo == "tercera" = Just $ sel2 (intervalo "3M" 0)
      |cht == sus2 && tipo == "tercera" = Nothing
      |cht == sus4 && tipo == "tercera" = Nothing
      |cht == fifths && tipo == "tercera" = Nothing
      |cht == dim && tipo == "tercera" = Just $ sel2 (intervalo "3m" 0)
      |cht == dim7 && tipo == "tercera"  = Just $ sel2 (intervalo "3m" 0)
      |cht == semidim && tipo == "tercera" = Just $ sel2 (intervalo "3m" 0)
      |cht == aug && tipo == "tercera" = Just $ sel2 (intervalo "3M" 0)

      |cht == major && tipo == "cuarta" = Just $ sel2 (intervalo "4J" 0)
      |cht == major7 && tipo == "cuarta" = Just $ sel2 (intervalo "4J" 0)
      |cht == minor && tipo == "cuarta" = Just $ sel2 (intervalo "4J" 0)
      |cht == minor7 && tipo == "cuarta" = Just $ sel2 (intervalo "4J" 0)
      |cht == dom && tipo == "cuarta" = Nothing
      |cht == sus2 && tipo == "cuarta" = Nothing -- ?
      |cht == sus4 && tipo == "cuarta" = Nothing -- ?
      |cht == fifths && tipo == "cuarta" = Nothing
      |cht == dim && tipo == "cuarta" = Just $ sel2 (intervalo "4J" 0)
      |cht == dim7 && tipo == "cuarta" = Just $ sel2 (intervalo "4J" 0)
      |cht == semidim && tipo == "cuarta" = Just $ sel2 (intervalo "4J" 0)
      |cht == aug && tipo == "cuarta" = Just $ sel2 (intervalo "4J" 0)

      |cht == major && tipo == "quinta" = Just $ sel2 (intervalo "5J" 0)
      |cht == major7 && tipo == "quinta" = Just $ sel2 (intervalo "5J" 0)
      |cht == minor && tipo == "quinta" = Just $ sel2 (intervalo "5J" 0)
      |cht == minor7 && tipo == "quinta" = Just $ sel2 (intervalo "5J" 0)
      |cht == dom && tipo == "quinta" = Just $ sel2 (intervalo "5J" 0)
      |cht == sus2 && tipo == "quinta" = Just $ sel2 (intervalo "5J" 0)
      |cht == sus4 && tipo == "quinta" = Just $ sel2 (intervalo "5J" 0)
      |cht == fifths && tipo == "quinta" = Just $ sel2 (intervalo "5J" 0)
      |cht == dim && tipo == "quinta" = Just $ sel2 (intervalo "5b" 0)
      |cht == dim7 && tipo == "quinta" =  Just $ sel2 (intervalo "5b" 0)
      |cht == semidim && tipo == "quinta" =  Just $ sel2 (intervalo "5b" 0)
      |cht == aug && tipo == "quinta" =  Just $ sel2 (intervalo "5#" 0)

      |cht == major && tipo == "sexta" = Just $ sel2 (intervalo "6M" 0)
      |cht == major7 && tipo == "sexta" = Just $ sel2 (intervalo "6M" 0)
      |cht == minor && tipo == "sexta" = Nothing
      |cht == minor7 && tipo == "sexta" = Nothing
      |cht == dom && tipo == "sexta" = Just $ sel2 (intervalo "6M" 0)
      |cht == sus2 && tipo == "sexta" = Nothing -- ?
      |cht == sus4 && tipo == "sexta" = Nothing -- ?
      |cht == fifths && tipo == "sexta" = Nothing
      |cht == dim && tipo == "sexta" = Just $ sel2 (intervalo "6b" 0)
      |cht == dim7 && tipo == "sexta" = Just $ sel2 (intervalo "6b" 0)
      |cht == semidim && tipo == "sexta" = Just $ sel2 (intervalo "6b" 0)
      |cht == aug && tipo == "sexta" = Nothing

      |cht == major && tipo == "septima" = Just $ sel2 (intervalo "7M" 0)
      |cht == major7 && tipo == "septima" = Just $ sel2 (intervalo "7M" 0)
      |cht == minor && tipo == "septima" = Just $ sel2 (intervalo "7m" 0)
      |cht == minor7 && tipo == "septima" = Just $ sel2 (intervalo "7m" 0)
      |cht == dom && tipo == "septima" = Just $ sel2 (intervalo "7m" 0)
      |cht == sus2 && tipo == "septima" = Nothing -- ?
      |cht == sus4 && tipo == "septima" = Nothing -- ?
      |cht == fifths && tipo == "septima" = Nothing
      |cht == dim && tipo == "septima" = Just $ sel2 (intervalo "7bb" 0) -- m7b5
      |cht == dim7 && tipo == "septima"  = Just $ sel2 (intervalo "7bb" 0)
      |cht == semidim && tipo == "septima" = Just $ sel2 (intervalo "7m" 0)
      |cht == aug && tipo == "septima" = Just $ sel2 (intervalo "7m" 0)

      |cht == major && tipo == "novena" = Just $ sel2 (intervalo "9M" 0)
      |cht == major7 && tipo == "novena" = Just $ sel2 (intervalo "9M" 0)
      |cht == minor && tipo == "novena" = Just $ sel2 (intervalo "9M" 0)
      |cht == minor7 && tipo == "novena" = Just $ sel2 (intervalo "9M" 0)
      |cht == dom && tipo == "novena" = Just $ sel2 (intervalo "9M" 0)
      |cht == sus2 && tipo == "novena" = Just $ sel2 (intervalo "9M" 0) -- ?
      |cht == sus4 && tipo == "novena" = Just $ sel2 (intervalo "9M" 0) -- ?
      |cht == fifths && tipo == "novena" = Just $ sel2 (intervalo "9M" 0)
      |cht == dim && tipo == "novena" = Just $ sel2 (intervalo "9M" 0) -- m7b5
      |cht == dim7 && tipo == "novena"  = Just $ sel2 (intervalo "9M" 0)
      |cht == semidim && tipo == "novena" = Just $ sel2 (intervalo "9M" 0)
      |cht == aug && tipo == "novena" = Just $ sel2 (intervalo "9M" 0)

      |cht == major && tipo == "oncena" = Just $ sel2 (intervalo "11#" 0)
      |cht == major7 && tipo == "oncena" = Just $ sel2 (intervalo "11#" 0)
      |cht == minor && tipo == "oncena" = Just $ sel2 (intervalo "11J" 0)
      |cht == minor7 && tipo == "oncena" = Just $ sel2 (intervalo "11J" 0)
      |cht == dom && tipo == "oncena" = Just $ sel2 (intervalo "11#" 0)
      |cht == sus2 && tipo == "oncena" = Just $ sel2 (intervalo "11b" 0)
      |cht == sus4 && tipo == "oncena" = Just $ sel2 (intervalo "11b" 0)
      |cht == fifths && tipo == "oncena" = Just $ sel2 (intervalo "11J" 0)-- ?
      |cht == dim && tipo == "oncena" = Just $ sel2 (intervalo "11J" 0)-- m7b5
      |cht == dim7 && tipo == "oncena"  = Just $ sel2 (intervalo "11J" 0)
      |cht == semidim && tipo == "oncena" = Just $ sel2 (intervalo "11J" 0)
      |cht == aug && tipo == "oncena" = Just $ sel2 (intervalo "11#" 0)

      |cht == major && tipo == "trecena" = Just $ sel2 (intervalo "13M" 0)
      |cht == major7 && tipo == "trecena" = Just $ sel2 (intervalo "13M" 0)
      |cht == minor && tipo == "trecena" = Nothing
      |cht == minor7 && tipo == "trecena" = Nothing
      |cht == dom && tipo == "trecena" = Just $ sel2 (intervalo "13b" 0)
      |cht == sus2 && tipo == "trecena" = Nothing
      |cht == sus4 && tipo == "trecena" = Just $ sel2 (intervalo "13b" 0)
      |cht == fifths && tipo == "trecena" = Nothing -- ?
      |cht == dim && tipo == "trecena" = Just $ sel2 (intervalo "13b" 0)-- m7b5
      |cht == dim7 && tipo == "trecena"  = Just $ sel2 (intervalo "13b" 0)
      |cht == semidim && tipo == "trecena" = Just $ sel2 (intervalo "13b" 0)
      |cht == aug && tipo == "trecena" = Nothing

      |otherwise = Nothing

  -- (!!) (generateChord (Chord root chordType)) (round interval)

-- generates a list of chords according to a given harmony and a given rhythm pattern
generatechords :: [Rational] -> Progression ->  [(Rational, [Pitch])]
generatechords attacks (Progression metre chords) = concat $ fmap (generateChordsFromRational (Progression metre chords)) attacks

generateChordsFromRational :: Progression -> Rational -> [(Rational, [Pitch])]
generateChordsFromRational (Progression metre chords) attack   = concat $ fmap (generateSingleChordFromRational metre attack) chords

generateSingleChordFromRational :: Rational -> Metre -> Chord -> [(Rational, [Pitch])]
generateSingleChordFromRational attack metre (Chord p chordType (start, end))
  | compareRationalWChordRange attack metre (start, end) = [(attack, generateChord (Chord p chordType (start, end)))]
  | otherwise = []

-- return a list of pitches from one chord
generateChord :: Chord -> [Pitch]
generateChord (Chord root chordType (start, end))  = fmap ((+) root) chordType

compareRationalWChordRange :: Rational -> Metre -> ChordPosition -> Bool
compareRationalWChordRange attack metre (startOffset, endOffset) = do
  let attackInMetre = attack / metre
  let attackInMetre' = fract attackInMetre
  let startInMetre = startOffset / metre
  let endInMetre = endOffset / metre
  let b | startOffset <= endOffset = (attackInMetre' >= startInMetre) && (attackInMetre' < endInMetre)
        | otherwise = (attackInMetre' < startInMetre) && (attackInMetre' >= endInMetre)
  b


concatChord :: (Rational, [Pitch]) -> [(Rational, Pitch)]
concatChord (attack, ps) =  fmap (\p -> (attack, p)) (snd (attack, ps))

concatChords :: [(Rational, [Pitch])] -> [(Rational, Pitch)] -- eg. for [60, 64, 67] three events all whith the same time all with different pitches
concatChords attackAndChords = concat $ fmap (\x -> concatChord x) attackAndChords


-- return a list of lists of pitches from a list of Chords
-- generateChords :: [Chord] -> [[Pitch]]
-- generateChords cs = fmap (\x -> generateChord x) cs


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

-- opcion 1: funcion octava
-- octavar -1 (intervalo "5a") -- returns ("quinta", (-1*12) (snd intervalo "5a"))
-- entonces => octavar (intervalo "5a", "4a", "3a") que seria equivalente a intervalo "5a" + 1, etc...
-- opcion 2: incorporarlo en la funcion intervalo
--intervalo "t" -1

-- una lista de intervalos
intervaloDouble :: Double -> Double -> (String, Double, Octava)
intervaloDouble intervalo octava = ("libre", intervalo, octava)

intervalo :: String -> Double -> (String, Double, Octava)
intervalo "unisono" octava = ("unisono", 0, octava)

intervalo "2a" octava = ("segunda", 0, octava)

intervalo "2m" octava = ("segundaMenor", 1, octava)

intervalo "2M" octava = ("segundaMayor", 2,  octava)

intervalo "3a" octava = ("tercera", 0, octava)

intervalo "3m" octava = ("terceraMenor", 3, octava)

intervalo "3M" octava = ("terceraMayor", 4, octava)

intervalo "4a" octava = ("cuarta", 0, octava)

intervalo "4J" octava = ("cuartaJusta", 5, octava)

intervalo "4#" octava = ("cuartaAug", 6, octava)

intervalo "5a" octava = ("quinta", 0, octava)

intervalo "5b" octava = ("quintaBemol", 6, octava)

intervalo "5J" octava = ("quintaJusta", 7, octava)

intervalo "5#" octava = ("quintaAug", 8, octava)

intervalo "6a" octava = ("sexta", 0, octava)

intervalo "6m" octava = ("sextaMenor", 8, octava)

intervalo "6M" octava = ("sextaMayor", 9, octava)

intervalo "7a" octava = ("septima", 0, octava)

intervalo "7m" octava = ("septimaMenor", 10, octava)

intervalo "7M" octava = ("septimaMayor", 11, octava)

intervalo "8a" octava = ("octava", 12, octava)

intervalo "9a" octava = ("novena", 0, octava)

intervalo "9m" octava = ("novenaMenor", 13, octava)

intervalo "9M" octava = ("novenaMayor", 14, octava)

intervalo "11a" octava = ("oncena",  0, octava)

intervalo "11b" octava = ("oncenaBemol", 16, octava)

intervalo "11J" octava = ("oncenaJusta", 17, octava)

intervalo "11#" octava = ("oncenaAug", 18, octava)

intervalo "13a" octava = ("trecena", 0, octava)

intervalo "13b" octava = ("trecenaBemol", 20, octava)

intervalo "13M" octava = ("trecenaMayor", 21, octava)

intervalo _ octava = ("nada", 0, octava)
