module Sound.Seis8s.Harmony where

import Sound.Seis8s.Generic
import Sound.Seis8s.Rhythm


import Data.Time
import Data.Tempo
import Data.Maybe
import Data.Function
import Data.List as List
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

generateSingleNoteFromChord :: (Rational, (String, Double, Octava)) -> Metre -> Chord ->  [(Rational, Pitch)]
generateSingleNoteFromChord (attack, (tipo, interval, octava)) metre (Chord p t (start, end))
  |compareRationalWChordRange attack metre (start, end) = attackAndNote attack note
  | otherwise = []
  where
    note = getNoteInChord (Chord p t (start, end)) (tipo, interval, octava) -- Maybe Pitch

attackAndNote :: Rational -> Maybe Pitch -> [(Rational, Pitch)]
attackAndNote attack (Just note) = [(attack, note)]
attackAndNote _ Nothing = []

-- functions to pick a chord that has a voicing

pickChords' :: [Rational] -> Progression -> [(String, Double, Octava)] -> [(Rational, [Pitch])]
pickChords' attacks (Progression metre chords) pitchPattern = concat $ fmap (\a -> pickChordsWithVoicingFromRational' (Progression metre chords) a pitchPattern) attacks 

pickChordsWithVoicingFromRational' :: Progression -> Rational -> [(String, Double, Octava)] -> [(Rational, [Pitch])]
pickChordsWithVoicingFromRational' (Progression metre chords) attack pitchPattern = do
   let acordes = acordesConVoicing $ getNotesInChords (Progression metre chords) pitchPattern--[([Pitch], (Rational, Rational))]
   concat $ fmap (pickSingleChordFromRational' attack metre) acordes

pickSingleChordFromRational' :: Rational -> Metre -> ([Pitch], (Rational, Rational)) -> [(Rational, [Pitch])]
pickSingleChordFromRational' attack metre (acorde, (start, end))
  | compareRationalWChordRange attack metre (start, end) = [(attack, acorde)]
  | otherwise = []

getNotesInChords :: Progression -> [(String, Double, Octava)] -> [([Pitch], (Start, End))]
getNotesInChords (Progression metre chords) intervalos = fmap (\c -> getNotesInChord c intervalos) chords

getNotesInChord :: Chord -> [(String, Double, Octava)] -> ([Pitch], (Start, End))
getNotesInChord (Chord root chordType (start, end)) intervalos =  do
  let acorde = catMaybes $ fmap (\i -> getNoteInChord (Chord root chordType (start, end)) i) intervalos
  (acorde, (start, end))

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
-- generatechords :: [Rational] -> Progression ->  [(Rational, [Pitch])]
generatechords :: [Rational] -> Progression ->  [(Rational, [Pitch])]
generatechords attacks (Progression metre chords) = concat $ fmap (generateChordsFromRational (Progression metre chords)) attacks

generateChordsFromRational :: Progression -> Rational -> [(Rational, [Pitch])]
generateChordsFromRational (Progression metre chords) attack   = concat $ fmap (generateSingleChordFromRational attack metre) chords

generateSingleChordFromRational :: Rational -> Metre -> Chord -> [(Rational, [Pitch])]
generateSingleChordFromRational attack metre (Chord p chordType (start, end))
  | compareRationalWChordRange attack metre (start, end) = [(attack, generateChord (Chord p chordType (start, end)))]
  | otherwise = []

-- return a list of pitches from one chord
generateChord :: Chord -> [Pitch]
generateChord (Chord root chordType (start, end))  = fmap ((+) root) chordType

pickChords :: [Rational] -> Progression ->  [(Rational, [Pitch])]
pickChords attacks (Progression metre chords) = concat $ fmap (pickChordsWithVoicingFromRational (Progression metre chords)) attacks

pickChordsWithVoicingFromRational :: Progression -> Rational -> [(Rational, [Pitch])]
pickChordsWithVoicingFromRational (Progression metre chords) attack = do
   let acordes = acordesConVoicing $ generateChordsFromProg (Progression metre chords) --[([Pitch], (Rational, Rational))]
   concat $ fmap (pickSingleChordFromRational attack metre) acordes

pickSingleChordFromRational :: Rational -> Metre -> ([Pitch], (Rational, Rational)) -> [(Rational, [Pitch])]
pickSingleChordFromRational attack metre (acorde, (start, end))
  | compareRationalWChordRange attack metre (start, end) = [(attack, acorde)]
  | otherwise = []

generateChordsFromProg :: Progression -> [([Pitch], (Rational, Rational))]
generateChordsFromProg (Progression metre chords)  = fmap (\c -> generateChordFromProg c) chords

generateChordFromProg :: Chord -> ([Pitch], (Rational, Rational))
generateChordFromProg (Chord root chordType (start, end))  = do
  let acorde = fmap ((+) root) chordType
  (acorde, (start,end))

compareRationalWChordRange :: Rational -> Metre -> ChordPosition -> Bool
compareRationalWChordRange attack metre (startOffset, endOffset) = do
  let attackInMetre = attack / metre
  let attackInMetre' = fract attackInMetre
  let startInMetre = startOffset / metre
  let endInMetre = endOffset / metre
  let b | startOffset <= endOffset = (attackInMetre' >= startInMetre) && (attackInMetre' < endInMetre)
        | otherwise = (attackInMetre' < startInMetre) && (attackInMetre' >= endInMetre)
  b
-- armonia 1 [c 0 5] => Progression 0.5 [Chord 60 major (0, 0.25)]
-- attack 1 es 0.5
-- compas gmm = compas "partido" (i.e. 0.5)
-- por lo tanto compareRationalWChordRange' 0.5 0.5 0.5 (0, 0.25)
-- compareRationalWChordRange' 1 1 1 (0, 0.5)
-- not in use
compareRationalWChordRange' :: Rational -> Metre -> Double -> ChordPosition -> Bool
compareRationalWChordRange' attack metre compas (startOffset, endOffset) = do
  let attack' = attack / toRational compas
  let metre' = metre / toRational compas
  let startOffset' = startOffset / toRational compas
  let endOffset' = endOffset / toRational compas
  let attackInMetre = attack' / metre' -- 1/1 = 1
  let attackInMetre' = fract attackInMetre -- 1.0 -> 0
  let startInMetre = startOffset' / metre' -- 0/1 = 0
  let endInMetre = endOffset' / metre' -- 0.5/1 = 0.5
  let b | startOffset' <= endOffset' = (attackInMetre' >= startInMetre) && (attackInMetre' < endInMetre)
        | otherwise = (attackInMetre' < startInMetre) && (attackInMetre' >= endInMetre)
  b


concatChord :: (Rational, [Pitch]) -> [(Rational, Pitch)]
concatChord (attack, ps) =  fmap (\p -> (attack, p)) (snd (attack, ps))

concatChords :: [(Rational, [Pitch])] -> [(Rational, Pitch)] -- eg. for [60, 64, 67] three events all whith the same time all with different pitches
concatChords attackAndChords = concat $ fmap (\x -> concatChord x) attackAndChords


-- a simple voicing

c1 = Chord 60 major (0, 0.5)
c2 = Chord 64 minor (0.5, 1)

--generateLine xs 1 myharmony = [(9 % 20,60.0),(9 % 20,64.0),(9 % 20,67.0)]
agruparNotas :: [(Rational, Pitch)] -> [[(Rational, Pitch)]]
agruparNotas xs = groupBy ((==) `on` fst) xs
-- 1. se recibe como [(Rational, Pitch)]
-- 2. lo transformamos a [[(Rational, Pitch)]]
-- 3. lo regresamos a  [(Rational, Pitch)]
-- [(0, 59), (0, 62), (0, 67), (0.5, 60), (0.5, 64), (0.5, 67)]
-- generateChordsFromProg :: Progression -> [([Pitch], (Rational, Rational))]
--
--
acordesConVoicing :: [([Pitch], (Rational, Rational))] -> [([Pitch], (Rational, Rational))]
acordesConVoicing prog
  | length prog == 0 = []
  | length prog == 1 =  prog
  | length prog == 2 =  [a, ab]
  | length prog == 3 =  [a, ab, bc]
  | length prog == 4 =  [a, ab, bc, cd]
  | length prog == 5 =  [a, ab, bc, cd, de]
  | length prog == 6 =  [a, ab, bc, cd, de, ef]
  | length prog == 7 =  [a, ab, bc, cd, de, ef, fg]
  | length prog == 8 =  [a, ab, bc, cd, de, ef, fg, gh]
  | length prog == 9 =  [a, ab, bc, cd, de, ef, fg, gh, hi]
  | length prog == 10 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij]
  | length prog == 11 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk]
  | length prog == 12 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl]
  | length prog == 13 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm]
  | length prog == 14 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn]
  | length prog == 15 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no]
  | length prog == 16 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op]
  | length prog == 17 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq]
  | length prog == 18 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr]
  | length prog == 19 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs]
  | length prog == 20 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st]
  | length prog == 21 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu]
  | length prog == 22 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv]
  | length prog == 23 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw]
  | length prog == 24 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw, wx]
  | length prog == 25 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw, wx, xy]
  | length prog == 26 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw, wx, xy, yz]
  | length prog == 27 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw, wx, xy, yz, za]
  | length prog == 28 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw, wx, xy, yz, za, ab']
  | length prog == 29 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw, wx, xy, yz, za, ab', bc']
  | length prog == 30 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw, wx, xy, yz, za, ab', bc', cd']
  | length prog == 31 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw, wx, xy, yz, za, ab', bc', cd', de']
  | length prog == 32 =  [a, ab, bc, cd, de, ef, fg, gh, hi, ij, jk, kl, lm, mn, no, op, pq, qr, rs, st, tu, uv, vw, wx, xy, yz, za, ab', bc', cd', de', ef']
    where
      a = (!!) prog 0
      ab = acordeConVoicing a ((!!) prog 1)
      bc = acordeConVoicing ab ((!!) prog 2)
      cd = acordeConVoicing bc ((!!) prog 3)
      de = acordeConVoicing cd ((!!) prog 4)
      ef = acordeConVoicing de ((!!) prog 5)
      fg = acordeConVoicing ef ((!!) prog 6)
      gh = acordeConVoicing fg ((!!) prog 7)
      hi = acordeConVoicing gh ((!!) prog 8)
      ij = acordeConVoicing hi ((!!) prog 9)
      jk = acordeConVoicing ij ((!!) prog 10)
      kl = acordeConVoicing jk ((!!) prog 11)
      lm = acordeConVoicing kl ((!!) prog 12)
      mn = acordeConVoicing lm ((!!) prog 13)
      no = acordeConVoicing mn ((!!) prog 14)
      op = acordeConVoicing no ((!!) prog 15)
      pq = acordeConVoicing op ((!!) prog 16)
      qr = acordeConVoicing pq ((!!) prog 17)
      rs = acordeConVoicing qr ((!!) prog 18)
      st = acordeConVoicing rs ((!!) prog 19)
      tu = acordeConVoicing st ((!!) prog 20)
      uv = acordeConVoicing tu ((!!) prog 21)
      vw = acordeConVoicing uv ((!!) prog 22)
      wx = acordeConVoicing vw ((!!) prog 23)
      xy = acordeConVoicing wx ((!!) prog 24)
      yz = acordeConVoicing xy ((!!) prog 25)
      za = acordeConVoicing yz ((!!) prog 26)
      ab' = acordeConVoicing za ((!!) prog 27)
      bc' = acordeConVoicing ab' ((!!) prog 28)
      cd' = acordeConVoicing bc' ((!!) prog 29)
      de' = acordeConVoicing cd' ((!!) prog 30)
      ef' = acordeConVoicing de' ((!!) prog 31)
--
-- -- [(9 % 20,60.0),(9 % 20,64.0),(9 % 20,67.0)]
-- -- [([Pitch], (Rational, Rational))] -- ([60, 64, 67], (0, 1))

acordeConVoicing :: ([Pitch], (Rational, Rational)) -> ([Pitch], (Rational, Rational)) -> ([Pitch], (Rational, Rational))
acordeConVoicing prev sig = do
  let notasComunesPrev = notasComunes (fst prev) (fst sig) -- [(0, 60), (0, 64), (0, 67)] [(0.25, 64), (0.25, 67), (0.25, 71)] = [(0.25, 64), (0.25, 67)]
  let notasComunesSig = notasComunes (fst sig) (fst prev)
  let notasNoComunesPrev = notasNoComunes' (fst prev) notasComunesPrev -- [(0, 60)]
  let notasNoComunesSig = notasNoComunes' (fst sig) notasComunesSig -- [(0.5, 71)]
  let dists = distancias'' notasNoComunesPrev notasNoComunesSig -- [(Double, Pitch)]
  let notasCercanas' = notasCercanas (length notasNoComunesSig) dists -- [71] -- [Pitch]
  let listaFinal = List.sort $ notasComunesPrev ++ notasCercanas'
  (listaFinal, snd sig)


-- notasNoComunes' :: [(Rational, Pitch)] -> [(Rational, Pitch)] -> [(Rational, Pitch)]
notasNoComunes' :: [Pitch] -> [Pitch] -> [Pitch]
notasNoComunes' xs ys = catMaybes $ fmap (\x -> notasNoComunes x ys) xs

-- [([Pitch], (Rational, Rational))]
-- notasNoComunes :: (Rational, Pitch) -> [(Rational, Pitch)] -> Maybe (Rational, Pitch)
notasNoComunes :: Pitch -> [Pitch] -> Maybe Pitch
notasNoComunes x xs
  | elem x xs = Nothing
  | otherwise = Just x

-- 71 [64, 67, 71] [64, 67]

-- 1. encontrar las notas en comun entre ambas listas
-- notasComunes :: [Pitch] -> [Pitch] -> [Pitch]
notasComunes :: [Pitch] -> [Pitch] -> [Pitch]
notasComunes prev sig = concat $ fmap (\p -> notaComun' p sig) prev

-- notaComun' :: (Rational, Pitch) -> [(Rational, Pitch)] -> [(Rational, Pitch)]
notaComun' :: Pitch -> [Pitch] -> [Pitch]
notaComun' prev sig = do
  let hacerListasDeListas' = hacerListasDeListas sig
  concat $ fmap (\s -> notaComun prev s) hacerListasDeListas'

-- notaComun :: (Rational, Pitch) -> [(Rational, Pitch)] -> [(Rational, Pitch)]
notaComun :: Pitch -> [Pitch] -> [Pitch]
notaComun prev sig = filter ((==) prev) sig

-- hacerListasDeListas :: [(Rational, Pitch)] -> [[(Rational, Pitch)]]
hacerListasDeListas :: [Pitch] -> [[Pitch]]
hacerListasDeListas xs = fmap (\x -> hacerListaDeListas x) xs

-- hacerListaDeListas :: (Rational, Pitch) -> [(Rational, Pitch)]
hacerListaDeListas :: Pitch -> [Pitch]
hacerListaDeListas x = do
  let xs = List.sort $ [x, (x - 12) .. 0] ++ [(x + 12), (x + 24) .. 127] -- [Pitch]
  xs

-- (0, 60) [(0.5, 65), (0.5, 69), (0.5, 72)]
-- (0, 60) [[ ], [], []]

-- 2. Escoger todos los 3 valores con distancia más corta de la lista.
distancias :: Pitch -> Pitch -> [(Double, Pitch)]
distancias prev sig = do
  let saltoSig = (sig - 12)
  let sig' = (sig + 12)
  let saltoSig' = (sig + 12) + 12
  let sigs = List.sort $ [sig, saltoSig .. 0] ++ [sig', saltoSig' .. 127] -- [Pitch]
  let sigs' = catMaybes $  fmap (\s -> if (s >= (prev - 12)) && (s <= (prev + 12)) then Just s else Nothing) sigs  -- [(Rational, Pitch)]
  let sigsYdist = fmap (\d -> (abs $ prev - d, d)) sigs'
  sigsYdist

distancias' :: Pitch -> [Pitch] -> [(Double, Pitch)]
distancias' prev sig = concat $ fmap (\s -> distancias prev s) sig

distancias'' :: [Pitch] -> [Pitch] -> [(Double, Pitch)]
distancias'' prev sig = List.sort $ concat $ fmap (\p -> distancias' p sig) prev

-- uniq which removes duplicates from a list (found here https://codereview.stackexchange.com/questions/150533/filter-duplicate-elements-in-haskell)
uniqPitch :: Eq b => [(a,b)] -> [(a,b)]
uniqPitch [] = []
uniqPitch ((x, y):xs) = (x,y) : uniqPitch (filter ((/= y) . snd) xs)

notasCercanas :: Int -> [(Double, Pitch)] -> [Pitch]
notasCercanas tamano notas = do
  let lista1 = take tamano $ uniqPitch notas
  List.sort $ fmap snd lista1




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
