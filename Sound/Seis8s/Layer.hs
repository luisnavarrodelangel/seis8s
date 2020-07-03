{-# LANGUAGE OverloadedStrings #-}

module Sound.Seis8s.Layer where

import Data.Time
import Data.Fixed
import Data.Tempo
import Sound.OSC as H
import qualified Data.Map as M
import Data.Tuple.Select
import Control.Monad.State
-- import Control.Monad
import qualified Data.Text  as T
import qualified Data.List as List

import Sound.Seis8s.Style
import Sound.Seis8s.Harmony
import Sound.Seis8s.Generic
import Sound.Seis8s.LayerState
import Sound.Seis8s.GlobalMaterial

--Layer type has to look like Instrument
-- an instrument as a layer. layer should have been the fundamental type all along.


-- what can be transformed from the instrument based on the Global and Instrument material
data Layer = Layer {
  getEvents :: GlobalMaterial -> Style -> Tempo -> BeginWindowTime -> EndWindowTime -> State LayerState [Event],
  style :: Style
}

--I can make function s from layer to layer that tranform the output (i.e. like fmapping over the list of events)
-- or also override the informayioni of the style. e.g every event louder
--the style field is being prepared to later be an argument of getEvents.

mapStyle :: (Style -> Style) -> Layer -> Layer
mapStyle f x = x { style = f (style x)}

mapEvents :: (Event -> Event) -> Layer -> Layer
mapEvents f x = x {getEvents = g}
  where
    --a function calling the old function
    g gm s t iw ew = fmap (fmap f) (getEvents x gm s t iw ew) -- State LayerState [Event]
    --list of events as an empty list

-- an event to event function could be one where makes all the events louder
-- db :: Rational -> Layer -> Layer
-- db gain = mapEvents (dbEvents gain)
--
-- forwardTime :: NominalDiffTime -> Layer -> Layer
-- forwardTime n = mapEvents (efunc n)


-- let l = alternar 2 seleccionarEstilo bajo
  --  (runState  (getEvents l testgmm cumbia mytempo (mytime 0) (mytime 1)) emptyLayerStat)
-- convert windows to metric position, and do calcs in metre posiion a convert to actual times.
      -- if my condition is true i want to call the getEvents function of x and if its false call the getEvents
      --of (f x)
      -- the events could come from f0 or f1
-- from where do i take the tempo?
myEvent :: [Event]
myEvent = [((mytime 3), M.fromList [("s", string "test")])]
-- dbEvents :: Rational -> Event -> Event
-- dbEvents gain x =  --uses dbamp (copy from musicW). call dbam on the gain to get a raw amplitude and
-- multiply that by whatever ampltude is on the event to return a new event.
  --
-- where :: [Event] -> [Event]
-- State LayerState [Event]-- is monad, when f map over it , you change the value of the last type like m a
-- the style doesn't need to be provided in layer, bcs it's implicit in the layer
-- now
-- parse style as layer


type BeginWindowTime = UTCTime
type EndWindowTime = UTCTime
-- type Event = (UTCTime, Map Text Datum)
-- type Event = (UTCTime, M.Map T.Text Datum)
type Event = (UTCTime, M.Map T.Text Datum)

-- ev :: [Event']
-- ev = [Event' (mytime 0, M.fromList [("n",Int32 {d_int32 = 1}),("note",Double {d_double = 0.0}),("s",ASCII_String {d_ascii_string = "piano"})])]

emptyLayer :: Layer
emptyLayer = Layer {getEvents = emptyEvents, style = defaultStyle}

emptyEvents _ _ _ _ _ = do
  return $ []

-- function that generates an instrument
cuerda :: Layer
cuerda = Layer {getEvents = cuerdaEvents, style = defaultStyle}

piano :: Layer
piano = Layer { getEvents = pianoEvents, style = defaultStyle}

bajo :: Layer
bajo = Layer {getEvents = bajoEvents, style = defaultStyle}
--
guira :: Layer
guira = Layer {getEvents = guiraEvents, style = defaultStyle}

contras :: Layer
contras = Layer {getEvents = contrasEvents, style = defaultStyle}

tarola :: Layer
tarola = Layer {getEvents = tarolaEvents, style = defaultStyle}

efecto :: Layer
efecto = Layer {getEvents = efectoEvents, style = defaultStyle}

cuerdaEvents gmm style tempo iw ew = do
  let pitchType = (fst $ cuerdaPitchPattern0 style)
  let equateLists' = equateLists (cuerdaRhythmPattern0 style) (cuerdaSampleNPattern0 style) (snd $ cuerdaPitchPattern0 style)
  let cuerdaRhythmPattern = sel1 equateLists'
  let cuerdaSampleNPattern = sel2 equateLists'
  let cuerdaPitchPattern = sel3 equateLists'
  let nPat = List.zip cuerdaRhythmPattern cuerdaSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip cuerdaRhythmPattern cuerdaPitchPattern -- [(RhythmicPosition, (String, Double))]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, (String, Int, Double))]
  let cuerdaline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) cuerdaline  -- [UTCTime]
  let instCmap = cmap'' "cuerdas" samplePat cuerdaline --Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

-- generateLine :: [(Rational, (String, Double))] -> [Harmony] -> [(Rational, Pitch)]


pianoEvents gmm style tempo iw ew = do
  let pitchType = fst $ pianoPitchPattern0 style
  let equateLists' = equateLists (pianoRhythmPattern0 style) (pianoSampleNPattern0 style) (snd $ pianoPitchPattern0 style)
  let pianoRhythmPattern = sel1 equateLists'
  let pianoSampleNPattern = sel2 equateLists'
  let pianoPitchPattern = sel3 equateLists'
  let nPat = List.zip pianoRhythmPattern pianoSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip pianoRhythmPattern pianoPitchPattern -- [(RhythmicPosition, (String, Double))]
  let pitchPat = pitchPattern pat tempo iw ew
  -- let attacks = rhythmicPattern (pianoRhythmPattern0 style) tempo iw ew  -- [Rational] [(1, 0), (1, 0.5)]
  -- let chordPattern = generatechords attacks (harmony gmm) -- [(Rational, [Pitch])]
  -- let pitchPattern = concatChords chordPattern -- [(Rational, Pitch)]
  let pianoline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) pianoline  -- [UTCTime]
  let instCmap = cmap'' "piano" samplePat pianoline--Map Text Datum
  let events =  List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

bajoEvents gmm style tempo iw ew = do
  let pitchType = (fst $ bassPitchPattern0 style)
  let equateLists' = equateLists (bassRhythmPattern0 style) (bassSampleNPattern0 style) (snd $ bassPitchPattern0 style)
  let bassRhythmPattern = sel1 equateLists'
  let bassSampleNPattern = sel2 equateLists'
  let bassPitchPattern = sel3 equateLists'
  let nPat = List.zip bassRhythmPattern bassSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip bassRhythmPattern bassPitchPattern --[(RhythmicPattern, Double)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let bassline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
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
  let pitchType = fst $ efectoPitchPattern0 style
  let equateLists' = equateLists (efectoRhythmPattern0 style) (efectoSampleNPattern0 style) (snd $ efectoPitchPattern0 style)
  let efectoRhythmPattern = sel1 equateLists'
  let efectoSampleNPattern = sel2 equateLists'
  let efectoPitchPattern = sel3 equateLists'
  let nPat = List.zip efectoRhythmPattern efectoSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip efectoRhythmPattern efectoPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let efectoline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) efectoline  -- [UTCTime]
  let instCmap = cmap'' "efecto" samplePat efectoline--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events


cmap'' :: String -> [(Rational, Int)] -> [(Rational, Pitch)] -> [M.Map T.Text Datum]
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
equateLists :: [(Rational, Rational)] -> [Int] -> [(String, Double, Double)] -> ([(Rational, Rational)], [Int], [(String, Double, Double)])
equateLists attacks ns chi
  | (length attacks == length ns) && (length ns == length chi) = (attacks, ns, chi)
  | (length attacks > length ns) && (length attacks == length chi) = (attacks, take (length attacks) $ cycle ns, chi)
  -- | (length attacks > length ns) && (length attacks > length chi) = (attacks, take (length attacks) $ cycle ns, take (length attacks) $ cycle chi)
  | (length attacks > length ns) && (length attacks > length chi) = (attacks, take (length attacks) $ cycle ns, chi)
  | (length attacks > length ns) && (length attacks < length chi) = (attacks, take (length attacks) $ cycle ns, chi)
  | (length attacks == length ns) && (length ns > length chi)  = (attacks, ns, take (length ns) $ cycle chi)
  | (length attacks < length ns) && (length ns == length chi)  = (take (length ns) $ cycle attacks, ns, chi)
  -- | (length attacks < length ns) && (length ns > length chi) = (take (length ns) $ cycle attacks, ns, take
  | (length attacks < length ns) && (length ns > length chi) =  (attacks, ns, chi)
  | (length attacks == length ns) && (length ns < length chi) = (take (length chi) $ cycle attacks, take (length chi) $ cycle ns, chi) -- genera acordes
  | otherwise = error "case not expected"
-- [0.5, 0.25] [1, 1] [0,1]
-- [0.5, 0.25] [1] [0,1]
-- [0.5, 0.25] [1] [0]
-- [0.25, 0.25] [1,1] [0]
-- [0.5] [1, 1] [0, 1]
-- [0.5] [1, 1] [0]
-- [0.5] [1] [0,1] -- genera acordes
-- [0,  0.125] [1] [0,1, 2]
-- bassSampleNPattern0 = [1]
-- bassRhythmPattern0 = [(1/1, 0/1), (1/1, 1/8)]
--bassPitchPattern0 = [0, 1, 2]
