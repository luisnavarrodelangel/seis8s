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

altavoz :: Layer
altavoz = Layer {getEvents = altavozEvents, style = defaultStyle}

congas :: Layer
congas = Layer {getEvents = congasEvents, style = defaultStyle}

clave :: Layer
clave = Layer {getEvents = claveEvents, style = defaultStyle}

extras :: Layer
extras = Layer {getEvents = extrasEvents, style = defaultStyle}

cuerdaEvents gmm style tempo iw ew = do
  let paneo = cuerdaPanPattern0 style
  let gain = cuerdaGainPattern0 style

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
  let instCmap = cmap'' "cuerdas" samplePat cuerdaline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

pianoEvents gmm style tempo iw ew = do
  let paneo = pianoPanPattern0 style
  let gain = pianoGainPattern0 style

  let pitchType = fst $ pianoPitchPattern0 style
  let equateLists' = equateLists (pianoRhythmPattern0 style) (pianoSampleNPattern0 style) (snd $ pianoPitchPattern0 style)
  let pianoRhythmPattern = sel1 equateLists'
  let pianoSampleNPattern = sel2 equateLists'
  let pianoPitchPattern = sel3 equateLists'
  let nPat = List.zip pianoRhythmPattern pianoSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip pianoRhythmPattern pianoPitchPattern -- [(RhythmicPosition, (String, Double))]
  let pitchPat = pitchPattern pat tempo iw ew
  let pianoline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) pianoline  -- [UTCTime]
  let instCmap = cmap'' "piano" samplePat pianoline paneo gain--Map Text Datum
  let events =  List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

bajoEvents gmm style tempo iw ew = do
  let paneo = bassPanPattern0 style
  let gain = bassGainPattern0 style

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
  let instCmap = cmap'' "bajo" samplePat bassline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

guiraEvents gmm style tempo iw ew = do
  let paneo = guiraPanPattern0 style
  let gain = guiraGainPattern0 style
  let pitchType = fst $ guiraPitchPattern0 style
  let equateLists' = equateLists (guiraRhythmPattern0 style) (guiraSampleNPattern0 style) (snd $ guiraPitchPattern0 style)
  let guiraRhythmPattern = sel1 equateLists'
  let guiraSampleNPattern = sel2 equateLists'
  let guiraPitchPattern = sel3 equateLists'
  let nPat = List.zip guiraRhythmPattern guiraSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip guiraRhythmPattern guiraPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let guiraline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) --
  let time = fmap (\c -> countToTime tempo (fst c)) guiraline  -- [UTCTime]
  let instCmap = cmap'' "guira" samplePat guiraline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

contrasEvents gmm style tempo iw ew = do
  let paneo = contrasPanPattern0 style
  let gain = contrasGainPattern0 style
  let pitchType = fst $ contrasPitchPattern0 style
  let equateLists' = equateLists (contrasRhythmPattern0 style) (contrasSampleNPattern0 style) (snd $ contrasPitchPattern0 style)
  let contrasRhythmPattern = sel1 equateLists'
  let contrasSampleNPattern = sel2 equateLists'
  let contrasPitchPattern = sel3 equateLists'
  let nPat = List.zip contrasRhythmPattern contrasSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip contrasRhythmPattern contrasPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let contrasline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) --
  let time = fmap (\c -> countToTime tempo (fst c)) contrasline  -- [UTCTime]
  let instCmap = cmap'' "contratiempos" samplePat contrasline paneo gain--Map Text Datum

  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

tarolaEvents gmm style tempo iw ew = do
  let paneo = tarolaPanPattern0 style
  let gain = tarolaGainPattern0 style
  let pitchType = fst $ tarolaPitchPattern0 style
  let equateLists' = equateLists (tarolaRhythmPattern0 style) (tarolaSampleNPattern0 style) (snd $ tarolaPitchPattern0 style)
  let tarolaRhythmPattern = sel1 equateLists'
  let tarolaSampleNPattern = sel2 equateLists'
  let tarolaPitchPattern = sel3 equateLists'
  let nPat = List.zip tarolaRhythmPattern tarolaSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip tarolaRhythmPattern tarolaPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let tarolaline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) --
  let time = fmap (\c -> countToTime tempo (fst c)) tarolaline  -- [UTCTime]
  let instCmap = cmap'' "tarola" samplePat tarolaline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

efectoEvents gmm style tempo iw ew = do
  let paneo = efectoPanPattern0 style
  let gain = efectoGainPattern0 style
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
  let instCmap = cmap'' "efecto" samplePat efectoline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

altavozEvents gmm style tempo iw ew = do
  let paneo = altavozPanPattern0 style
  let gain = altavozGainPattern0 style
  let pitchType = fst $ altavozPitchPattern0 style
  let equateLists' = equateLists (altavozRhythmPattern0 style) (altavozSampleNPattern0 style) (snd $ altavozPitchPattern0 style)
  let altavozRhythmPattern = sel1 equateLists'
  let altavozSampleNPattern = sel2 equateLists'
  let altavozPitchPattern = sel3 equateLists'
  let nPat = List.zip altavozRhythmPattern altavozSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip altavozRhythmPattern altavozPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let altavozline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) altavozline  -- [UTCTime]
  let instCmap = cmap'' "altavoz" samplePat altavozline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

extrasEvents gmm style tempo iw ew = do
  let paneo = extrasPanPattern0 style
  let gain = extrasGainPattern0 style
  let pitchType = fst $ extrasPitchPattern0 style
  let equateLists' = equateLists (extrasRhythmPattern0 style) (extrasSampleNPattern0 style) (snd $ extrasPitchPattern0 style)
  let extrasRhythmPattern = sel1 equateLists'
  let extrasSampleNPattern = sel2 equateLists'
  let extrasPitchPattern = sel3 equateLists'
  let nPat = List.zip extrasRhythmPattern extrasSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip extrasRhythmPattern extrasPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let extrasline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) extrasline  -- [UTCTime]
  let instCmap = cmap'' "extras" samplePat extrasline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

congasEvents gmm style tempo iw ew = do
  let paneo = congasPanPattern0 style
  let gain = congasGainPattern0 style
  let pitchType = fst $ congasPitchPattern0 style
  let equateLists' = equateLists (congasRhythmPattern0 style) (congasSampleNPattern0 style) (snd $ congasPitchPattern0 style)
  let congasRhythmPattern = sel1 equateLists'
  let congasSampleNPattern = sel2 equateLists'
  let congasPitchPattern = sel3 equateLists'
  let nPat = List.zip congasRhythmPattern congasSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip congasRhythmPattern congasPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let congasline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) congasline  -- [UTCTime]
  let instCmap = cmap'' "congas" samplePat congasline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

claveEvents gmm style tempo iw ew = do
  let paneo = clavePanPattern0 style
  let gain = claveGainPattern0 style
  let pitchType = fst $ clavePitchPattern0 style
  let equateLists' = equateLists (claveRhythmPattern0 style) (claveSampleNPattern0 style) (snd $ clavePitchPattern0 style)
  let claveRhythmPattern = sel1 equateLists'
  let claveSampleNPattern = sel2 equateLists'
  let clavePitchPattern = sel3 equateLists'
  let nPat = List.zip claveRhythmPattern claveSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip claveRhythmPattern clavePitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let claveline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) claveline  -- [UTCTime]
  let instCmap = cmap'' "tarola" samplePat claveline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

cmap'' :: String -> [(Rational, Int)] -> [(Rational, Pitch)] -> Double -> Double -> [M.Map T.Text Datum]
cmap'' sampleName is ps pan gain = do
  let is' = fmap (\i -> snd i) is
  let ps' = fmap (\p -> snd p) ps
  fmap (\(i, p) -> cmap' sampleName i p pan gain) $ zip is' ps'

cmap' :: String -> Int -> Pitch -> Double -> Double -> M.Map T.Text Datum
cmap' sampleName sampleIndex pitch pan gain = M.fromList [("s", string sampleName), ("n", int32 sampleIndex), ("note", double pitchAdjustedOctave), ("pan", double pan), ("gain", double gain)]
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
