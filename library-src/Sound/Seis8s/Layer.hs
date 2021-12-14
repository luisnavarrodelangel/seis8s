{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

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
-- myEvent :: [Event]
-- myEvent = [((mytime 3), M.fromList [("s", string "test")])]
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
type Event = (UTCTime, M.Map T.Text Datum)

-- ev :: [Event']
-- ev = [Event' (mytime 0, M.fromList [("n",Int32 {d_int32 = 1}),("note",Double {d_double = 0.0}),("s",ASCII_String {d_ascii_string = "teclado"})])]

emptyLayer :: Layer
emptyLayer = Layer {getEvents = emptyEvents, style = defaultStyle}

emptyEvents _ _ _ _ _ = do
  return $ []

-- function that generates an instrument
acordeon :: Layer
acordeon = Layer {getEvents = acordeonEvents, style = defaultStyle}

zampoña :: Layer
zampoña = Layer {getEvents = guiraEvents, style = defaultStyle}

cuerda :: Layer
cuerda = Layer {getEvents = cuerdaEvents, style = defaultStyle}

teclado :: Layer
teclado = Layer { getEvents = tecladoEvents, style = defaultStyle}

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

jamblock :: Layer
jamblock = Layer {getEvents = jamblockEvents, style = defaultStyle}

extras :: Layer
extras = Layer {getEvents = extrasEvents, style = defaultStyle}

zampoñaEvents gmm style tempo iw ew = do
  let paneo = zampoñaPanPattern0 style
  let gain = zampoñaGainPattern0 style
  let pitchType = (fst $ zampoñaPitchPattern0 style)
  let equateLists' = equateLists (zampoñaRhythmPattern0 style) (zampoñaSampleNPattern0 style) (snd $ zampoñaPitchPattern0 style)
  let zampoñaRhythmPattern = sel1 equateLists' -- [(Rational, Rational)]
  let zampoñaRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) zampoñaRhythmPattern

  let zampoñaSampleNPattern = sel2 equateLists'
  let zampoñaPitchPattern = sel3 equateLists'
  let nPat = List.zip zampoñaRhythmPattern' zampoñaSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip zampoñaRhythmPattern' zampoñaPitchPattern -- [(RhythmicPosition, (String, Double))]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, (String, Int, Double))]
  let zampoñaline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) zampoñaline  -- [UTCTime]
  let instCmap = cmap'' "zampoña" samplePat zampoñaline paneo gain--[Map Text Datum]
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]

  return events

acordeonEvents gmm style tempo iw ew = do
  let paneo = acordeonPanPattern0 style
  let gain = acordeonGainPattern0 style
  let pitchType = (fst $ acordeonPitchPattern0 style)
  let equateLists' = equateLists (acordeonRhythmPattern0 style) (acordeonSampleNPattern0 style) (snd $ acordeonPitchPattern0 style)
  let acordeonRhythmPattern = sel1 equateLists' -- [(Rational, Rational)]
  let acordeonRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) acordeonRhythmPattern

  let acordeonSampleNPattern = sel2 equateLists'
  let acordeonPitchPattern = sel3 equateLists'
  let nPat = List.zip acordeonRhythmPattern' acordeonSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip acordeonRhythmPattern' acordeonPitchPattern -- [(RhythmicPosition, (String, Double))]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, (String, Int, Double))]
  let acordeonline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) acordeonline  -- [UTCTime]
  let instCmap = cmap'' "acordeon" samplePat acordeonline paneo gain--[Map Text Datum]
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]

  return events

cuerdaEvents gmm style tempo iw ew = do
  let paneo = cuerdaPanPattern0 style
  let gain = cuerdaGainPattern0 style
  let pitchType = (fst $ cuerdaPitchPattern0 style)
  let equateLists' = equateLists (cuerdaRhythmPattern0 style) (cuerdaSampleNPattern0 style) (snd $ cuerdaPitchPattern0 style)
  let cuerdaRhythmPattern = sel1 equateLists' -- [(Rational, Rational)]
  let cuerdaRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) cuerdaRhythmPattern

  let cuerdaSampleNPattern = sel2 equateLists'
  let cuerdaPitchPattern = sel3 equateLists'
  let nPat = List.zip cuerdaRhythmPattern' cuerdaSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip cuerdaRhythmPattern' cuerdaPitchPattern -- [(RhythmicPosition, (String, Double))]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, (String, Int, Double))]
  let cuerdaline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) cuerdaline  -- [UTCTime]
  let instCmap = cmap'' "cuerdas" samplePat cuerdaline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]

  return events

tecladoEvents' gmm style tempo iw ew = do
  let paneo = tecladoPanPattern0 style
  let gain = tecladoGainPattern0 style

  let pitchType = fst $ tecladoPitchPattern0 style
  let equateLists' = equateLists (tecladoRhythmPattern0 style) (tecladoSampleNPattern0 style) (snd $ tecladoPitchPattern0 style)
  let tecladoRhythmPattern = sel1 equateLists'
  let tecladoRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) tecladoRhythmPattern

  let tecladoRhythmPattern'' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm)))  (tecladoRhythmPattern0 style)

  let tecladoSampleNPattern = sel2 equateLists'
  let tecladoPitchPattern = sel3 equateLists'
  let nPat = List.zip tecladoRhythmPattern' tecladoSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]

  let nPat' = List.zip (rhythmicPattern tecladoRhythmPattern'' tempo iw ew) tecladoSampleNPattern --[(RhythmicPattern, Int)]

  let samplePat' =  concat $ replicate (length tecladoRhythmPattern'') nPat'--[(Rational, Int)]

  let pat = List.zip tecladoRhythmPattern' tecladoPitchPattern -- [(RhythmicPosition, (String, Double))]
  let pitchPat = pitchPattern pat tempo iw ew
  let tecladoline | pitchType == "intervalo" = (generateLine pitchPat (harmony gmm)) -- [(Rational, Pitch )]
                  | pitchType == "midinote" = (generateLineFromMidi pitchPat) -- [(Rational, Pitch)]
                -- | pitchType == "acorde" = concatChords $ pickChords (rhythmicPattern tecladoRhythmPattern' tempo iw ew) (harmony gmm) --[(Rational, Pitch )]
                  | pitchType == "acorde" = concatChords $ pickChords' (rhythmicPattern tecladoRhythmPattern'' tempo iw ew) (harmony gmm) (snd $ tecladoPitchPattern0 style) -- tecladoPitchPattern
  let time = fmap (\c -> countToTime tempo (fst c)) tecladoline  -- [UTCTime]
  -- let instCmap = cmap'' "teclado" samplePat tecladoline paneo gain--Map Text Datum
  let instCmap = cmap'' "teclado" samplePat' tecladoline paneo gain--Map Text Datum
  let events =  List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events

tecladoEvents gmm style tempo iw ew = do
  let paneo = tecladoPanPattern0 style
  let gain = tecladoGainPattern0 style

  let pitchType = fst $ tecladoPitchPattern0 style
  let equateLists' = equateLists (tecladoRhythmPattern0 style) (tecladoSampleNPattern0 style) (snd $ tecladoPitchPattern0 style)
  let tecladoRhythmPattern = sel1 equateLists'
  let tecladoRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) tecladoRhythmPattern

  let tecladoRhythmPattern'' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm)))  (tecladoRhythmPattern0 style)

  let tecladoSampleNPattern = sel2 equateLists'
  let tecladoPitchPattern = sel3 equateLists'
  -- let nPat = List.zip tecladoRhythmPattern' tecladoSampleNPattern --[(RhythmicPattern, Int)]
  -- let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)] -- maybe not needed of any of the instruments?

  let pat = List.zip tecladoRhythmPattern' tecladoPitchPattern -- [(RhythmicPosition, (String, Double))]
  let pitchPat = pitchPattern pat tempo iw ew
  let tecladoline | pitchType == "intervalo" = (generateLine pitchPat (harmony gmm)) -- [(Rational, Pitch )]
                  | pitchType == "midinote" = (generateLineFromMidi pitchPat) -- [(Rational, Pitch)]
                -- | pitchType == "acorde" = concatChords $ pickChords (rhythmicPattern tecladoRhythmPattern' tempo iw ew) (harmony gmm) --[(Rational, Pitch )]
                  | pitchType == "acorde" = concatChords $ pickChords' (rhythmicPattern tecladoRhythmPattern'' tempo iw ew) (harmony gmm) (snd $ tecladoPitchPattern0 style) -- tecladoPitchPattern
  let time = fmap (\c -> countToTime tempo (fst c)) tecladoline  -- [UTCTime]
  -- let instCmap = cmap'' "teclado" samplePat tecladoline paneo gain--Map Text Datum

  -- let nPat' = concat $ replicate (length (rhythmicPattern tecladoRhythmPattern'' tempo iw ew)) (tecladoSampleNPattern0 style)--[(Rational, Int)]
  -- let samplePat' = zip (fmap (\t -> timeToCount tempo t) time) (concat $ replicate (length time) (tecladoSampleNPattern0 style))
  let samplePat' = samplePatternRat time (tecladoSampleNPattern0 style) tempo

  let instCmap = cmap'' "teclado" samplePat' tecladoline paneo gain--Map Text Datum
  let events =  List.zip time instCmap -- [(UTCTime, Map Text Datum)]
  return events


nTest = [0,0,0,0,0,0]
nTest2 = [0,0,0]

rTest :: [(Rational, Rational)]
rTest = [(0.5,0.125), (0.5,0.125), (0.5,0.125), (0.5, 0.375), (0.5, 0.375), (0.5, 0.375)]

rTest2 :: [(Rational, Rational)]
rTest2 = [(0.5,0.125), (0.5, 0.375)]

rTest3 :: [(Rational, Rational)]
rTest3 = [(0.5,0.125), (0.5, 0.375), (0.5,0.125), (0.5, 0.375)]

-- p
-- tecladoSampleNPattern1 = take 6 $ cycle [0],
-- tecladoRhythmPattern1 = [(1,0.25), (1,0.25), (1,0.25), (1, 0.75), (1, 0.75), (1, 0.75)], -- ie. [𝄽 ♩ 𝄽 ♩],
-- tecladoPitchPattern1 = ("intervalo", [intervalo "unisono" 0, intervalo "3a" 0, intervalo "5a" 0, intervalo "unisono" 0, intervalo "3a" 0, intervalo "5a" 0]), -- not used yet

noteTest = [("unisono",0.0,0.0),("tercera",0.0,0.0),("quinta",0.0,0.0),("unisono",0.0,0.0),("tercera",0.0,0.0),("quinta",0.0,0.0)]

noteTest2 = [("unisono",0.0,0.0),("tercera",0.0,0.0),("quinta",0.0,0.0)]

noteTest3 = [("unisono",0.0,0.0),("tercera",0.0,0.0),("quinta",0.0,0.0)]

bajoEvents gmm style tempo iw ew = do
  let paneo = bassPanPattern0 style
  let gain = bassGainPattern0 style

  let pitchType = (fst $ bassPitchPattern0 style)
  let equateLists' = equateLists (bassRhythmPattern0 style) (bassSampleNPattern0 style) (snd $ bassPitchPattern0 style)
  let bassRhythmPattern = sel1 equateLists'
  let bassRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) bassRhythmPattern

  let bassSampleNPattern = sel2 equateLists'
  let bassPitchPattern = sel3 equateLists'
  let nPat = List.zip bassRhythmPattern' bassSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip bassRhythmPattern' bassPitchPattern --[(RhythmicPattern, Double)]
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
  let guiraRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) guiraRhythmPattern

  let guiraSampleNPattern = sel2 equateLists'
  let guiraPitchPattern = sel3 equateLists'
  let nPat = List.zip guiraRhythmPattern' guiraSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip guiraRhythmPattern' guiraPitchPattern --[(RhythmicPattern, Int)]
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
  let contrasRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) contrasRhythmPattern

  let contrasSampleNPattern = sel2 equateLists'
  let contrasPitchPattern = sel3 equateLists'
  let nPat = List.zip contrasRhythmPattern' contrasSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip contrasRhythmPattern' contrasPitchPattern --[(RhythmicPattern, Int)]
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
  let tarolaRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) tarolaRhythmPattern

  let tarolaSampleNPattern = sel2 equateLists'
  let tarolaPitchPattern = sel3 equateLists'
  let nPat = List.zip tarolaRhythmPattern' tarolaSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip tarolaRhythmPattern' tarolaPitchPattern --[(RhythmicPattern, Int)]
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
  let efectoRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) efectoRhythmPattern
  let efectoSampleNPattern = sel2 equateLists'
  let efectoPitchPattern = sel3 equateLists'
  let nPat = List.zip efectoRhythmPattern' efectoSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip efectoRhythmPattern' efectoPitchPattern --[(RhythmicPattern, Int)]
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
  let altavozRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) altavozRhythmPattern

  let altavozSampleNPattern = sel2 equateLists'
  let altavozPitchPattern = sel3 equateLists'
  let nPat = List.zip altavozRhythmPattern' altavozSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip altavozRhythmPattern' altavozPitchPattern --[(RhythmicPattern, Int)]
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
  let extrasRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) extrasRhythmPattern

  let extrasSampleNPattern = sel2 equateLists'
  let extrasPitchPattern = sel3 equateLists'
  let nPat = List.zip extrasRhythmPattern' extrasSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip extrasRhythmPattern' extrasPitchPattern --[(RhythmicPattern, Int)]
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
  let equateLists' = equateLists2 (congasRhythmPattern0 style) (congasSampleNPattern0 style) (snd $ congasPitchPattern0 style)
  let congasRhythmPattern = sel1 equateLists'
  let congasRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) congasRhythmPattern
  let congasSampleNPattern = sel2 equateLists'
  let congasPitchPattern = sel3 equateLists'
  let nPat = List.zip congasRhythmPattern' congasSampleNPattern --[(RhythmicPattern, (String, Int)]
  -- let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)] -- debe ser (Rational, (String, Int))
  let samplePat = samplePattern2 nPat tempo iw ew --[(Rational, Int)] -- debe ser (Rational, (String, Int))
  let pat = List.zip congasRhythmPattern' congasPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let congasline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) congasline  -- [UTCTime]
  let instCmap = cmap''' samplePat congasline paneo gain--Map Text Datum
  -- let instCmap = cmap''' samplePat congasline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]

  return events

claveEvents gmm style tempo iw ew = do
  let paneo = clavePanPattern0 style
  let gain = claveGainPattern0 style
  let pitchType = fst $ clavePitchPattern0 style
  let equateLists' = equateLists (claveRhythmPattern0 style) (claveSampleNPattern0 style) (snd $ clavePitchPattern0 style)
  let claveRhythmPattern = sel1 equateLists'
  let claveRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) claveRhythmPattern

  let claveSampleNPattern = sel2 equateLists'
  let clavePitchPattern = sel3 equateLists'
  let nPat = List.zip claveRhythmPattern'  claveSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip claveRhythmPattern' clavePitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let claveline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) claveline  -- [UTCTime]
  let instCmap = cmap'' "tarola" samplePat claveline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]

  return events


jamblockEvents gmm style tempo iw ew = do
  let paneo = jamblockPanPattern0 style
  let gain = jamblockGainPattern0 style
  let pitchType = fst $ jamblockPitchPattern0 style
  let equateLists' = equateLists (jamblockRhythmPattern0 style) (jamblockSampleNPattern0 style) (snd $ jamblockPitchPattern0 style)
  let jamblockRhythmPattern = sel1 equateLists'
  let jamblockRhythmPattern' = fmap (\(metre,attack) -> (metre * toRational (compas gmm), attack * toRational (compas gmm))) jamblockRhythmPattern

  let jamblockSampleNPattern = sel2 equateLists'
  let jamblockPitchPattern = sel3 equateLists'
  let nPat = List.zip jamblockRhythmPattern'  jamblockSampleNPattern --[(RhythmicPattern, Int)]
  let samplePat = samplePattern nPat tempo iw ew --[(Rational, Int)]
  let pat = List.zip jamblockRhythmPattern' jamblockPitchPattern --[(RhythmicPattern, Int)]
  let pitchPat = pitchPattern pat tempo iw ew --[(Rational, Int)]
  let jamblockline = if pitchType == "intervalo" then (generateLine pitchPat (harmony gmm)) else (generateLineFromMidi pitchPat) -- [(Rational, [Pitch])]
  let time = fmap (\c -> countToTime tempo (fst c)) jamblockline  -- [UTCTime]
  let instCmap = cmap'' "jamblock" samplePat jamblockline paneo gain--Map Text Datum
  let events = List.zip time instCmap -- [(UTCTime, Map Text Datum)]

  return events

-- [("quinto", 0), ("quinto", 1) ...]
cmap''' :: [(Rational, (String, Int))] -> [(Rational, Pitch)] -> Double -> Double -> [M.Map T.Text Datum]
cmap''' ns ps pan gain = do
  let fs' = fmap (\f -> fst $ snd f) ns -- string - folder name
  let ns' = fmap (\n -> snd $ snd n) ns -- int
  let ps' = fmap (\p -> snd p) ps
  fmap (\(f, n, p) -> cmap' f n p pan gain) $ zip3 fs' ns' ps'

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
equateLists :: [(Rational, Rational)] -> NPattern -> [(String, Double, Double)] -> ([(Rational, Rational)], [Int], [(String, Double, Double)])
equateLists attacks (NPattern1 ns) chi
  | (length attacks == length ns) && (length ns == length chi) = (attacks, ns, chi)
  | (length attacks > length ns) && (length attacks == length chi) = (attacks, take (length attacks) $ cycle' ns, chi)
  | (length attacks > length ns) && (length attacks > length chi) = (attacks, take (length attacks) $ cycle' ns, take (length attacks) $ cycle' chi)
  | (length attacks > length ns) && (length attacks > length chi) = (attacks, take (length attacks) $ cycle' ns, chi)
  | (length attacks > length ns) && (length attacks < length chi) = (attacks, take (length attacks) $ cycle' ns, chi)
  | (length attacks == length ns) && (length ns > length chi)  = (attacks, ns, take (length ns) $ cycle' chi)
  | (length attacks < length ns) && (length ns == length chi)  = (take (length ns) $ cycle' attacks, ns, chi)
  -- | (length attacks < length ns) && (length ns > length chi) = (take (length ns) $ cycle' attacks, ns, take --??
  | (length attacks < length ns) && (length ns > length chi) =  (attacks, ns, chi)
  | (length attacks == length ns) && (length ns < length chi) = (take (length chi) $ cycle' attacks, take (length chi) $ cycle' ns, chi) -- genera acordes
  | otherwise = error "case not expected"

equateLists2 :: [(Rational, Rational)] -> NPattern -> [(String, Double, Double)] -> ([(Rational, Rational)],  [(String, Int)], [(String, Double, Double)])
equateLists2 attacks (NPattern2 ns) chi
  | (length attacks == length ns) && (length ns == length chi) = (attacks, ns, chi)
  | (length attacks > length ns) && (length attacks == length chi) = (attacks, take (length attacks) $ cycle' ns, chi)
  | (length attacks > length ns) && (length attacks > length chi) = (attacks, take (length attacks) $ cycle' ns, take (length attacks) $ cycle' chi)
  | (length attacks > length ns) && (length attacks > length chi) = (attacks, take (length attacks) $ cycle' ns, chi)
  | (length attacks > length ns) && (length attacks < length chi) = (attacks, take (length attacks) $ cycle' ns, chi)
  | (length attacks == length ns) && (length ns > length chi)  = (attacks, ns, take (length ns) $ cycle' chi)
  | (length attacks < length ns) && (length ns == length chi)  = (take (length ns) $ cycle' attacks, ns, chi)
  | (length attacks < length ns) && (length ns > length chi) = (attacks, take (length attacks) ns, chi)
  | (length attacks < length ns) && (length ns > length chi) =  (attacks, ns, chi)
  | (length attacks == length ns) && (length ns < length chi) = (take (length chi) $ cycle' attacks, take (length chi) $ cycle' ns, chi) -- genera acordes
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
