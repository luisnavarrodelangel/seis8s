module Sound.Cumbia.Instrument where

import Data.Tempo
import Data.Time
import Data.Map as Map
import Data.Fixed
import Control.Monad.State
import qualified Sound.Tidal.Context as Tidal
import qualified Data.List as List

import Sound.Cumbia.InstrumentState
import Sound.Cumbia.GlobalMaterial
import Sound.Cumbia.Style
import Sound.Cumbia.Harmony

--set the project as a cabal library (not hackage)-- look at tempi
-- look at tempi.cabal in github
-- Haskell versioning
-- - Build-depends:
--       base >=4.8 && <5,
--       time >=1.8.0.2 && <1.9
--       tempi >= 1.0.2.0 && < 1.10

-- for my version use 0.0.1

-- 2. parsing and rendering

-- what can be transformed from the instrument based on the Global and Instrument material
data Instrument = Instrument {
  getEvents :: GlobalMaterial -> Style -> Tempo -> BeginWindowTime -> EndWindowTime -> State InstrumentState [Event]
}


type BeginWindowTime = UTCTime
type EndWindowTime = UTCTime
type Event = (UTCTime, Tidal.ControlMap)

-- function that generates an instrument
piano :: Instrument
piano = Instrument { getEvents = pianoEvents}

pianoEvents gmm style tempo iw ew = do
  let attacks = rhythmicPattern (pianoRhythmPattern0 style) tempo iw ew  -- [Rational] [(1, 0), (1, 0.5)]
  let chordPattern = generatechords attacks (harmony gmm) -- [(Rational, [Pitch])]
  let pitchPattern = concatChords chordPattern -- [(Rational, Pitch)]
  let time = fmap (\c -> countToTime tempo (fst c)) pitchPattern  -- [UTCTime]
  let cmap =  fmap (\p -> fromList [("sample_name", Tidal.VS "piano"), ("note", Tidal.VF $ snd p)]) pitchPattern --Tidal.ControlMap
  let events = zip time cmap -- [(UTCTime, Tidal.ControlMap)]
  return events

testgmm = GlobalMaterial {harmony = myHarmony }

myHarmony = [Harmony (Chord 0 major) (2, 0) (2, 1), Harmony (Chord 2 minor) (2, 1) (2, 2)]
myTempo  = Tempo {freq = 1, time = myTime 0, count = 0}

myDay :: Day
myDay = fromGregorian 2020 4 4

myTime :: Rational -> UTCTime
myTime s = UTCTime myDay (realToFrac s)


-- --piano mg cumbia
-- -- someParser
