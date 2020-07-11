module Sound.Seis8s.GlobalMaterial where

import Sound.Seis8s.Harmony

import Data.Tempo
import Data.Time

--GlobalMaterial -- i.e current events in the global material
-- it doesnt change by itself, only by the user
data GlobalMaterial = GlobalMaterial {
  -- tempo :: Tempo, --look at punctual, timenot, -- e.g. tempo 120 meaning 120/60 -- Cps, anchorTime(anyTime at the cycle grid) numberOfTheCycleAtThatTime
  harmony :: Progression -- combination of a pitch and a chord type, e.g 60 major
} deriving (Show)

defaultGlobalMaterial = GlobalMaterial { harmony = Progression 0.5 [Chord 60 major (0, 0.5)]}

-- defaultGlobalMaterial = GlobalMaterial { harmony = myharmony'}


myharmony = Progression 1 [Chord 60 major (0, 0.5), Chord 62 minor (0.5, 1)]
-- myharmony' = Progression 1 [Chord 60 [0, 4, 7] (0, 0.5), Chord 64 [(-1), 0, 7] (0.5, 1)]
testgmm = GlobalMaterial {harmony = myharmony }
-- [60, 64, 67]
-- [64, 67, 71]

-- myTempo  = Tempo {freq = 1, time = myTime 0, count = 0}
-- tempo is provided by Estuary in this case.
-- choice 1 is tempo being separate from the language, taking it out from the
-- choice 2 is having it there and over
