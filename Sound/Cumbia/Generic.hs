module Sound.Cumbia.Generic where


import Data.Tempo
import Data.Time

fract x = x - (realToFrac $ floor x)

-- test functions
mytempo  = Tempo {freq = 1, time = mytime 0, count = 0}

myDay :: Day
myDay = fromGregorian 2020 4 4

mytime :: Rational -> UTCTime
mytime s = UTCTime myDay (realToFrac s)

-- myharmony = [Harmony (Chord 60 major) (2, 0) (2, 1), Harmony (Chord 62 minor) (2, 1) (2, 2)]
--
-- testgmm = GlobalMaterial {harmony = myharmony }
--
-- cmajor = Chord 60 major
-- eminor = Chord 64 minor
