module Sound.Seis8s.Generic where


import Data.Tempo
import Data.Time

fract :: RealFrac a => a -> a
fract x = x - (realToFrac $ floor x)

-- test functions
mytempo  = Tempo {freq = 0.50, time = mytime 0, count = 0}

myDay :: Day
myDay = fromGregorian 2020 4 4

mytime :: Rational -> UTCTime
mytime s = UTCTime myDay (realToFrac s)

cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = cycle xs
--
-- testgmm = GlobalMaterial {harmony = myharmony }
--
-- cmajor = Chord 60 major
-- eminor = Chord 64 minor
