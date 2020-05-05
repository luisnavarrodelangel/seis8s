module Generic where

fract x = x - (realToFrac $ floor x)
