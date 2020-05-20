module Sound.Cumbia.Program where

import Sound.Cumbia.GlobalMaterial
import Sound.Cumbia.Style as S
import Sound.Cumbia.Instrument

import Data.IntMap.Strict


-- type Program = (GlobalMaterial,Style,Instrument)
data Layer = Layer (S.Style, Instrument)
type Program = ([Layer], GlobalMaterial)
-- type Program = (Layer, GlobalMaterial)

emptyLayer = Layer (defaultStyle, emptyInstrument)
