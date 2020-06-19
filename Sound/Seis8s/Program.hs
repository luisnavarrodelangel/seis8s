module Sound.Seis8s.Program where

import Sound.Seis8s.GlobalMaterial
import Sound.Seis8s.Style as S
import Sound.Seis8s.Layer

import Data.IntMap.Strict


-- type Program = (GlobalMaterial,Style,Instrument)
-- data Layer = Layer (S.Style, Instrument)
type Program = ([Layer], GlobalMaterial)
-- type Program = (Layer, GlobalMaterial)

-- emptyLayer = Layer (defaultStyle, emptyInstrument)
