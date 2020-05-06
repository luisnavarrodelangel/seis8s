module Parser (parseLang) where

import Program
import GlobalMaterial
import Style
import Instrument

import Language.Haskell.Exts
import Language.Haskellish
import Data.Tempo
import Data.Time
import Data.Bifunctor

-- import Text.ParserCombinators.Parsec
import qualified Sound.Tidal.Context as Tidal

type H = Haskellish ()

-- type Program = (GlobalMaterial,Style, Instrument)

parseLang :: String -> Either String Program
parseLang = f . parseExp
  where
    f (ParseOk x) =  second fst $ runHaskellish myParser () x
    f (ParseFailed l s) = Left s

myParser:: H Program
myParser = do
    reserved "test"
    return (defaultGlobalMaterial,cumbia,piano)

-- render :: (GlobalMaterial,Style,Instrument) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime,Tidal.ControlMap)]
render :: Program -> Tempo -> UTCTime -> UTCTime -> [(UTCTime,Tidal.ControlMap)]
render (gmm, style, inst) tempo iw ew = (pianoEvents gmm style tempo iw ew) inst
