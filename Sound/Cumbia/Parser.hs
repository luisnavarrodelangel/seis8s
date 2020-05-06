module Sound.Cumbia.Parser (parseLang, render) where

import Sound.Cumbia.Program
import Sound.Cumbia.GlobalMaterial
import Sound.Cumbia.Style
import Sound.Cumbia.Instrument
import Sound.Cumbia.InstrumentState

import Language.Haskell.Exts
import Language.Haskellish
import Control.Monad.State
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
    reserved "piano"
    return (defaultGlobalMaterial,cumbia,piano)

--   getEvents :: GlobalMaterial -> Style -> Tempo -> BeginWindowTime -> EndWindowTime -> State InstrumentState [Event]
-- render :: (GlobalMaterial,Style,Instrument) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime,Tidal.ControlMap)]
-- runState :: State s a -> s -> (a, s) -- as soon as the state is meaningful I should stop discarding it.
--check Tidal.params for looking at the available params for webdirt

render :: Program -> Tempo -> UTCTime -> UTCTime -> [(UTCTime,Tidal.ControlMap)]
render (gmm, style, inst) tempo iw ew = fst $ runState x emptyInstrumentState --this should be another argument to my render function
  where
     x = getEvents inst gmm style tempo iw ew
