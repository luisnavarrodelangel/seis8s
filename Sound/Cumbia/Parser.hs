module Sound.Cumbia.Parser (parseLang, render) where

import Sound.Cumbia.Program
import Sound.Cumbia.GlobalMaterial
import Sound.Cumbia.Style as S
import Sound.Cumbia.Instrument
import Sound.Cumbia.InstrumentState
import Sound.Cumbia.Harmony
import Sound.Cumbia.Rhythm

import Language.Haskellish as LH
import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Control.Monad.State
import Data.Map as Map 
import Sound.OSC as H
import Data.Text as T
import Data.Bifunctor
import Data.Tempo
import Data.Time



type H = Haskellish GlobalMaterial

-- data Program = Program [Layer] GlobalMaterial
-- type Program = ([Layer], GlobalMaterial)

-- f :: (Style, Intrument)
-- f cumbia piano

parseLang :: String -> Either String ([Layer], GlobalMaterial)
parseLang s = (f . parseExp) $ ( "do {" ++ s ++ "}" )
  where
    f (ParseOk x) = runHaskellish layers defaultGlobalMaterial x -- Either String (a, st)
    f (ParseFailed l s) = Left s

layers :: H [Layer]
layers =  listOfDoStatements statement

statement :: H Layer
statement =  parseLayer <|> globalStatement -- programaLiteral <|> programaConEstiloEInst

globalStatement :: H Layer -- should return []
globalStatement = do
  f <- progressionToGm -- globalMaterialFunctions --
  st <- get
  let newState = f -- f st
  put newState
  return emptyLayer

pianoAsLayer :: Layer
pianoAsLayer = Layer (defaultStyle, piano)

-- globalMaterialFunctions :: H (GlobalMaterial -> GlobalMaterial)
-- globalMaterialFunctions = pitchToGm

progressionToGm :: H GlobalMaterial
progressionToGm = progressionToGm' <*> progressionParser

progressionToGm' :: H ([Harmony] -> GlobalMaterial)
progressionToGm' = setNewGlobalMaterial <$ reserved "harmonia"

progressionParser :: H [Harmony]
progressionParser = list harmoniaParser

harmoniaParser :: H Harmony
harmoniaParser = harmoniaParser' <*> rationalOrInteger

harmoniaParser' :: H (Rational -> Harmony)
harmoniaParser' = harmoniaParser'' <*> rationalOrInteger

harmoniaParser'' :: H (Rational -> Rational -> Harmony)
harmoniaParser'' = harmoniaParser''' <*> rationalOrInteger

harmoniaParser''' :: H (Rational -> Rational -> Rational -> Harmony)
harmoniaParser''' = harmoniaParser'''' <*> chordTypeParser

harmoniaParser'''' :: H (ChordType -> Rational -> Rational -> Rational -> Harmony)
harmoniaParser'''' = do
  p <- pitchParser
  return $ setHarmony p

setNewGlobalMaterial :: [Harmony] -> GlobalMaterial
setNewGlobalMaterial hs = GlobalMaterial { harmony = hs }

setHarmony :: Pitch -> ChordType -> Rational -> Rational -> Rational -> Harmony
setHarmony p t m s e = Harmony (Chord p t) (m, s) (m, e)

pitchParser :: H Pitch
pitchParser =
  c <$ reserved "c"     <|>
  cs <$ reserved "c#"   <|>
  cs <$ reserved "db"   <|>
  d <$ reserved "d"     <|>
  ds <$ reserved "d#"   <|>
  ds <$ reserved "eb"   <|>
  e <$ reserved "e"     <|>
  f <$ reserved "f"     <|>
  fs <$ reserved "f#"   <|>
  fs <$ reserved "gb"   <|>
  g <$ reserved "g"     <|>
  gs <$ reserved "g#"   <|>
  gs <$ reserved "ab"   <|>
  a <$ reserved "a"     <|>
  as <$ reserved "a#"   <|>
  as <$ reserved "bb"   <|>
  b <$ reserved "b"

chordTypeParser :: H ChordType
chordTypeParser =
  major <$ reserved "maj"      <|> -- no capital letters?
  minor <$ reserved "min"      <|>
  major7 <$ reserved "maj7"    <|>
  minor7 <$ reserved "min7"    <|>
  dom <$ reserved "dom"        <|>
  fifths <$ reserved "quintas" <|>
  sus4 <$ reserved "sus4"      <|>
  sus2 <$ reserved "sus2"      <|>
  aug <$ reserved "aug"        <|>
  dim <$ reserved "dim"

parseLayer :: H Layer
parseLayer = parseStyleInstAsLayer <|> parseInstAsLayer

inst :: H Instrument
inst =
  piano <$ reserved "piano" <|>
  bajo <$ reserved "bajo" <|>
  guira <$ reserved "guira"

parseInstAsLayer :: H Layer
parseInstAsLayer = do
  x <- inst
  return $ instAsLayer x

-- Convert an instrument to a Layer using a defaultStyle
instAsLayer :: Instrument -> Layer
instAsLayer i = Layer (defaultStyle, i)

--or
parseStyleInstAsLayer :: H Layer
parseStyleInstAsLayer = parseStyleAsFunction <*> inst

estilo :: H S.Style
estilo = cumbia <$ reserved "cumbia"

-- provide that missing function similar to asignarEstiloEInst
parseStyleAsFunction :: H (Instrument -> Layer)
parseStyleAsFunction = do
  s <- estilo
  return $ \i -> styleAndInstrumentToLayer s i

-- make sense but should exist in the language
styleAndInstrumentToLayer ::  S.Style -> Instrument -> Layer
styleAndInstrumentToLayer s i = Layer (s, i)


-- so I can do :
-- cumbia piano
-- (noDownBeats cumbia) piano

-- -- GlobalMaterial
-- clave dosTres --gets parsed 1st that changes the state
-- harmony Cmaj Emin -- 2nd, and changes the state
-- clave tresDos -- 3rd -- and changes the state


-- statement :: H [Layer]
-- statement = programaLiteral -- globalStatement -- <|> layer -- programaLiteral <|> programaConEstiloEInst
--
-- globalStatement :: H [Layer] -- should return []
-- globalStatement = do
--   f <- globalMaterialFunctions --
--   st <- get
--   let newState = f st
--   put newState
--   return []
--
-- globalMaterialFunctions :: H (GlobalMaterial -> GlobalMaterial)
-- globalMaterialFunctions = do
--   reserved "harmonia:"
--   return $ GlobalMaterial {
--                            harmony = [Harmony (Chord 65 major) (1, 0) (1, 1)]
--                           }
--
  -- parsers that change the global material in different ways
  --with choices, e.g. for harmony and for clave
  --punctual x = 3, where x is now globally available

--

--

--
-- programaConEstiloEInst :: H Program
-- programaConEstiloEInst = asignarEstiloEInst <$> estilo <*> inst
-- --
-- -- f :: H (Instrument -> Program)
-- -- f = setStyle <$ reserved "cumbia"
--



--   getEvents :: GlobalMaterial -> Style -> Tempo -> BeginWindowTime -> EndWindowTime -> State InstrumentState [Event]
-- render :: (GlobalMaterial,Style,Instrument) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime,Map Text Datum)]
-- runState :: State s a -> s -> (a, s) -- as soon as the state is meaningful I should stop discarding it.
--check Tidal.params for looking at the available params for webdirt
render :: ([Layer], GlobalMaterial) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime, Map Text Datum)]
render (ls, gm) tempo iw ew = Prelude.concat $ fmap (\l -> render' (l, gm) tempo iw ew) ls

  -- data Layer = Layer (S.Style, Instrument)
  -- -- data Program = Program [Layer] GlobalMaterial
  -- type Program = ([Layer], GlobalMaterial)

render' :: (Layer, GlobalMaterial) -> Tempo -> UTCTime -> UTCTime -> [(UTCTime, Map Text Datum)]
render' ((Layer (s, i)), gm) tempo iw ew = fst $ runState x emptyInstrumentState --this should be another argument to my render function
  where
     x = getEvents i gm s tempo iw ew
