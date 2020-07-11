module Sound.Seis8s.LayerState where

import Control.Monad.State
import Sound.Seis8s.Harmony

import Data.Maybe
import Data.Fixed
import Data.List as List


--this is about what do I need to keep track of ? -- better to keep it empty for now

data LayerState = LayerState deriving (Show)

emptyLayerState = LayerState
-- thing :: State LayerState [Event]
-- thing = do
--   s <- get
--   return s

c1 = Chord 60 major (0, 0.5)
c2 = Chord 64 minor (0.5, 1)
-- recibe una
makeVoicing :: Chord -> Chord -> [Pitch]
makeVoicing newC oldC = do
  let newC' = generateChord newC
  let oldC' = generateChord oldC
  let notasComunes'' = notasComunes' newC' oldC' -- [Double]
  let notasNoComunes' = notasConMovimientoCercano' (notasNoComunes newC' oldC') oldC'
  -- let notasNoComunes = notasNoComunes t2 t1
  notasNoComunes'

notasComunes' :: [Pitch] -> [Pitch] -> [Pitch]
notasComunes' xs ys = List.sort $ concat $ fmap (\x -> notasComunes x ys) xs


notasConMovimientoCercano' :: [Pitch] -> [Pitch] -> [Pitch]
notasConMovimientoCercano' news olds = concat $ fmap (\n -> notasConMovimientoCercano n olds) news

-- let notasNoComunes = notasNoComunes t2 t1
notasConMovimientoCercano :: Pitch -> [Pitch] -> [Pitch]
notasConMovimientoCercano new olds = catMaybes $ fmap (\o -> comparaNotas new o) olds


-- 1. old - new >= 0 then new, then old vs a [new]
-- 2. sort and get 1st element of list
-- 3. [1, 12, 48, 71]
-- regresa el equivalente en picth para el numero mas pequenio

-- 1. para el numero cuya distancia sea la minima en relacion a old devuelve el numero new
-- comparaNotas' :: [Pitch] -> Pitch -> [Pitch]
-- comparaNotas' news old = sort $ catMaybes $ fmap (\n -> comparaNotas n old) news

comparaNotas :: Pitch -> Pitch -> Maybe Pitch
comparaNotas x old = do
  let ex = (x + 12)
  let is = (x - 12)
  let es = (x + 12) + 12
  let xs = [x, is .. 0] ++ [ex, es .. 127] -- [Pitch]
  let news' = xs
  let distancia = filter (>= 0) $ fmap ((-) old) news' -- [1 , 12 ...]
  let minima = minimum distancia
  listaAValor $ catMaybes $ fmap (\x' -> if ((old - x') == minima) then (Just x') else Nothing) news'

listaAValor :: [Pitch] -> Maybe Pitch
listaAValor [x] = Just x
listaAValor _ = Nothing
  -- [60, 64, 67] [64, 67, 71]
-- comunes [64, 67]
-- no comunes [60, 71], this should be [60, 59]
-- escoge el 71 que este más cerca de 60
-- escoge el numero que sea < que la octava siguiente de dicho numero [...]
-- (\x -> (x < ( + 12) == x)

notasComunes :: Pitch -> [Pitch] -> [Double]
notasComunes x ys = do
  let ex = (x + 12)
  let is = (x - 12)
  let es = (x + 12) + 12
  let xs = [x, is .. 0] ++ [ex, es .. 127] -- [Pitch]
  replicate (length $ catMaybes $ fmap (\x' -> compararElem x' ys) xs) x

compararElem :: Double -> [Double] -> Maybe Bool
compararElem x ys
  |elem x ys = Just True
  |otherwise = Nothing

notasNoComunes :: [Pitch] -> [Pitch] -> [Double]
notasNoComunes xs ys = catMaybes $ fmap (\x -> compararElemNoComun x ys) xs

compararElemNoComun :: Double -> [Double] -> Maybe Double
compararElemNoComun x ys
  |not (elem x ys) = Just x
  |otherwise = Nothing
