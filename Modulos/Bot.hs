module Bot where

import Data.List (minimumBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Unidad
import Types (TipoCarro(..))
import Physics (distanceBetween, normalize, rad2deg)
import qualified Data.Map.Strict as Map

-- Acciones que puede tomar el bot
data BotAction = DispararA Int | Mover (Float, Float) | Girar Float
  deriving (Show, Eq)

-- ==========================================================
-- Estrategia principal según tipo de carro
-- ==========================================================

botEstrategico :: Mundo -> CarroCombate -> Maybe [BotAction]
botEstrategico mundo carro
  | energia carro <= 0 = Nothing
  | otherwise =
      case tipoCarro carro of
        Ligero     -> Just (estrategiaGenerica mundo carro)
        Cazacarros -> Just (estrategiaGenerica mundo carro)
        Pesado     -> Just (estrategiaGenerica mundo carro)

-- ==========================================================
-- Estrategia común (ligero, pesado, cazacarros)
-- ==========================================================

estrategiaGenerica :: Mundo -> CarroCombate -> [BotAction]
estrategiaGenerica mundo carro =
  case enemigoMasCercano carro (carros mundo) of
    Just enemigo ->
      let (dx, dy) = direccionHacia carro enemigo
          apuntar  = apuntarHacia carro enemigo
          puedeVer = veEntre carro enemigo (carros mundo)
      in if puedeVer
           then [apuntar, DispararA (carroId enemigo)]
           else [Mover (dx, dy)]
    Nothing -> [Girar 2]

-- ==========================================================
-- Utilidades del bot
-- ==========================================================

enemigoMasCercano :: CarroCombate -> [CarroCombate] -> Maybe CarroCombate
enemigoMasCercano carro todos =
  let enemigos = filter (\e -> team e /= team carro && energia e > 0) todos
  in if null(enemigos)
       then Nothing
       else Just (minimumBy (comparing (distanceBetween (posicionCarro carro) . posicionCarro)) enemigos)

-- Calcular el ángulo para girar el cañón
apuntarHacia :: CarroCombate -> CarroCombate -> BotAction
apuntarHacia carro objetivo =
  let (x1, y1) = posicionCarro carro
      (x2, y2) = posicionCarro objetivo
      angObjetivo = rad2deg (atan2 (y2 - y1) (x2 - x1))
  in Girar (angObjetivo - getdireccionCanon carro)

-- Calcular vector de movimiento hacia un objetivo
direccionHacia :: CarroCombate -> CarroCombate -> (Float, Float)
direccionHacia carro objetivo =
  let (x1, y1) = posicionCarro carro
      (x2, y2) = posicionCarro objetivo
      (dx, dy) = (x2 - x1, y2 - y1)
  in normalize (dx, dy)
