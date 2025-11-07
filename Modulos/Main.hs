module Main where

import Objeto (Objeto(..))
import Types (TipoCarro(..), MunicionTipo(..), Vector, Size, Position, Angle, Distance, Value(..))
import Unidad
import Bot (botEstrategico, BotAction(..))
import Physics (updatePosition, vectorNulo, normalize, distanceBetween)
import Collisions (CollisionEvent(..), checkCollisions)
import GameTypes
import Torneos

--------------------------------
-- MAIN
--------------------------------
main :: IO ()
main = do
  putStrLn "Iniciando Haskell Tank Game..."
  putStrLn "¿Cuántos torneos deseas ejecutar?"
  numTorneos <- readLn
  ejecutarTorneos numTorneos