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
  putStrLn "=== Haskell Tank Game ==="
  putStrLn "¿Cuántos torneos deseas ejecutar?"
  numTorneos <- readLn
  
  -- Crear estado inicial del primer torneo
  mundoInicial <- crearNuevoMundo  -- Tu función de generación
  
  let estadoInicial = GameState
        { mundo = mundoInicial
        , tiempo = 0.0
        , ronda = 1
        , modo = Jugando  -- ← Empieza directo, sin menú
        , explosions = []
        , bgIndex = 1
        , proximoMeteoritoId = 100
        , tiempoProxMeteorito = 2.0
        , torneoActual = 1
        , torneosRestantes = numTorneos - 1  -- Restamos el primero
        , tiempoEsperaVictoria = 0.0
        }
  
  -- Iniciar loop de Gloss
  playIO
    (InWindow "Tank Game" (800, 600) (100, 100))
    white
    60
    estadoInicial
    renderGame      -- Tu función de rendering
    handleEvent     -- Manejador de eventos
    updateGame      -- Tu función de actualización
