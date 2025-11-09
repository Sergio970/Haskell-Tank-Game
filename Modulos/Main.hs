module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Objeto (Objeto(..))
import Types (TipoCarro(..), MunicionTipo(..), Vector, Size, Position, Angle, Distance, Value(..))
import Unidad
import Bot (botEstrategico, BotAction(..))
import Physics (updatePosition, vectorNulo, normalize, distanceBetween)
import Collisions (CollisionEvent(..), checkCollisions)
import GameTypes
import Torneos (mundoAleatorio, updateGame, mundoDesdeConfig, rondasDesdeConfig)
import Rendering (renderGame)

--------------------------------
-- MAIN
--------------------------------

main :: IO ()
main = do
  putStrLn "=== Haskell Tank Game ==="
  -- ...existing code...
  rondas <- rondasDesdeConfig
  -- Crear estado inicial del primer torneo
  mundoInicial <- mundoDesdeConfig 
  
  let estadoInicial = GameState
        { mundo = mundoInicial
        , tiempo = 0.0
        , ronda = 1
        , modo = Jugando  -- Empieza directo, sin menú
        , explosions = []
        , bgIndex = 1
        , proximoMeteoritoId = 100
        , tiempoProxMeteorito = 2.0
        , actualTorneo = 1
        , torneosSobrantes = max 0 (rondas - 1)  -- desde config
        , tiempoEsperaVictoria = 0.0
        }
  
  -- Iniciar loop de Gloss
  playIO
    (InWindow "Tank Game" (800, 600) (100, 100))
    white
    60
    estadoInicial
    renderGame      -- función de rendering
    handleEvent     -- Manejador de eventos
    updateGame      -- función de actualización
