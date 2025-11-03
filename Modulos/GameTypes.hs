module GameTypes where

import Unidad (Mundo)
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..), Modifiers(..))

data Modo = Menu | Jugando | Victoria Int deriving (Eq, Show)

data ExplosionType = ImpactExplosion | DeathExplosion
  deriving (Show, Eq)

data Explosion = Explosion
  { explosionPos  :: (Float, Float)
  , explosionTime :: Float
  , explosionType :: ExplosionType
  } deriving (Show)

data GameState = GameState
  { mundo      :: Mundo
  , tiempo     :: Float
  , ronda      :: Int
  , modo       :: Modo
  , explosions :: [Explosion]
  , bgIndex    :: Int          -- 1 o 2, selección de fondo
  , proximoMeteoritoId  :: Int
  , tiempoProxMeteorito :: Float
  } deriving (Show)

-- Event handler básico para el menú
handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) gs =
  if modo gs == Menu then pure gs { modo = Jugando } else pure gs

-- Alternar fondo en el menú con tecla 'F'
handleEvent (EventKey (Char 'f') Down _ _) gs =
  if modo gs == Menu
    then pure gs { bgIndex = if bgIndex gs == 1 then 2 else 1 }
    else pure gs

-- Pausar (P) cuando estás jugando: vuelve al menú
handleEvent (EventKey (Char 'p') Down _ _) gs =
  if modo gs == Jugando
    then pure gs { modo = Menu }
    else pure gs

-- Reiniciar partida con 'R' 
handleEvent (EventKey (Char 'r') Down _ _) gs = do
  return gs  
-- Ignorar todo lo demás
handleEvent _ gs = pure gs
