module GameTypes where

import Unidad (Mundo)
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..), Modifiers(..))

data Modo = Menu | Jugando deriving (Eq, Show)

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
  } deriving (Show)

-- Event handler básico para el menú
handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) gs =
  if modo gs == Menu then pure gs { modo = Jugando } else pure gs

-- Reiniciar (R) cuando estás jugando: vuelve al menú con mundo nuevo
handleEvent (EventKey (Char 'r') Down _ _) gs =
  if modo gs == Jugando
    then pure gs { modo = Menu }
    else pure gs

-- Ignorar todo lo demás
handleEvent _ gs = pure gs
