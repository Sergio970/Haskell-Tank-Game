module GameTypes where

import Unidad (Mundo)
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..), Modifiers(..))
import System.Exit (exitSuccess)
import qualified Data.Map.Strict as Map


data Modo = Menu | Jugando | Victoria Int | FinTorneos deriving (Eq, Show)

data ExplosionType = ImpactExplosion | DeathExplosion
  deriving (Show, Eq)

data Explosion = Explosion
  { explosionPos  :: (Float, Float)
  , explosionTime :: Float
  , explosionType :: ExplosionType
  } deriving (Show)


data GameState statsBot statsTorneo = GameState
  { mundo      :: Mundo
  , tiempo     :: Float
  , ronda      :: Int
  , modo       :: Modo
  , explosions :: [Explosion]
  , bgIndex    :: Int
  , proximoMeteoritoId  :: Int
  , tiempoProxMeteorito :: Float
  , actualTorneo :: Int
  , torneosSobrantes :: Int
  , tiempoEsperaVictoria :: Float
  , estadisticasBots :: Map.Map Int statsBot
  , historialTorneos :: [statsTorneo]
  } deriving (Show)


-- Event handler básico
handleEventBasico :: Event -> GameState sb st -> IO (GameState sb st)
handleEventBasico (EventKey (SpecialKey KeyEnter) Down _ _) gs =
  if modo gs == Menu then pure gs { modo = Jugando } else pure gs

handleEventBasico (EventKey (Char 'f') Down _ _) gs =
  if modo gs == Menu
    then pure gs { bgIndex = if bgIndex gs == 1 then 2 else 1 }
    else pure gs

handleEventBasico (EventKey (Char 'p') Down _ _) gs =
  if modo gs == Jugando
    then pure gs { modo = Menu }
    else pure gs

handleEventBasico (EventKey (Char 'r') Down _ _) gs = 
  pure gs

handleEventBasico (EventKey (SpecialKey KeyEsc) Down _ _) gs = do
  putStrLn "Saliendo del juego..."
  exitSuccess

handleEventBasico _ gs = pure gs

