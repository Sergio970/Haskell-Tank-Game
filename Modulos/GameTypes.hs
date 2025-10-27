module GameTypes where
import Unidad (Mundo)

data GameState = GameState
  { mundo  :: Mundo
  , tiempo :: Float
  , ronda  :: Int
  }
