module Objeto
  ( Objeto(..)
  ) where
import Types (Position, Vector, Angle, Size)

-- Tipo genérico para objetos del juego
data Objeto a = Objeto
  { posicion       :: Position
  , direccion      :: Angle
  , direccionCanon :: Angle
  , velocidad      :: Vector
  , tamano         :: Size
  , atributos      :: a
  } deriving (Show, Eq)
