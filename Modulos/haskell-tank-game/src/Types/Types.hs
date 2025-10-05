module Types.Types where

import System.Random (randomRIO)
import Data.List (elemIndex, delete, nub)
import qualified Data.Map.Strict as Map
import Data.List (tails)
import Entities.Carro (TipoCarro)
import Entities.Municion (MunicionTipo)
import Entities.Mundo (Mundo)


type Vector = (Float, Float)
type Position = (Float, Float)
type Point = (Float, Float)
type Angle = Float
type Distance = Float
type Size = (Float, Float)

-- Tipos de valores que puede guardar la memoria
data Value
  = VInt Int
  | VFloat Float
  | VBool Bool
  | VString String
  | VPoint Point
  | VSize Size
  | VTipoCarro TipoCarro
  | VMunicionTipo MunicionTipo
  deriving (Show, Eq)

-- Tipo de la memoria: un diccionario de clave -> valor
type Memory = Map.Map String Value