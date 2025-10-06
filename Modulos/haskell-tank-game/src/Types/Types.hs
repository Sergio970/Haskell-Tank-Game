module Types.Types where

import qualified Data.Map.Strict as Map

type Vector = (Float, Float)
type Position = (Float, Float)
type Point = (Float, Float)
type Angle = Float
type Distance = Float
type Size = (Float, Float)

-- Enums compartidos
data TipoCarro    = Ligero | Pesado | Cazacarros
  deriving (Show, Eq)

data MunicionTipo = AP | AE
  deriving (Show, Eq)

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