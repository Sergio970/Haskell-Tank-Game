module Entities.Mundo where

import qualified Data.Map.Strict as Map
import Entities.Carro
import Entities.Municion
import Entities.Tripulacion

data Mundo = Mundo {
    carros      :: [CarroCombate],
    proyectiles :: [Proyectil],
    tamanoMundo :: Size,
    memoria     :: Memory
} deriving (Show)

-- Funciones para gestionar el estado del mundo
agregarCarro :: CarroCombate -> Mundo -> Mundo
agregarCarro carro mundo = mundo { carros = carro : carros mundo }

agregarProyectil :: Proyectil -> Mundo -> Mundo
agregarProyectil proyectil mundo = mundo { proyectiles = proyectil : proyectiles mundo }

removerCarro :: Int -> Mundo -> Mundo
removerCarro cid mundo = mundo { carros = filter ((/= cid) . carroId) (carros mundo) }

removerProyectil :: Int -> Mundo -> Mundo
removerProyectil pid mundo = mundo { proyectiles = filter ((/= pid) . proyectilId) (proyectiles mundo) }