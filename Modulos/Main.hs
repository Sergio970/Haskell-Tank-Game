module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad (foldM)


import Objeto (Objeto(..))
import Types (TipoCarro(Ligero, Pesado, Cazacarros), MunicionTipo(AP, AE), Vector)

import Unidad

import Bot (botCombinado, BotAction(..))
import Collisions (CollisionEvent(..), checkCollisions)
import Physics (polygonsIntersectSAT)

-- ============================================================
-- =========        Funciones de ejemplo              =========
-- ============================================================

-- Tripulación totalmente viva
tripulacionViva :: Tripulacion
tripulacionViva = Tripulacion Vivo Vivo Vivo Vivo Vivo

-- Crear Carros de ejemplo con ID incluido (usando Objeto + CarroAtributos)
carroLigero :: Int -> Int -> (Float,Float) -> CarroCombate
carroLigero cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto
      { posicion  = pos
      , direccion = 0.0
      , velocidad = (10.0, 0.0)
      , tamano    = (3.0, 3.0)
      , atributos = CarroAtributos
          { carroIdE        = cid
          , equipoE         = equipo
          , tipoCarroE      = Ligero
          , energiaE        = 100
          , blindajeE       = blindajeBase Ligero
          , alcanceVisionE  = visionBase Ligero
          , alcanceRadioE   = radioBase Ligero
          , tripulacionE    = tripulacionViva
          , municionesE     =
              replicate 3 (Municion AP 75.0 Map.empty) ++
              replicate 2 (Municion AE 120.0 Map.empty)
          , cadenciaE       = 1.0
          , precisionBaseE  = 0.9
          , memoriaCarroE   = memoriaLigero
          }
      }

carroPesado :: Int -> Int -> (Float,Float) -> CarroCombate
carroPesado cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto
      { posicion  = pos
      , direccion = 0.0
      , velocidad = (4.0, 0.0)
      , tamano    = (5.0, 5.0)
      , atributos = CarroAtributos
          { carroIdE        = cid
          , equipoE         = equipo
          , tipoCarroE      = Pesado
          , energiaE        = 300
          , blindajeE       = blindajeBase Pesado
          , alcanceVisionE  = visionBase Pesado
          , alcanceRadioE   = radioBase Pesado
          , tripulacionE    = tripulacionViva
          , municionesE     =
              replicate 4 (Municion AP 120.0 Map.empty) ++
              replicate 3 (Municion AE 150.0 Map.empty)
          , cadenciaE       = 2.0
          , precisionBaseE  = 0.8
          , memoriaCarroE   = memoriaPesado
          }
      }

cazacarros :: Int -> Int -> (Float,Float) -> CarroCombate
cazacarros cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto
      { posicion  = pos
      , direccion = 0.0
      , velocidad = (7.0, 0.0)
      , tamano    = (4.0, 4.0)
      , atributos = CarroAtributos
          { carroIdE        = cid
          , equipoE         = equipo
          , tipoCarroE      = Cazacarros
          , energiaE        = 180
          , blindajeE       = blindajeBase Cazacarros
          , alcanceVisionE  = visionBase Cazacarros
          , alcanceRadioE   = radioBase Cazacarros
          , tripulacionE    = tripulacionViva
          , municionesE     =
              replicate 3 (Municion AP 110.0 Map.empty) ++
              replicate 2 (Municion AE 140.0 Map.empty)
          , cadenciaE       = 1.5
          , precisionBaseE  = 0.85
          , memoriaCarroE   = memoriaCazacarros
          }
      }

-- Instancias de ejemplo
c1 :: CarroCombate
c1 = carroLigero 1 1 (10,10)

c2 :: CarroCombate
c2 = carroPesado 2 1 (40,20)

c3 :: CarroCombate
c3 = cazacarros 3 2 (200,20)

-- Mundo de ejemplo con dos equipos
mundoEjemplo :: Mundo
mundoEjemplo = Mundo
  { carros      = [c1, c2, c3]
  , proyectiles = []
  , tamanoMundo = (1000, 1000)
  , memoria     = memoriaMundo
  }

-- ============================================================
-- Ejemplo de uso (simulación simplificada)
-- ============================================================

ataqueInstantaneo :: Int -> CarroCombate -> CarroCombate -> IO (CarroCombate, Maybe CarroCombate)
ataqueInstantaneo pid atacante objetivo = do
  case procesarDisparo pid atacante objetivo of
    Nothing -> return (atacante, Nothing)
    Just (atacante', objetivoDaniado, dmg) -> do
      objetivoFinal <- aplicarDanioConMuerteAleatoria dmg objetivoDaniado
      return (atacante', Just objetivoFinal)

-- Función PURA: procesa todo el disparo e impacto
procesarDisparo :: Int -> CarroCombate -> CarroCombate 
                -> Maybe (CarroCombate, CarroCombate, Int)
procesarDisparo pid atacante objetivo = do
  (proj, atacante') <- dispararA pid atacante objetivo
  let m = municionProyectil proj
  objetivoAfter <- aplicarImpactoDirecto m objetivo
  let dmg = calcularDanioRecibido objetivo objetivoAfter
  return (atacante', objetivoAfter, dmg)

-- Función PURA auxiliar para calcular daño recibido
calcularDanioRecibido :: CarroCombate -> CarroCombate -> Int
calcularDanioRecibido antes despues =
  energia antes - energia despues 


-- ============================================================
-- Nota:
-- - Valores (penetración, daños, multiplicadores) son ejemplos y se pueden afinar.
-- - La simulación de trayectoria y tiempos entre disparos no está detallada; el ejemplo
--   `ataqueInstantaneo` asume impacto inmediato para facilitar presentación.
-- - Para ejecutar todo en GHCi: cargar el archivo y usar `mostrarVisionDe mundoEjemplo`
--   o probar `ataqueInstantaneo` entre carros de `mundoEjemplo`.
-- ============================================================

data GameState = GameState
  { mundo :: Mundo
  , tiempo :: Float
  , ronda :: Int
  }

main :: IO ()
main = do
  putStrLn "Hola, Haskell Tank Game!"
  -- aquí puedes iniciar tu mundo, crear carros, etc. 

updateGame :: Float -> GameState -> IO GameState
updateGame dt gs = do
  nuevoMundo <- bucleTorneo dt (mundo gs)
  return gs { mundo = nuevoMundo }

-- ============================================================
-- Bucle principal del juego (lógica del torneo)
-- ============================================================

bucleTorneo :: Float -> Mundo -> IO Mundo
bucleTorneo dt mundoActual = do
  -- Cada bot decide sus acciones según la situación actual del mundo
  let decisiones = mapMaybe (\car -> fmap (\x -> (car, x)) (botCombinado mundoActual car)) (carros mundoActual)

  -- Se aplican todas las acciones (mover, disparar, recargar, etc.)
  mundoTrasAcciones <- foldM aplicarAccionesBot mundoActual decisiones

  -- Se detectan colisiones entre proyectiles y tanques
  let eventos = checkCollisions (carros mundoTrasAcciones) (proyectiles mundoTrasAcciones)

  -- Se aplican los efectos de esas colisiones (daño, destrucción, etc.)
  mundoFinal <- aplicarEventos eventos mundoTrasAcciones

  return mundoFinal

-- Aplicar las acciones que devuelve cada bot
aplicarAccionesBot :: Mundo -> (CarroCombate, [BotAction]) -> IO Mundo
aplicarAccionesBot m (carro, acts) = foldM aplicar m acts
  where
    aplicar mundo (DispararA objetivoId) =
      case buscarCarro objetivoId (carros mundo) of
        Just obj ->
          let pid = length (proyectiles mundo) + 1
          in case dispararA pid carro obj of
              Just (proj, carroSinMun) ->
                return $ agregarProyectil proj
                       $ reemplazarCarro carroSinMun (carros mundo) mundo
              Nothing -> return mundo
        Nothing -> return mundo
    aplicar mundo (Mover v) = return $ mundo { carros = map (moverCarro v) (carros mundo) }
    aplicar mundo _ = return mundo

-- Aplica los efectos de las colisiones
aplicarEventos :: [CollisionEvent] -> Mundo -> IO Mundo
aplicarEventos eventos m = foldM procesarEvento m eventos
  where
    procesarEvento mundo (RobotHit carroId pid) = do
      case (buscarCarro carroId (carros mundo), buscarProyectil pid (proyectiles mundo)) of
        (Just car, Just proj) -> do
          let mun = municionProyectil proj
          case aplicarImpactoDirecto mun car of
            Just carDaño -> do
              carFinal <- aplicarDanioConMuerteAleatoria (danioEstim mun) carDaño
              return $ removerProyectil pid
                     $ reemplazarCarro carFinal (carros mundo) mundo
            Nothing -> return $ removerProyectil pid mundo
        _ -> return mundo
    procesarEvento mundo (RobotRobot _ _) = return mundo

-- Helpers auxiliares
-- safeHead es polimórfica
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- buscarCarro: busca por id en una lista de CarroCombate
buscarCarro :: Int -> [CarroCombate] -> Maybe CarroCombate
buscarCarro cid = safeHead . filter ((== cid) . carroId)

-- buscarProyectil: busca por id en una lista de Proyectil
buscarProyectil :: Int -> [Proyectil] -> Maybe Proyectil
buscarProyectil pid = safeHead . filter ((== pid) . proyectilId)

-- reemplazarCarro: sustituye (o añade) un carro en la lista dentro del Mundo
reemplazarCarro :: CarroCombate -> [CarroCombate] -> Mundo -> Mundo
reemplazarCarro c cs m = m { carros = c : filter ((/= carroId c) . carroId) cs }

-- moverCarro: aplica un vector de desplazamiento al carro
moverCarro :: Vector -> CarroCombate -> CarroCombate
moverCarro (dx, dy) c =
  let (x, y) = posicionCarro c
  in c { posicion = (x + dx, y + dy) }