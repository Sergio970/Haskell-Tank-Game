module Main where

import qualified Data.Map.Strict as Map

import Types.Objeto (Objeto(..))

import Entities.Carro
  ( CarroCombate, CarroAtributos(..), TipoCarro(..)
  , visionBase, blindajeBase, radioBase
  , memoriaLigero, memoriaPesado, memoriaCazacarros, memoriaMundo
  , aplicarEfectosTripulacion, carrosVistosPor
  , aplicarDanioConMuerteAleatoria
  )

import Entities.Municion
  ( Municion(..), MunicionTipo(..), Proyectil(..)
  , dispararA, aplicarImpactoDirecto, municionProyectil
  )

import Entities.Tripulacion
  ( Tripulacion(..), EstadoTripulante(..)
  )

import Entities.Mundo (Mundo(..))

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

-- Simular que a ataca b: dispara, se crea proyectil y al impactar aplicamos daño (impacto instantáneo)
ataqueInstantaneo :: Int -> CarroCombate -> CarroCombate -> IO (CarroCombate, Maybe CarroCombate)
ataqueInstantaneo pid atacante objetivo =
  case dispararA pid atacante objetivo of
    Nothing -> return (atacante, Nothing) -- sin munición
    Just (proj, atacante') -> do
      let m             = municionProyectil proj
          objetivoAfter = aplicarImpactoDirecto m objetivo
          -- acceso vía 'atributos' (Objeto + CarroAtributos)
          dmg           = energiaE (atributos objetivo) - energiaE (atributos objetivoAfter)
      objetivoFinal <- aplicarDanioConMuerteAleatoria dmg objetivoAfter
      return (atacante', Just objetivoFinal)

-- Función para mostrar lo que ve cada carro en un mundo
mostrarVisionDe :: Mundo -> IO ()
mostrarVisionDe mundo = mapM_ printVision (carros mundo)
  where
    printVision c = do
      let vistos      = carrosVistosPor c mundo
          eq          = equipoE (atributos c)
          tipo        = tipoCarroE (atributos c)
          vistosTipos = map (tipoCarroE . atributos) vistos
      putStrLn $
        "Carro (team " ++ show eq ++ ", tipo " ++ show tipo ++
        ") ve " ++ show vistosTipos


-- ============================================================
-- Nota:
-- - Valores (penetración, daños, multiplicadores) son ejemplos y se pueden afinar.
-- - La simulación de trayectoria y tiempos entre disparos no está detallada; el ejemplo
--   `ataqueInstantaneo` asume impacto inmediato para facilitar presentación.
-- - Para ejecutar todo en GHCi: cargar el archivo y usar `mostrarVisionDe mundoEjemplo`
--   o probar `ataqueInstantaneo` entre carros de `mundoEjemplo`.
-- ============================================================