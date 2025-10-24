module Bot where

import Unidad
import Types
import Physics

import Data.Maybe (listToMaybe)

-- ============================================================
-- 5. DSL para acciones del Bot
-- ============================================================

-- Acciones que un bot puede realizar
data BotAction
    = Mover Vector              -- Moverse en cierta dirección
    | Girar Angle               -- Girar el carro
    | DispararA Int             -- Disparar al ID de otro carro
    | Recargar                  -- Recargar munición
    | Esperar                   -- No hacer nada (paso de turno)
    deriving (Show, Eq)

-- ============================================================
-- Bot principal: comportamiento diferente según tipo de tanque
-- ============================================================
botEstrategico :: Mundo -> CarroCombate -> Maybe [BotAction]
botEstrategico mundo carro =
  case tipoCarro carro of
    Ligero      -> Just (estrategiaLigero mundo carro)
    Cazacarros  -> Just (estrategiaCazacarros mundo carro)
    Pesado      -> Just (estrategiaPesado mundo carro)

-- ============================================================
-- Estrategia del tanque ligero
-- ============================================================
estrategiaLigero :: Mundo -> CarroCombate -> [BotAction]
estrategiaLigero mundo carro =
  case listToMaybe (filter (\e -> e /= carro
                                 && veEntre carro e (carros mundo)
                                 && team e /= team carro)
                           (carros mundo)) of
    Just enemigo -> [apuntarHacia carro enemigo, DispararA (carroId enemigo)]
    Nothing      -> [Mover (direccionAleatoria carro)]

-- ============================================================
-- Estrategia del cazacarros
-- ============================================================
estrategiaCazacarros :: Mundo -> CarroCombate -> [BotAction]
estrategiaCazacarros mundo carro =
  case listToMaybe (filter (\e -> e /= carro
                                 && veEntre carro e (carros mundo)
                                 && team e /= team carro)
                           (carros mundo)) of
    Just enemigo -> [apuntarHacia carro enemigo, DispararA (carroId enemigo)]
    Nothing      -> [Girar 15]  -- patrulla girando cuando no ve enemigos

-- ============================================================
-- Estrategia del tanque pesado
-- ============================================================
estrategiaPesado :: Mundo -> CarroCombate -> [BotAction]
estrategiaPesado mundo carro =
  let enemigosVistos = filter (\e -> e /= carro
                                   && veEntre carro e (carros mundo)
                                   && team e /= team carro)
                              (carros mundo)
  in case listToMaybe (filter (\e -> energia e < 100) enemigosVistos) of
      Just debil -> [apuntarHacia carro debil, DispararA (carroId debil)]
      Nothing    -> [Mover (direccionAleatoria carro)]

-- ============================================================
-- Funciones auxiliares
-- ============================================================

-- Devuelve un vector direccional simple basado en el ID del carro
direccionAleatoria :: CarroCombate -> Vector
direccionAleatoria carro =
  let ang = fromIntegral (carroId carro * 37) * 0.5
  in (cos ang, sin ang)

-- Calcula la acción de giro hacia otro tanque
apuntarHacia :: CarroCombate -> CarroCombate -> BotAction
apuntarHacia carro objetivo =
  let (x1, y1) = posicionCarro carro
      (x2, y2) = posicionCarro objetivo
      anguloActual  = direccionCarro carro
      anguloObjetivo = rad2deg (atan2 (y2 - y1) (x2 - x1))
      delta = anguloObjetivo - anguloActual
  in Girar (delta * 20)

{-
Notas / Ideas de extensión:

-- 1) Bot más avanzado con movimiento hacia el enemigo
botConMovimiento :: Mundo -> CarroCombate -> [BotAction]
botConMovimiento mundo carro =
    case carrosVistosPor carro mundo of
        [] -> [Esperar]
        (enemigo:_) ->
            let (x1,y1) = posicionCarro carro
                (x2,y2) = posicionCarro enemigo
                dx = x2 - x1
                dy = y2 - y1
            in [Mover (normalize (dx, dy)), DispararA (carroId enemigo)]

-- 2) Integración futura:
-- Se podría añadir un campo `bot :: Mundo -> CarroCombate -> [BotAction]` 
-- a cada CarroCombate para asignarle un bot y que la simulación llame
-- a la función de decisiones del bot en cada turno.
-}
