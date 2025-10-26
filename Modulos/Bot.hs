module Bot where

import Unidad
import Types
import Physics

import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.List (minimumBy)

-- ============================================================
-- 5. DSL para acciones del Bot
-- ============================================================

-- Acciones que un bot puede realizar
data BotAction
    = Mover Vector              -- Moverse en cierta dirección
    | Girar Angle               -- Girar el carro
    | GirarCanon Angle          -- Girar el cañón
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
  case carrosVistosPor carro mundo of
    -- Si ve a un enemigo
    (enemigo:_) ->
      -- Guarda la última posición conocida del enemigo en su memoria
      let carroConMemoria = setMemory "lastEnemyPos" (VPoint (posicionCarro enemigo)) carro
      in [apuntarHacia carroConMemoria enemigo, DispararA (carroId enemigo)]

    -- Si no ve a nadie
    [] ->
      case getMemory "lastEnemyPos" carro of
        -- Si recuerda dónde estuvo un enemigo, apunta hacia allí por un tiempo
        Just (VPoint pos) ->
          let objetivoFantasma = setPosicion pos carro -- Un "enemigo fantasma" para apuntar
          in [apuntarHacia carro objetivoFantasma]
        -- Si no tiene a nadie en memoria, patrulla girando el chasis
        Nothing -> [Girar 10]

-- ============================================================
-- Estrategia del tanque pesado
-- ============================================================
estrategiaPesado :: Mundo -> CarroCombate -> [BotAction]
estrategiaPesado mundo carro =
  let enemigosVisibles = carrosVistosPor carro mundo
  in case getMemory "target_venganza" carro of
    -- Si tiene un objetivo de venganza
    Just (VInt id_venganza) ->
      case filter (\e -> carroId e == id_venganza) enemigosVisibles of
        -- Si lo ve, lo persigue y ataca
        (enemigo:_) ->
          [apuntarHacia carro enemigo, Mover (direccionHacia carro enemigo), DispararA (carroId enemigo)]
        -- Si no lo ve, borra la memoria para buscar otro
        [] ->
          let carroSinMemoria = setMemory "target_venganza" (VString "None") carro
          in [Girar 15] -- Gira para buscarlo

    -- Si no tiene sed de venganza, busca al enemigo más cercano
    _ ->
      case enemigoMasCercano carro enemigosVisibles of
        Just enemigo ->
          [apuntarHacia carro enemigo, DispararA (carroId enemigo)]
        Nothing ->
          [Mover (direccionAleatoria carro)]

-- ============================================================
-- Funciones auxiliares
-- ============================================================

-- Devuelve la dirección normalizada hacia un objetivo
direccionHacia :: CarroCombate -> CarroCombate -> Vector
direccionHacia origen destino =
  let (x1, y1) = posicionCarro origen
      (x2, y2) = posicionCarro destino
  in normalize (x2 - x1, y2 - y1)

-- Encuentra al enemigo más cercano de una lista
enemigoMasCercano :: CarroCombate -> [CarroCombate] -> Maybe CarroCombate
enemigoMasCercano _ [] = Nothing
enemigoMasCercano carro enemigos =
  Just $ minimumBy (comparing (distanceBetween (posicionCarro carro) . posicionCarro)) enemigos

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
      anguloActual  = getdireccionCanon carro
      anguloObjetivo = rad2deg (atan2 (y2 - y1) (x2 - x1))
      delta = anguloObjetivo - anguloActual
  in GirarCanon delta

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
