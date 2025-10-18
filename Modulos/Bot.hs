module Bot where

import Unidad
import Types

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

-- Bot de ejemplo
botEjemplo :: Mundo -> CarroCombate -> Maybe BotAction
botEjemplo mundo carro = -- Si no hay enemigos, devuelve Nothing. Si hay alguno, devuelve Just (DispararA id).
    DispararA . carroId <$> listToMaybe (carrosVistosPor carro mundo)

botCombinado :: Mundo -> CarroCombate -> Maybe [BotAction]
botCombinado mundo carro = -- Une las acciones de disparar y moverse
    pure (++) <*> atacar <*> mover
  where
    atacar = fmap (:[]) (botEjemplo mundo carro)
    mover  = pure [Mover (1, 0)]

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
