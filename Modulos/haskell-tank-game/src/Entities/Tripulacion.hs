module Entities.Tripulacion where

import System.Random (randomRIO)
import Entities.Carro (CarroCombate)

data EstadoTripulante = Vivo | Muerto
    deriving (Show, Eq)

data Tripulacion = Tripulacion {
    comandante    :: EstadoTripulante,
    conductor     :: EstadoTripulante,
    artillero     :: EstadoTripulante,
    operadorRadio :: EstadoTripulante,
    cargador      :: EstadoTripulante
} deriving (Show, Eq)

-- Función para verificar si todos los tripulantes están vivos
todosVivos :: Tripulacion -> Bool
todosVivos Tripulacion{..} = 
    comandante == Vivo && conductor == Vivo && artillero == Vivo && operadorRadio == Vivo && cargador == Vivo

-- Función para contar los tripulantes vivos
contarVivos :: Tripulacion -> Int
contarVivos Tripulacion{..} = 
    length $ filter (== Vivo) [comandante, conductor, artillero, operadorRadio, cargador]

-- ============================================================
-- Tripulación: efectos sobre parámetros del carro (MODIFICABLES)
-- ============================================================

-- Aplica los efectos de la tripulación sobre velocidad, cadencia, precisión y alcance radio.
aplicarEfectosTripulacion :: CarroCombate -> CarroCombate
aplicarEfectosTripulacion carro@CarroCombate{..} =
    let t = tripulacion
        -- Velocidad: conductor muerto reduce velocidad mucho (50%)
        (vx, vy) = velocidadCarro
        velocidadFinal = case conductor t of
            Muerto -> (vx * 0.5, vy * 0.5)
            Vivo   -> (vx, vy)
        -- Cadencia: cargador muerto incrementa tiempo entre disparos (más lento)
        cadenciaFinal = case cargador t of
            Muerto -> cadencia * 1.8  -- recarga ~80% más lenta
            Vivo   -> cadencia
        -- Precisión: artillero muerto reduce precisión
        precisionFactor = case artillero t of
            Muerto -> 0.6  -- precisión baja a 60%
            Vivo   -> 1.0
        precisionFinal = precisionBase * precisionFactor
        -- Alcance radio: operador muerto reduce alcance radio
        alcanceRadioFinal = case operadorRadio t of
            Muerto -> alcanceRadio * 0.5
            Vivo   -> alcanceRadio
        -- Comandante afecta levemente a todo (si muerto, penaliza un poco)
        (vel', cad', prec', radio') = case comandante t of
            Muerto -> (velocidadFinal, cadenciaFinal * 1.1, precisionFinal * 0.9, alcanceRadioFinal * 0.95)
            Vivo   -> (velocidadFinal, cadenciaFinal, precisionFinal, alcanceRadioFinal)
    in carro { velocidadCarro = vel', cadencia = cad', precisionBase = prec', alcanceRadio = radio' }

-- ============================================================
-- Tripulación: matar tripulante aleatorio si hay daño
-- ============================================================

-- Matar un tripulante aleatorio sólo si hubo suficiente daño (por simplicidad: si dmg>0)
matarTripulanteAleatorioSiDanio :: Int -> CarroCombate -> IO CarroCombate
matarTripulanteAleatorioSiDanio dmg carro
    | dmg <= 0 = return carro
    | otherwise = matarTripulanteAleatorio carro

matarTripulanteAleatorio :: CarroCombate -> IO CarroCombate
matarTripulanteAleatorio carro@CarroCombate{..} = do
    let t = tripulacion
        vivos = [ ("conductor", conductor t)
                , ("artillero", artillero t)
                , ("operadorRadio", operadorRadio t)
                , ("cargador", cargador t)
                , ("comandante", comandante t)
                ]
        vivosSolo = [r | (r, estado) <- vivos, estado == Vivo]
    if null vivosSolo
        then return carro -- nadie que matar
        else do
            idx <- randomRIO (0, length vivosSolo - 1)
            let elegido = vivosSolo !! idx
                trip' = case elegido of
                    "conductor"     -> t { conductor = Muerto }
                    "artillero"     -> t { artillero = Muerto }
                    "operadorRadio" -> t { operadorRadio = Muerto }
                    "cargador"      -> t { cargador = Muerto }
                    "comandante"    -> t { comandante = Muerto }
                    _               -> t
            return carro { tripulacion = trip' }