module Entities.Municion where

import qualified Data.Map.Strict as Map
import Physics.Physics (Vector, Position, Angle)
import Types.Types (Memory)
import Entities.Carro (CarroCombate)

data MunicionTipo = AP | AE
    deriving (Show, Eq)

data Municion = Municion {
    tipoMun       :: MunicionTipo,
    calibreMun    :: Float,    -- por ejemplo mm o valor abstracto
    memoriaMun    :: Memory
} deriving (Show, Eq)

data Proyectil = Proyectil {
    proyectilId        :: Int,
    posicionProyectil  :: Position,
    direccionProyectil :: Angle,
    velocidadProyectil :: Vector,
    municionProyectil  :: Municion,
    disparadorTeam     :: Int,
    memoriaProj        :: Memory
} deriving (Show)

-- ============================================================
-- Munición: penetración y daño según tipo y calibre
-- ============================================================

-- Estimación de penetración (valor abstracto) para comparar con blindaje
penetracionEstim :: Municion -> Float
penetracionEstim (Municion { tipoMun = AP, calibreMun = c }) = c * 30.0   -- AP penetra mucho (ej: calib*30)
penetracionEstim (Municion { tipoMun = AE, calibreMun = c }) = c * 8.0    -- AE poca penetración

-- Daño base infligido por munición (antes de modificadores)
danioEstim :: Municion -> Int
danioEstim (Municion { tipoMun = AP, calibreMun = c }) = round (c * 6.0)  -- AP da más daño directo al penetrar
danioEstim (Municion { tipoMun = AE, calibreMun = c }) = round (c * 4.0)  -- AE menos daño pero siempre hace algo

-- ============================================================
-- Elección de munición y disparo
-- ============================================================

-- Buscar índice de munición en inventario por tipo y (preferencia por calibre mayor)
buscarMunicionPreferida :: MunicionTipo -> CarroCombate -> Maybe Int
buscarMunicionPreferida mtype carro =
    let ms = municiones carro
        indexed = zip [0..] ms
        -- preferir mayor calibre del tipo pedido
        filtered = filter (\(_,m) -> tipoMun m == mtype) indexed
    in if null filtered then Nothing else Just (fst (maximumByCalibre filtered))
  where
    maximumByCalibre :: [(Int, Municion)] -> (Int, Municion)
    maximumByCalibre = foldl1 (\a@(_, m1) b@(_, m2) -> if calibreMun m1 >= calibreMun m2 then a else b)

-- Decide qué munición usar contra un objetivo: prefer AP si su penetración > blindaje objetivo
-- Si no hay AP o no penetra, usar AE si hay.
elegirMunicionPara :: CarroCombate -> CarroCombate -> Maybe Int
elegirMunicionPara atacante objetivo =
    let ms = municiones atacante
        indexAP = buscarMunicionPreferida AP atacante
        indexAE = buscarMunicionPreferida AE atacante
        tryAP = case indexAP of
            Nothing -> Nothing
            Just i  -> let m = ms !! i in if penetracionEstim m > blindaje objetivo then Just i else Nothing
    in case tryAP of
        Just i  -> Just i
        Nothing -> indexAE  -- fallback a AE si existe

-- Disparar: devuelve Maybe proyectil y atacante actualizado (munición consumida), no aplica daño aún
dispararA :: Int -> CarroCombate -> CarroCombate -> Maybe (Proyectil, CarroCombate)
dispararA pid atacante objetivo =
    case elegirMunicionPara atacante objetivo of
        Nothing -> Nothing
        Just idx ->
            let m = municiones atacante !! idx
                pos = posicionCarro atacante
                dir = direccionCarro atacante
                velProj = (0.0, 200.0)
                proyectil = Proyectil pid pos dir velProj m (team atacante) (memoriaMun m)
                nuevaLista = take idx (municiones atacante) ++ drop (idx+1) (municiones atacante)
                atacante' = atacante { municiones = nuevaLista }
            in Just (proyectil, atacante')

-- ============================================================
-- Aplicar impacto: al impactar, calculamos daño según tipo y blindaje objetivo
-- Si AP penetra (penetracion>blindaje) aplica mayor daño; si no, si llega AE aplica daño menor.
-- ============================================================

-- Aplica un impacto directo (sin probabilidades de fallo)
aplicarImpactoDirecto :: Municion -> CarroCombate -> CarroCombate
aplicarImpactoDirecto mun objetivo =
    let pen = penetracionEstim mun
        dmgBase = danioEstim mun
        dmg = case tipoMun mun of
            AP -> if pen > blindaje objetivo
                      then dmgBase * 2  -- penetración: daño mayor
                      else round (fromIntegral dmgBase * 0.6) -- falla penetrar -> menos daño
            AE -> dmgBase  -- explosivo hace daño consistente aunque no penetre
        nuevaEnergia = energia objetivo - dmg
    in objetivo { energia = max 0 nuevaEnergia }