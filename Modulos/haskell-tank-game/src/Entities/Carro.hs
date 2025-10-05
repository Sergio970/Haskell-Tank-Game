module Entities.Carro where

import qualified Data.Map.Strict as Map
import Data.List (tails)
import Physics.Physics (Vector, Position, Size, Angle, updatePosition)
import Types.Types (TipoCarro, Tripulacion, Municion, Mundo)
import Entities.Tripulacion (aplicarEfectosTripulacion, matarTripulanteAleatorioSiDanio)
import Entities.Municion (Municion)


data TipoCarro = Ligero | Pesado | Cazacarros
    deriving (Show, Eq)

data CarroCombate = CarroCombate {
    carroId        :: Int,
    team           :: Int,
    tipoCarro      :: TipoCarro,
    posicionCarro  :: Position,
    direccionCarro :: Angle,
    velocidadCarro :: Vector,
    tamanoCarro    :: Size,
    energia        :: Int,
    blindaje       :: Float,
    alcanceVision  :: Float,
    alcanceRadio   :: Float,
    tripulacion    :: Tripulacion,
    municiones     :: [Municion],
    cadencia       :: Float,
    precisionBase  :: Float,
    memoriaCarro   :: Map.Map String Value
} deriving (Show)

-- ============================================================
-- Parámetros de diseño: cómo la tripulación y tipo afectan (MODIFICABLES)
-- ============================================================

-- Ajustes de visión base según tipoCarro (valores de ejemplo)
visionBase :: TipoCarro -> Distance
visionBase Ligero     = 200.0  -- spotters: mucha visión
visionBase Cazacarros = 120.0
visionBase Pesado     = 80.0   -- pesados: poca visión

-- Ajustes de blindaje base por tipo (ejemplo)
blindajeBase :: TipoCarro -> Float
blindajeBase Ligero     = 50
blindajeBase Cazacarros = 80
blindajeBase Pesado     = 200

-- Ajuste de radio base por tipo (ejemplo)
radioBase :: TipoCarro -> Distance
radioBase Ligero     = 150.0
radioBase Cazacarros = 120.0
radioBase Pesado     = 100.0

-- ============================================================
-- Daño al carro y posibilidad de matar tripulante aleatorio (si hay daño y hay tripulantes vivos)
-- ============================================================

aplicarDanioConMuerteAleatoria :: Int -> CarroCombate -> IO CarroCombate
aplicarDanioConMuerteAleatoria dmg carro = do
    let nuevaEnergia = energia carro - dmg
        carroConEnergia = carro { energia = max 0 nuevaEnergia }
    carroConPosibleMuerte <- matarTripulanteAleatorioSiDanio dmg carroConEnergia
    return (aplicarEfectosTripulacion carroConPosibleMuerte)

-- ============================================================
-- Visión y radio compartida por equipo
-- ============================================================

-- Un carro 'a' puede ver a 'b' si:
-- 1) Distancia(a,b) <= alcanceVision(a)   OR
-- 2) existe un compañero 'c' del mismo equipo tal que:
--      Distancia(a,c) <= alcanceRadio(a)  AND Distancia(c,b) <= alcanceVision(c)
-- (tomamos efectos de tripulación ya aplicados: alcanceRadio y alcanceVision ya deben reflejar eso)
veEntre :: CarroCombate -> CarroCombate -> [CarroCombate] -> Bool
veEntre a b allCarros =
    let d_ab = distanceBetween (posicionCarro a) (posicionCarro b)
        propio = d_ab <= alcanceVision a
        -- compañeros en el mismo equipo distintos de a
        compañeros = filter (\c -> team c == team a && posicionCarro c /= posicionCarro a) allCarros
        radioCheck = any (\c ->
                            distanceBetween (posicionCarro a) (posicionCarro c) <= alcanceRadio a
                            && distanceBetween (posicionCarro c) (posicionCarro b) <= alcanceVision c
                         ) compañeros
    in propio || radioCheck

-- Lista de carros que 'a' ve en el mundo (aplicando compartir por radio)
carrosVistosPor :: CarroCombate -> Mundo -> [CarroCombate]
carrosVistosPor a mundo =
    let allC = carros mundo
        aAplic = aplicarEfectosTripulacion a
        -- importante: aplicar efectos a los compañeros también (porque su vision puede depender de su tripulación)
        allAplicados = map aplicarEfectosTripulacion allC
        visibles = filter (\b -> posicionCarro b /= posicionCarro a && veEntre aAplic b allAplicados) allAplicados
    in visibles

-- ============================================================
-- Memoria de carro: almacenamiento de parámetros y estado (MODIFICABLES)
-- ============================================================

-- Memoria base para un carro ligero
memoriaLigero :: Memory
memoriaLigero = Map.fromList
    [ ("tipoCarro",    VTipoCarro Ligero)
    , ("vida",         VInt 100)
    , ("blindaje",     VFloat 30.0)
    , ("tamano",       VSize (2.0, 2.0))
    , ("alcanceVision",VFloat 20.0)
    , ("cadencia",     VFloat 1.8)
    , ("precision",    VFloat 0.8)
    ]

-- Memoria base para un carro pesado
memoriaPesado :: Memory
memoriaPesado = Map.fromList
    [ ("tipoCarro",    VTipoCarro Pesado)
    , ("vida",         VInt 200)
    , ("blindaje",     VFloat 70.0)
    , ("tamano",       VSize (3.0, 3.0))
    , ("alcanceVision",VFloat 10.0)
    , ("cadencia",     VFloat 2.0)
    , ("precision",    VFloat 0.6)
    ]

-- Memoria base para un cazacarros
memoriaCazacarros :: Memory
memoriaCazacarros = Map.fromList
    [ ("tipoCarro",    VTipoCarro Cazacarros)
    , ("vida",         VInt 120)
    , ("blindaje",     VFloat 40.0)
    , ("tamano",       VSize (2.5, 2.0))
    , ("alcanceVision",VFloat 15.0)
    , ("cadencia",     VFloat 1.2)
    , ("precision",    VFloat 0.9)
    ]

memoriaMunicionAP :: Memory
memoriaMunicionAP = Map.fromList
    [ ("tipoMun",  VMunicionTipo AP)
    , ("calibre",  VFloat 0.8)
    ]

memoriaMunicionAE :: Memory
memoriaMunicionAE = Map.fromList
    [ ("tipoMun",  VMunicionTipo AE)
    , ("calibre",  VFloat 0.5)
    ]

-- Guardar un valor en la memoria de un carro
setMemory :: String -> Value -> CarroCombate -> CarroCombate
setMemory key val carro@CarroCombate{..} =
    carro { memoriaCarro = Map.insert key val memoriaCarro }

-- Leer un valor de la memoria de un carro
getMemory :: String -> CarroCombate -> Maybe Value
getMemory key CarroCombate{..} = Map.lookup key memoriaCarro    

memoriaMundo :: Memory
memoriaMundo = Map.fromList
    [ ("tamanoMundo", VSize (50, 50)) ]
