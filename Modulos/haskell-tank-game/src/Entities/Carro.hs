module Entities.Carro where

import qualified Data.Map.Strict as Map
import Data.List (tails)

import Types.Objeto (Objeto(..))
import Types.Types
  ( Vector, Position, Size, Angle, Distance
  , Memory, Value(..)
  , TipoCarro(..), MunicionTipo(..)
  )

import Physics.Physics (distanceBetween)
import Entities.Tripulacion
  ( Tripulacion(..)
  , matarTripulanteAleatorioSiDanio
  )
import Entities.Municion (Municion)
import Entities.Mundo (Mundo(..))

-- ===============================
-- Atributos específicos del carro
-- ===============================

data CarroAtributos = CarroAtributos
  { carroIdE        :: Int
  , equipoE         :: Int
  , tipoCarroE      :: TipoCarro
  , energiaE        :: Int
  , blindajeE       :: Float
  , alcanceVisionE  :: Float
  , alcanceRadioE   :: Float
  , tripulacionE    :: Tripulacion
  , municionesE     :: [Municion]
  , cadenciaE       :: Float
  , precisionBaseE  :: Float
  , memoriaCarroE   :: Map.Map String Value
  } deriving (Show, Eq)

-- Un carro es un Objeto con CarroAtributos como 'atributos'
type CarroCombate = Objeto CarroAtributos

-- ===================================
-- Wrappers de geometría (Objeto)
-- ===================================

posicionCarro  :: CarroCombate -> Position
posicionCarro  = posicion

direccionCarro :: CarroCombate -> Angle
direccionCarro = direccion

velocidadCarro :: CarroCombate -> Vector
velocidadCarro = velocidad

tamanoCarro    :: CarroCombate -> Size
tamanoCarro    = tamano

-- ===================================
-- Wrappers de atributos del Carro
-- ===================================

carroId :: CarroCombate -> Int
carroId Objeto{ atributos = CarroAtributos{ carroIdE = x } } = x

team :: CarroCombate -> Int
team Objeto{ atributos = CarroAtributos{ equipoE = x } } = x

tipoCarro :: CarroCombate -> TipoCarro
tipoCarro Objeto{ atributos = CarroAtributos{ tipoCarroE = t } } = t

energia :: CarroCombate -> Int
energia Objeto{ atributos = CarroAtributos{ energiaE = e } } = e

blindaje :: CarroCombate -> Float
blindaje Objeto{ atributos = CarroAtributos{ blindajeE = b } } = b

alcanceVision :: CarroCombate -> Float
alcanceVision Objeto{ atributos = CarroAtributos{ alcanceVisionE = v } } = v

alcanceRadio :: CarroCombate -> Float
alcanceRadio Objeto{ atributos = CarroAtributos{ alcanceRadioE = r } } = r

tripulacion :: CarroCombate -> Tripulacion
tripulacion Objeto{ atributos = CarroAtributos{ tripulacionE = t } } = t

municiones :: CarroCombate -> [Municion]
municiones Objeto{ atributos = CarroAtributos{ municionesE = ms } } = ms

cadencia :: CarroCombate -> Float
cadencia Objeto{ atributos = CarroAtributos{ cadenciaE = c } } = c

precisionBase :: CarroCombate -> Float
precisionBase Objeto{ atributos = CarroAtributos{ precisionBaseE = p } } = p

memoriaCarro :: CarroCombate -> Map.Map String Value
memoriaCarro Objeto{ atributos = CarroAtributos{ memoriaCarroE = m } } = m

-- ===================================
-- Setters
-- ===================================

setEnergia :: Int -> CarroCombate -> CarroCombate
setEnergia e obj@Objeto{ atributos = a } =
  obj { atributos = a { energiaE = e } }

setMemoriaCarro :: Map.Map String Value -> CarroCombate -> CarroCombate
setMemoriaCarro m obj@Objeto{ atributos = a } =
  obj { atributos = a { memoriaCarroE = m } }

setTripulacion :: Tripulacion -> CarroCombate -> CarroCombate
setTripulacion t obj@Objeto{ atributos = a } =
  obj { atributos = a { tripulacionE = t } }

-- ============================================================
-- Parámetros de diseño
-- ============================================================

visionBase :: TipoCarro -> Distance
visionBase Ligero     = 200.0
visionBase Cazacarros = 120.0
visionBase Pesado     = 80.0

blindajeBase :: TipoCarro -> Float
blindajeBase Ligero     = 50
blindajeBase Cazacarros = 80
blindajeBase Pesado     = 200

radioBase :: TipoCarro -> Distance
radioBase Ligero     = 150.0
radioBase Cazacarros = 120.0
radioBase Pesado     = 100.0

-- ============================================================
-- Efectos de la tripulación sobre el carro
-- (se define aquí para no acoplar el módulo Tripulacion al Objeto/Carro)
-- ============================================================

aplicarEfectosTripulacion :: CarroCombate -> CarroCombate
aplicarEfectosTripulacion carro@Objeto{ velocidad = (vx, vy)
                                      , atributos = a@CarroAtributos
                                          { tripulacionE   = t
                                          , cadenciaE      = cad0
                                          , precisionBaseE = prec0
                                          , alcanceRadioE  = radio0
                                          }
                                      } =
  let -- Velocidad: conductor muerto reduce mucho
      vel1 = case conductor t of
               Muerto -> (vx * 0.5, vy * 0.5)
               Vivo   -> (vx, vy)
      -- Cadencia: cargador muerto => más lento
      cad1 = case cargador t of
               Muerto -> cad0 * 1.8
               Vivo   -> cad0
      -- Precisión: artillero muerto => peor
      prec1 = case artillero t of
                Muerto -> prec0 * 0.6
                Vivo   -> prec0
      -- Radio: operador de radio muerto => menos alcance
      radio1 = case operadorRadio t of
                 Muerto -> radio0 * 0.5
                 Vivo   -> radio0
      -- Comandante penaliza levemente todo si está muerto
      (vel', cad', prec', radio') =
        case comandante t of
          Muerto -> (vel1, cad1 * 1.1, prec1 * 0.9, radio1 * 0.95)
          Vivo   -> (vel1, cad1,        prec1,       radio1)
  in carro
       { velocidad = vel'
       , atributos = a { cadenciaE      = cad'
                       , precisionBaseE = prec'
                       , alcanceRadioE  = radio'
                       }
       }

-- ============================================================
-- Daño al carro y posible muerte de tripulante
-- ============================================================

aplicarDanioConMuerteAleatoria :: Int -> CarroCombate -> IO CarroCombate
aplicarDanioConMuerteAleatoria dmg carro = do
  let nuevaE          = max 0 (energia carro - dmg)
      carroConEnergia = setEnergia nuevaE carro
  t' <- matarTripulanteAleatorioSiDanio dmg (tripulacion carroConEnergia)
  let carroConTrip = setTripulacion t' carroConEnergia
  return (aplicarEfectosTripulacion carroConTrip)

-- ============================================================
-- Visión y radio compartida por equipo
-- ============================================================

veEntre :: CarroCombate -> CarroCombate -> [CarroCombate] -> Bool
veEntre a b allCarros =
  let d_ab       = distanceBetween (posicionCarro a) (posicionCarro b)
      propio     = d_ab <= alcanceVision a
      companeros = filter (\c -> team c == team a && posicionCarro c /= posicionCarro a) allCarros
      radioCheck = any (\c ->
                         distanceBetween (posicionCarro a) (posicionCarro c) <= alcanceRadio a
                      && distanceBetween (posicionCarro c) (posicionCarro b) <= alcanceVision c
                       ) companeros
  in propio || radioCheck

carrosVistosPor :: CarroCombate -> Mundo -> [CarroCombate]
carrosVistosPor a mundo =
  let allC         = carros mundo
      aAplic       = aplicarEfectosTripulacion a
      allAplicados = map aplicarEfectosTripulacion allC
  in filter (\b -> posicionCarro b /= posicionCarro a && veEntre aAplic b allAplicados) allAplicados

-- ============================================================
-- Memorias base
-- ============================================================

memoriaLigero :: Memory
memoriaLigero = Map.fromList
  [ ("tipoCarro",     VTipoCarro Ligero)
  , ("vida",          VInt 100)
  , ("blindaje",      VFloat 30.0)
  , ("tamano",        VSize (2.0, 2.0))
  , ("alcanceVision", VFloat 20.0)
  , ("cadencia",      VFloat 1.8)
  , ("precision",     VFloat 0.8)
  ]

memoriaPesado :: Memory
memoriaPesado = Map.fromList
  [ ("tipoCarro",     VTipoCarro Pesado)
  , ("vida",          VInt 200)
  , ("blindaje",      VFloat 70.0)
  , ("tamano",        VSize (3.0, 3.0))
  , ("alcanceVision", VFloat 10.0)
  , ("cadencia",      VFloat 2.0)
  , ("precision",     VFloat 0.6)
  ]

memoriaCazacarros :: Memory
memoriaCazacarros = Map.fromList
  [ ("tipoCarro",     VTipoCarro Cazacarros)
  , ("vida",          VInt 120)
  , ("blindaje",      VFloat 40.0)
  , ("tamano",        VSize (2.5, 2.0))
  , ("alcanceVision", VFloat 15.0)
  , ("cadencia",      VFloat 1.2)
  , ("precision",     VFloat 0.9)
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

-- Helpers de memoria
setMemory :: String -> Value -> CarroCombate -> CarroCombate
setMemory key val obj@Objeto{ atributos = a@CarroAtributos{ memoriaCarroE = m } } =
  obj { atributos = a { memoriaCarroE = Map.insert key val m } }

getMemory :: String -> CarroCombate -> Maybe Value
getMemory key Objeto{ atributos = CarroAtributos{ memoriaCarroE = m } } =
  Map.lookup key m

memoriaMundo :: Memory
memoriaMundo = Map.fromList
  [ ("tamanoMundo", VSize (50, 50)) ]
