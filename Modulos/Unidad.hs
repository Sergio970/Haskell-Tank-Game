{-# LANGUAGE DuplicateRecordFields #-}
module Unidad (
    -- Tipos principales
    CarroCombate(..),
    CarroAtributos(..),
    Mundo(..),
    Tripulacion(..),
    EstadoTripulante(..),
    Meteorito(..),
    Estela(..),

    -- Bomba
    Bomba(..),
    agregarBomba,

    -- Wrappers de acceso
    posicionCarro,
    direccionCarro,
    getdireccionCanon,
    velocidadCarro,
    tamanoCarro,
    carroId,
    team,
    tipoCarro,
    energia,
    blindaje,
    alcanceVision,
    alcanceRadio,
    tripulacion,
    municiones,
    cadencia,
    precisionBase,
    memoriaCarro,

    -- Setters
    setPosicion,
    setEnergia,
    setMemoriaCarro,
    setTripulacion,

    -- Parámetros / utilidades
    visionBase,
    blindajeBase,
    radioBase,
    aplicarEfectosTripulacion,

    -- Memorias base
    memoriaLigero,
    memoriaPesado,
    memoriaCazacarros,
    memoriaMunicionAP,
    memoriaMunicionAE,
    memoriaMundo,
    tripulacionViva,

    -- Munición / disparo / daño
    Municion(..),
    Proyectil(..),
    dispararA,
    aplicarImpactoDirecto,
    calcularDanio,
    aplicarDanioConMuerteAleatoria,

    -- Mundo
    agregarCarro,
    agregarProyectil,
    removerCarro,
    removerProyectil,
    carrosVistosPor,
    mostrarVisionDe,

    -- Visión
    veEntre,

    -- Bucle básico (sin bots, sin colisiones)
    tickSeconds
) where

import qualified Data.Map.Strict as Map
import System.Random (randomRIO)
import Data.List (maximumBy)
import Data.Ord  (comparing)

import Objeto (Objeto(..))
import Physics (deg2rad, distanceBetween, updatePosition)
import Types
  ( Vector, Position, Size, Angle, Distance
  , Memory, Value(..)
  , TipoCarro(..), MunicionTipo(..)
  )

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

type CarroCombate = Objeto CarroAtributos

-- ===============================
-- Wrappers Objeto
-- ===============================

posicionCarro  :: CarroCombate -> Position
posicionCarro  = posicion

direccionCarro :: CarroCombate -> Angle
direccionCarro = direccion

getdireccionCanon :: CarroCombate -> Angle
getdireccionCanon = direccionCanon

velocidadCarro :: CarroCombate -> Vector
velocidadCarro = velocidad

tamanoCarro    :: CarroCombate -> Size
tamanoCarro    = tamano

-- Wrappers atributos Carro
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

-- ===============================
-- Setters
-- ===============================

setPosicion :: Position -> CarroCombate -> CarroCombate
setPosicion p obj = obj { posicion = p }

setEnergia :: Int -> CarroCombate -> CarroCombate
setEnergia e obj@Objeto{ atributos = a } = obj { atributos = a { energiaE = e } }

setMemoriaCarro :: Map.Map String Value -> CarroCombate -> CarroCombate
setMemoriaCarro m obj@Objeto{ atributos = a } = obj { atributos = a { memoriaCarroE = m } }

setTripulacion :: Tripulacion -> CarroCombate -> CarroCombate
setTripulacion t obj@Objeto{ atributos = a } = obj { atributos = a { tripulacionE = t } }

-- ===============================
-- Parámetros de diseño
-- ===============================

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

-- ===============================
-- Tripulación
-- ===============================

data EstadoTripulante = Vivo | Muerto deriving (Show, Eq)

data Tripulacion = Tripulacion
  { comandante    :: EstadoTripulante
  , conductor     :: EstadoTripulante
  , artillero     :: EstadoTripulante
  , operadorRadio :: EstadoTripulante
  , cargador      :: EstadoTripulante
  } deriving (Show, Eq)

tripulacionViva :: Tripulacion
tripulacionViva = Tripulacion Vivo Vivo Vivo Vivo Vivo

-- Efectos de tripulación
aplicarEfectosTripulacion :: CarroCombate -> CarroCombate
aplicarEfectosTripulacion carro@Objeto{ velocidad = (vx, vy)
                                      , atributos = a@CarroAtributos
                                          { tripulacionE   = t
                                          , cadenciaE      = cad0
                                          , precisionBaseE = prec0
                                          , alcanceRadioE  = radio0
                                          } } =
  let vel1   = case conductor t     of { Muerto -> (vx*0.5, vy*0.5); Vivo -> (vx,vy) }
      cad1   = case cargador t      of { Muerto -> cad0*1.8;          Vivo -> cad0 }
      prec1  = case artillero t     of { Muerto -> prec0*0.6;         Vivo -> prec0 }
      radio1 = case operadorRadio t  of { Muerto -> radio0*0.5;        Vivo -> radio0 }
      (vel', cad', prec', radio') =
        case comandante t of { Muerto -> (vel1, cad1*1.1, prec1*0.9, radio1*0.95); Vivo -> (vel1,cad1,prec1,radio1) }
  in carro { velocidad = vel'
           , atributos = a { cadenciaE = cad', precisionBaseE = prec', alcanceRadioE = radio' }
           }

-- ===============================
-- Memorias base
-- ===============================

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

-- Helpers de memoria (puedes exportarlos si los usa tu Bot)
setMemory :: String -> Value -> CarroCombate -> CarroCombate
setMemory key val obj@Objeto{ atributos = a@CarroAtributos{ memoriaCarroE = m } } =
  obj { atributos = a { memoriaCarroE = Map.insert key val m } }

getMemory :: String -> CarroCombate -> Maybe Value
getMemory key Objeto{ atributos = CarroAtributos{ memoriaCarroE = m } } =
  Map.lookup key m

memoriaMundo :: Memory
memoriaMundo = Map.fromList [ ("tamanoMundo", VSize (50, 50)) ]

-- ===============================
-- Munición / Proyectil
-- ===============================

data Municion = Municion
  { tipoMun    :: MunicionTipo
  , calibreMun :: Float
  , memoriaMun :: Memory
  } deriving (Show, Eq)

data Proyectil = Proyectil
  { proyectilId        :: Int
  , posicionProyectil  :: Position
  , direccionProyectil :: Angle
  , velocidadProyectil :: Vector
  , municionProyectil  :: Municion
  , disparadorTeam     :: Int
  , memoriaProj        :: Memory
  } deriving (Show, Eq)

penetracionEstim :: Municion -> Float
penetracionEstim (Municion AP c _) = c * 30.0
penetracionEstim (Municion AE c _) = c * 8.0

danioEstim :: Municion -> Int
danioEstim (Municion AP c _) = round (c * 6.0)
danioEstim (Municion AE c _) = round (c * 4.0)

calcularDanio :: MunicionTipo -> Float -> Int -> Float -> Int
calcularDanio AP pen dmgBase blind
  | pen > blind = dmgBase * 2
  | otherwise   = round (fromIntegral dmgBase * 0.6)
calcularDanio AE _ dmgBase _ = dmgBase

-- Elegir munición y disparar
buscarMunicionPreferida :: MunicionTipo -> CarroCombate -> Maybe Int
buscarMunicionPreferida mtype carro =
  fst <$> maximumByMay (comparing (calibreMun . snd)) seleccionadas
  where
    seleccionadas = filter ((== mtype) . tipoMun . snd) (zip [0..] (municiones carro))
    maximumByMay _ [] = Nothing
    maximumByMay cmp xs = Just (maximumBy cmp xs)

elegirMunicionPara :: CarroCombate -> CarroCombate -> Maybe Int
elegirMunicionPara atacante objetivo = do
  let ms = municiones atacante
  i <- buscarMunicionPreferida AP atacante
  let m = ms !! i
  if penetracionEstim m > blindaje objetivo
    then Just i
    else buscarMunicionPreferida AE atacante

dispararA :: Int -> CarroCombate -> CarroCombate -> Maybe (Proyectil, CarroCombate)
dispararA pid atacante _objetivo =
  case municiones atacante of
    [] -> Nothing
    (m:resto) ->
      let dirG       = getdireccionCanon atacante
          dirR       = deg2rad dirG
          (tankW, _) = tamanoCarro atacante
          (tx, ty)   = posicionCarro atacante
          offset     = tankW / 2 + 1.0
          pos        = (tx + offset * cos dirR, ty + offset * sin dirR)
          vel        = (250 * cos dirR, 250 * sin dirR)  -- 💨 velocidad visible
          proyectil  = Proyectil pid pos dirG vel m (team atacante) (memoriaMun m)
          atacante'  = atacante { atributos = (atributos atacante) { municionesE = resto } }
      in Just (proyectil, atacante')


aplicarImpactoDirecto :: Municion -> CarroCombate -> Maybe CarroCombate
aplicarImpactoDirecto mun objetivo =
  let valida obj = if energia obj > 0 then Just obj else Nothing
      calcular obj =
        let pen     = penetracionEstim mun
            dmgBase = danioEstim mun
            dmg     = calcularDanio (tipoMun mun) pen dmgBase (blindaje obj)
            nuevaE  = max 0 (energia obj - dmg)
        in setEnergia nuevaE obj
  in calcular <$> valida objetivo

-- ===============================
-- Tripulación / daño aleatorio
-- ===============================

matarTripulanteAleatorioSiDanio :: Int -> Tripulacion -> IO Tripulacion
matarTripulanteAleatorioSiDanio dmg t
  | dmg <= 0  = pure t
  | otherwise = do
      mt' <- matarTripulanteAleatorio t
      pure (maybe t id mt')

obtenerTripulantesVivos :: Tripulacion -> [String]
obtenerTripulantesVivos t =
  let roles = [ ("conductor",     conductor t)
              , ("artillero",     artillero t)
              , ("operadorRadio", operadorRadio t)
              , ("cargador",      cargador t)
              , ("comandante",    comandante t)
              ]
  in [ r | (r,e) <- roles, e == Vivo ]

matarTripulante :: String -> Tripulacion -> Tripulacion
matarTripulante "conductor"     t = t { conductor     = Muerto }
matarTripulante "artillero"     t = t { artillero     = Muerto }
matarTripulante "operadorRadio" t = t { operadorRadio = Muerto }
matarTripulante "cargador"      t = t { cargador      = Muerto }
matarTripulante "comandante"    t = t { comandante    = Muerto }
matarTripulante _               t = t

matarTripulanteAleatorio :: Tripulacion -> IO (Maybe Tripulacion)
matarTripulanteAleatorio t =
  case obtenerTripulantesVivos t of
    [] -> pure Nothing
    vivos -> do
      i <- randomRIO (0, length vivos - 1)
      let elegido = vivos !! i
      pure (Just (matarTripulante elegido t))

aplicarDanioConMuerteAleatoria :: Int -> CarroCombate -> IO CarroCombate
aplicarDanioConMuerteAleatoria dmg carro = do
  t' <- matarTripulanteAleatorioSiDanio dmg (tripulacion carro)
  let carroConTrip = setTripulacion t' carro
  pure (aplicarEfectosTripulacion carroConTrip)

-- ===============================
-- Mundo
-- ===============================

data Mundo = Mundo
  { carros      :: [CarroCombate]
  , proyectiles :: [Proyectil]
  , obstaculos  :: [Meteorito]
  , bombas      :: [Bomba]       
  , tamanoMundo :: Size
  , memoria     :: Memory
  } deriving (Show)

agregarCarro :: CarroCombate -> Mundo -> Mundo
agregarCarro c m = m { carros = c : carros m }

agregarProyectil :: Proyectil -> Mundo -> Mundo
agregarProyectil p m = m { proyectiles = p : proyectiles m }

-- Nuevo: agregar bomba
agregarBomba :: Bomba -> Mundo -> Mundo
agregarBomba b m = m { bombas = b : bombas m }

removerCarro :: Int -> Mundo -> Mundo
removerCarro cid m = m { carros = filter ((/= cid) . carroId) (carros m) }

removerProyectil :: Int -> Mundo -> Mundo
removerProyectil pid m = m { proyectiles = filter ((/= pid) . proyectilId) (proyectiles m) }

-- Visión simple (directa + por radio de compañeros)
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
carrosVistosPor carro m =
  let allC         = carros m
      aAplic       = aplicarEfectosTripulacion carro
      allAplicados = map aplicarEfectosTripulacion allC
      visible b    = posicionCarro b /= posicionCarro carro && veEntre aAplic b allAplicados
  in filter visible allAplicados

mostrarVisionDe :: Mundo -> IO ()
mostrarVisionDe m = mapM_ printVision (carros m)
  where
    printVision c = do
      let vistos      = carrosVistosPor c m
          eq          = team c
          tipo        = tipoCarro c
          vistosTipos = map tipoCarro vistos
      putStrLn $
        "Carro (team " ++ show eq ++ ", tipo " ++ show tipo ++
        ") ve " ++ show vistosTipos

-- ===============================
-- Obstáculos
-- ===============================

data Meteorito = Meteorito
  { meteoritoId :: Int
  , posicionMeteorito :: Position
  , velocidadMeteorito :: Vector
  , tamanoMeteorito :: Float
  , rotacionMeteorito :: Float
  , velocidadRotacion :: Float
  , vida :: Int
  , estelas :: [Estela]
  } deriving (Show, Eq)

data Estela = Estela
  { estelaId :: Int
  , estelaPos :: Position
  , estelaVida :: Float -- TTL
  , estelaRadio :: Float
  , estelaIntensidad :: Float -- daño
  } deriving (Show, Eq)

-- Nueva: Bomba
data Bomba = Bomba
  { bombaId       :: Int
  , posicionBomba :: Position
  , radioBomba    :: Float
  , activaBomba   :: Bool    -- nueva
  , tiempoBomba   :: Float   -- nueva
  } deriving (Show, Eq)

-- ===============================
-- Bucle básico (sin bots ni colisiones)
-- ===============================

tickSeconds :: Float
tickSeconds = 1 / 60  -- 60 FPS

-- Movimiento sencillo por física (no IA, no colisiones):
bucleTorneo :: Float -> Mundo -> IO Mundo
bucleTorneo dt m = do
  let cs  = carros m
      ps  = proyectiles m
      cs' = [ c { posicion = updatePosition dt (posicion c) (velocidad c) } | c <- cs ]
      ps' = [ p { posicionProyectil = updatePosition dt (posicionProyectil p) (velocidadProyectil p) } | p <- ps ]
  pure m { carros = cs', proyectiles = ps' }
