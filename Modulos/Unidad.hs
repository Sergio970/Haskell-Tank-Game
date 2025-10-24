module Unidad where

import qualified Data.Map.Strict as Map
import Data.List (tails, maximumBy)
import Physics (distanceBetween, deg2rad)
import System.Random (randomRIO)
import Objeto (Objeto(..))
import Data.Maybe (listToMaybe)
import Data.Ord   (comparing)

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

-- Un carro es un Objeto con CarroAtributos como 'atributos'
type CarroCombate = Objeto CarroAtributos

{-
“Los wrappers son funciones auxiliares que actúan como una capa de abstracción entre la lógica del juego y 
la estructura interna de los tipos de datos. Permiten acceder de forma uniforme a los campos de los objetos, 
sin acoplar el código a los detalles de implementación. De este modo, 
si en el futuro cambiamos la forma en que almacenamos la información del carro o del mundo, 
solo será necesario modificar los wrappers, no todo el resto del código.”
-}

-- ===================================
-- Wrappers de geometría (Objeto)
-- ===================================

-- Estos acceden a los campos generales de Objeto

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

-- Estos acceden al campo atributos :: CarroAtributos y, dentro de él, a sus subcampos
-- Aquí el patrón { atributos = CarroAtributos{ carroIdE = x } }
-- extrae el valor x del campo carroIdE que está dentro del campo atributos del Objeto

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
setEnergia e obj@Objeto{ atributos = a } = -- a es el valor del campo atributos extraído del mismo objeto
  obj { atributos = a { energiaE = e } } -- a { energiaE = e } → modificas la energía dentro de los atributos

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
               Muerto -> cad0 * 1.8 -- Aumenta la cadencia en un 80%
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
  t' <- matarTripulanteAleatorioSiDanio dmg (tripulacion carro) -- Probabilidad de matar a un tripulante según daño
  let carroConTrip = setTripulacion t' carro
  return (aplicarEfectosTripulacion carroConTrip)

-- ============================================================
-- Visión y radio compartida por equipo
-- ============================================================

veEntre :: CarroCombate -> CarroCombate -> [CarroCombate] -> Bool
veEntre a b allCarros =
  let d_ab       = distanceBetween (posicionCarro a) (posicionCarro b) -- Guarda la distancia entre ambos carros
      -- Si la distancia es menor al rango de visión, a ve a b
      propio     = d_ab <= alcanceVision a
      -- Coge los carros c donde:
      --    posicionCarro c /= posicionCarro a -> evita cogerse a sí mismo
      --    cuyos team (equipoId) sea el mismo
      companeros = filter (\c -> team c == team a && posicionCarro c /= posicionCarro a) allCarros
      -- Existe algún compañero c que esté dentro del alcance de radio de a y vea a b directamente
      radioCheck = any (\c ->
                         distanceBetween (posicionCarro a) (posicionCarro c) <= alcanceRadio a
                      && distanceBetween (posicionCarro c) (posicionCarro b) <= alcanceVision c
                       ) companeros
  -- Devuelve si lo ve directamente o si algún copañero lo ve directamente
  in propio || radioCheck


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

-- ============ MUNICIÓN ============
-- ============ Tipos ============

data Municion = Municion
  { tipoMun    :: MunicionTipo
  , calibreMun :: Float      -- mm o valor abstracto
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
  } deriving (Show)

-- ============================================================
-- Munición: penetración y daño según tipo y calibre
-- ============================================================

penetracionEstim :: Municion -> Float
penetracionEstim (Municion { tipoMun = AP, calibreMun = c }) = c * 30.0
penetracionEstim (Municion { tipoMun = AE, calibreMun = c }) = c * 8.0

danioEstim :: Municion -> Int
-- round redondea al entero más cercano
danioEstim (Municion { tipoMun = AP, calibreMun = c }) = round (c * 6.0)
danioEstim (Municion { tipoMun = AE, calibreMun = c }) = round (c * 4.0)

-- ============================================================
-- Elección de munición y disparo
-- ============================================================

buscarMunicionPreferida :: MunicionTipo -> CarroCombate -> Maybe Int
buscarMunicionPreferida mtype carro =
  -- fmap extrae el índice del resultado máximo si existe
  fst <$> maximumByMay (comparing (calibreMun . snd)) seleccionadas
  where
    seleccionadas = filter ((== mtype) . tipoMun . snd) (zip [0..] (municiones carro))

    -- Versión segura de maximumBy (evita errores en listas vacías)
    maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
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

-- Devuelve (proyectil, atacanteSinEsaMunición)
dispararA :: Int -> CarroCombate -> CarroCombate -> Maybe (Proyectil, CarroCombate)
dispararA pid atacante objetivo = do
  idx <- elegirMunicionPara atacante objetivo
  let ms         = municiones atacante
      m          = ms !! idx
      dirGrados   = direccionCarro atacante -- El ángulo del tanque en grados
      dirRad      = deg2rad dirGrados       -- Convertimos a radianes para los cálculos
      -- Obtenemos el tamaño y posición del tanque que dispara
      (tankWidth, _) = tamanoCarro atacante
      (tankX, tankY) = posicionCarro atacante
      -- Calculamos una posición inicial justo delante del cañón del tanque.
      -- (La mitad del ancho del tanque + un pequeño espacio)
      offset      = tankWidth / 2 + 1.0
      pos         = (tankX + offset * cos dirRad, tankY + offset * sin dirRad)
      -- Asignamos la velocidad usando el mismo ángulo en radianes
      velProj     = (300 * cos dirRad, 300 * sin dirRad)
      proyectil  = Proyectil pid pos dirGrados velProj m (team atacante) (memoriaMun m)
      nuevaLista = take idx ms ++ drop (idx + 1) ms
      atacante'  = actualizarMuniciones atacante nuevaLista
  pure (proyectil, atacante')
  where
    actualizarMuniciones c nuevas =
      case c of
        obj@Objeto{ atributos = a@CarroAtributos{} } ->
          obj { atributos = a { municionesE = nuevas } }

-- ============================================================
-- Aplicar impacto directo
-- ============================================================

aplicarImpactoDirecto :: Municion -> CarroCombate -> Maybe CarroCombate
aplicarImpactoDirecto mun objetivo =
  calcularNuevaEnergia <$> validarObjetivo objetivo
  where
    validarObjetivo obj = if energia obj > 0 then Just obj else Nothing
    
    calcularNuevaEnergia obj =
      let dmg    = calcularDanioTotal mun obj
          nuevaE = max 0 (energia obj - dmg)
      in setEnergia nuevaE obj
    
    calcularDanioTotal m obj =
      let pen     = penetracionEstim m
          dmgBase = danioEstim m
      in calcularDanio (tipoMun m) pen dmgBase (blindaje obj)

-- Función PURA auxiliar para calcular el daño
calcularDanio :: MunicionTipo -> Float -> Int -> Float -> Int
calcularDanio AP pen dmgBase blind
  | pen > blind = dmgBase * 2
  | otherwise   = round (fromIntegral dmgBase * 0.6)
calcularDanio AE _ dmgBase _ = dmgBase

-- =========================
-- Tipos de tripulación
-- =========================

data EstadoTripulante = Vivo | Muerto
  deriving (Show, Eq)

data Tripulacion = Tripulacion
  { comandante    :: EstadoTripulante
  , conductor     :: EstadoTripulante
  , artillero     :: EstadoTripulante
  , operadorRadio :: EstadoTripulante
  , cargador      :: EstadoTripulante
  } deriving (Show, Eq)

-- =========================
-- Utilidades
-- =========================

todosVivos :: Tripulacion -> Bool
todosVivos t =
  comandante t    == Vivo &&
  conductor t     == Vivo &&
  artillero t     == Vivo &&
  operadorRadio t == Vivo &&
  cargador t      == Vivo

contarVivos :: Tripulacion -> Int
contarVivos t =
  length $ filter (== Vivo)
    [ comandante t, conductor t, artillero t, operadorRadio t, cargador t ]

-- ============================================================
-- Muerte aleatoria de tripulante (sin conocer Carro)
-- ============================================================

matarTripulanteAleatorioSiDanio :: Int -> Tripulacion -> IO Tripulacion
matarTripulanteAleatorioSiDanio dmg t
  | dmg <= 0  = return t
  | otherwise = do
      resultado <- matarTripulanteAleatorio t
      case resultado of
        Nothing -> return t  -- No había vivos, devuelve tripulación original
        Just t' -> return t' -- Devuelve tripulación con un muerto

-- Función PURA: obtener lista de tripulantes vivos
obtenerTripulantesVivos :: Tripulacion -> [String]
obtenerTripulantesVivos t =
  let roles = [ ("conductor",     conductor t)
              , ("artillero",     artillero t)
              , ("operadorRadio", operadorRadio t)
              , ("cargador",      cargador t)
              , ("comandante",    comandante t)
              ]
  in [ rol | (rol, estado) <- roles, estado == Vivo ]

-- Función PURA: matar un tripulante específico
matarTripulante :: String -> Tripulacion -> Tripulacion
matarTripulante "conductor"     t = t { conductor     = Muerto }
matarTripulante "artillero"     t = t { artillero     = Muerto }
matarTripulante "operadorRadio" t = t { operadorRadio = Muerto }
matarTripulante "cargador"      t = t { cargador      = Muerto }
matarTripulante "comandante"    t = t { comandante    = Muerto }
matarTripulante _               t = t

-- Versión con Maybe (devuelve Nothing si no hay vivos)
matarTripulanteAleatorio :: Tripulacion -> IO (Maybe Tripulacion)
matarTripulanteAleatorio t = do
  case obtenerTripulantesVivos t of
    [] -> return Nothing  -- No hay vivos
    disponibles -> do
      idx <- randomRIO (0, length disponibles - 1)
      let elegido = disponibles !! idx
      return (Just (matarTripulante elegido t))

-- ============================================================
-- Definición del Mundo
-- ============================================================

data Mundo = Mundo {
    carros      :: [CarroCombate],
    proyectiles :: [Proyectil],
    tamanoMundo :: Size,
    memoria     :: Memory
} deriving (Show)

-- Funciones para gestionar el estado del mundo
agregarCarro :: CarroCombate -> Mundo -> Mundo
agregarCarro carro mundo = mundo { carros = carro : carros mundo }

agregarProyectil :: Proyectil -> Mundo -> Mundo
agregarProyectil proyectil mundo = mundo { proyectiles = proyectil : proyectiles mundo }

removerCarro :: Int -> Mundo -> Mundo
removerCarro cid mundo = mundo { carros = filter ((/= cid) . carroId) (carros mundo) }

removerProyectil :: Int -> Mundo -> Mundo
removerProyectil pid mundo = mundo { proyectiles = filter ((/= pid) . proyectilId) (proyectiles mundo) }

carrosVistosPor :: CarroCombate -> Mundo -> [CarroCombate]
carrosVistosPor carro mundo =
  let allC         = carros mundo
      aAplic       = aplicarEfectosTripulacion carro
      allAplicados = map aplicarEfectosTripulacion allC
      visible b    = posicionCarro b /= posicionCarro carro && veEntre aAplic b allAplicados
  in filter visible allAplicados

  -- Función que muestra lo que ve cada carro
mostrarVisionDe :: Mundo -> IO ()
mostrarVisionDe mundo = mapM_ printVision (carros mundo)
  where
    printVision c = do
      let vistos      = carrosVistosPor c mundo
          eq          = team c
          tipo        = tipoCarro c
          vistosTipos = map tipoCarro vistos
      putStrLn $
        "Carro (team " ++ show eq ++ ", tipo " ++ show tipo ++
        ") ve " ++ show vistosTipos