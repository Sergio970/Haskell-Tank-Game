-- PRACTICA 3 FINAL: Carros de Combate con tripulación, visión por radio y munición con calibre
{-# LANGUAGE RecordWildCards #-}

import System.Random (randomRIO)
import Data.List (elemIndex, delete, nub)
import qualified Data.Map.Strict as Map
import Data.List (tails)

type Point = (Float, Float)
type Vector = (Float, Float)
type Angle = Float
type Distance = Float
type Position = Point
type Size = (Float, Float)

-- Tipos de valores que puede guardar la memoria
data Value
  = VInt Int
  | VFloat Float
  | VBool Bool
  | VString String
  | VPoint Point
  | VSize Size
  | VTipoCarro TipoCarro
  | VMunicionTipo MunicionTipo
  deriving (Show, Eq)

-- Tipo de la memoria: un diccionario de clave -> valor
type Memory = Map.Map String Value

-- Operaciones vectoriales
addVec, subVec :: Vector -> Vector -> Vector
addVec (x1,y1) (x2,y2) = (x1+x2, y1+y2)
subVec (x1,y1) (x2,y2) = (x1-x2, y1-y2)

dot :: Vector -> Vector -> Float
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

perp :: Vector -> Vector
perp (x,y) = (-y, x)

normalize :: Vector -> Vector
normalize (x, y) =
    let mag = sqrt (x*x + y*y)
    in if mag == 0 then (0,0) else (x / mag, y / mag)

-- Vértices de un rectángulo centrado en (cx,cy), tamaño (w,h), rotado un ángulo
getRectVertices :: Position -> Size -> Angle -> [Position]
getRectVertices (cx, cy) (w,h) ang =
  [ (cx + x * cos ang - y * sin ang, cy + x * sin ang + y * cos ang)
  | (x,y) <- [(-hw,-hh), (-hw,hh), (hw,hh), (hw,-hh)] ]
  where
    hw = w/2
    hh = h/2

-- Proyección de un polígono en un eje
projectPolygon :: [Position] -> Vector -> (Float, Float)
projectPolygon poly axis =
  let projs = [dot p axis | p <- poly]
  in (minimum projs, maximum projs)

-- Comprobar solapamiento en un eje
overlapOnAxis :: [Position] -> [Position] -> Vector -> Bool
overlapOnAxis polyA polyB axis =
  let (amin, amax) = projectPolygon polyA axis
      (bmin, bmax) = projectPolygon polyB axis
  in not (amax < bmin || bmax < amin)

-- SAT completo
polygonsIntersectSAT :: [Position] -> [Position] -> Bool
polygonsIntersectSAT a b =
  all (axisOverlap a b) axes
  where
    edges vs = zip vs (tail (cycle vs))
    normals vs = [ normalize (perp (subVec v2 v1)) | (v1,v2) <- edges vs ]
    axes = normals a ++ normals b
    axisOverlap pa pb axis = overlapOnAxis pa pb axis

-- ============================================================
-- Tipos: carro, tripulación, munición, mundo
-- ============================================================

data TipoCarro = Ligero | Pesado | Cazacarros
    deriving (Show, Eq)

data MunicionTipo = AP | AE
    deriving (Show, Eq)

-- Munición con calibre -> permite estimar penetración y daño
data Municion = Municion {
    tipoMun       :: MunicionTipo,
    calibreMun    :: Float,    -- por ejemplo mm o valor abstracto
    memoriaMun       :: Memory
} deriving (Show, Eq)

data EstadoTripulante = Vivo | Muerto
    deriving (Show, Eq)

data Tripulacion = Tripulacion {
    comandante    :: EstadoTripulante,
    conductor     :: EstadoTripulante,
    artillero     :: EstadoTripulante,
    operadorRadio :: EstadoTripulante,
    cargador      :: EstadoTripulante
} deriving (Show, Eq)

data CarroCombate = CarroCombate {
    carroId        :: Int,
    team           :: Int,         -- equipo identificador
    tipoCarro      :: TipoCarro,
    posicionCarro  :: Position,
    direccionCarro :: Angle,
    velocidadCarro :: Vector,
    tamanoCarro    :: Size,
    energia        :: Int,         -- "HP"
    blindaje       :: Float,       -- valor de blindaje
    alcanceVision  :: Distance,    -- base (sera modificado por tipo)
    alcanceRadio   :: Distance,    -- base (será modificado por tripulación)
    tripulacion    :: Tripulacion,
    municiones     :: [Municion],
    cadencia       :: Float,       -- tiempo entre disparos (segundos) base
    precisionBase  :: Float,        -- 0..1 precision base
    memoriaCarro        :: Memory
} deriving (Show)

data Proyectil = Proyectil {
    proyectilId        :: Int,
    posicionProyectil  :: Position,
    direccionProyectil :: Angle,
    velocidadProyectil :: Vector,
    municionProyectil  :: Municion,
    disparadorTeam     :: Int,
    memoriaProj        :: Memory
} deriving (Show)

data Mundo = Mundo {
    carros      :: [CarroCombate],
    proyectiles :: [Proyectil],
    tamanoMundo :: Size,
    memoria     :: Memory
} deriving (Show)

-- ============================================================
-- Parámetros de diseño: cómo la tripulación y tipo afectan
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
-- Matemáticas auxiliares
-- ============================================================

distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) = sqrt dx2dy2
  where
    dx = x2 - x1
    dy = y2 - y1
    dx2dy2 = dx ** 2 + dy ** 2

updatePosition :: Float -> Position -> Vector -> Position
updatePosition dt (x, y) (vx, vy) = (x + vx * dt, y + vy * dt)

-- ============================================================
-- Tripulación: efectos sobre parámetros del carro
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

-- Decide qué muni usar contra un objetivo: prefer AP si su penetración > blindaje objetivo
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

-- ============================================================
-- Daño al carro y posibilidad de matar tripulante aleatorio (si hay daño y hay tripulantes vivos)
-- ============================================================

aplicarDanioConMuerteAleatoria :: Int -> CarroCombate -> IO CarroCombate
aplicarDanioConMuerteAleatoria dmg carro = do
    let nuevaEnergia = energia carro - dmg
        carroConEnergia = carro { energia = max 0 nuevaEnergia }
    carroConPosibleMuerte <- matarTripulanteAleatorioSiDanio dmg carroConEnergia
    return (aplicarEfectosTripulacion carroConPosibleMuerte)

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
-- Ejemplo: crear carros, munición y un mundo
-- ============================================================

-- Helper para crear tripulación totalmente viva
tripulacionViva :: Tripulacion
tripulacionViva = Tripulacion Vivo Vivo Vivo Vivo Vivo

-- Crear Carros de ejemplo con ID incluido
carroLigero :: Int -> Int -> Position -> CarroCombate
carroLigero cid equipo pos = aplicarEfectosTripulacion $ CarroCombate {
    carroId = cid,
    team = equipo,
    tipoCarro = Ligero,
    posicionCarro = pos,
    direccionCarro = 0.0,
    velocidadCarro = (10.0, 0.0),
    tamanoCarro = (3.0, 3.0),
    energia = 100,
    blindaje = blindajeBase Ligero,
    alcanceVision = visionBase Ligero,
    alcanceRadio = radioBase Ligero,
    tripulacion = tripulacionViva,
    municiones = replicate 3 (Municion AP 75.0 memoriaMunicionAP) ++ replicate 2 (Municion AE 120.0 memoriaMunicionAE),
    cadencia = 1.0,
    precisionBase = 0.9,
    memoriaCarro = memoriaLigero
}

carroPesado :: Int -> Int -> Position -> CarroCombate
carroPesado cid equipo pos = aplicarEfectosTripulacion $ CarroCombate {
    carroId = cid,
    team = equipo,
    tipoCarro = Pesado,
    posicionCarro = pos,
    direccionCarro = 0.0,
    velocidadCarro = (4.0, 0.0),
    tamanoCarro = (5.0, 5.0),
    energia = 300,
    blindaje = blindajeBase Pesado,
    alcanceVision = visionBase Pesado,
    alcanceRadio = radioBase Pesado,
    tripulacion = tripulacionViva,
    municiones = replicate 4 (Municion AP 120.0 memoriaMunicionAP) ++ replicate 3 (Municion AE 150.0 memoriaMunicionAE),
    cadencia = 2.0,
    precisionBase = 0.8,
    memoriaCarro = memoriaPesado
}

cazacarros :: Int -> Int -> Position -> CarroCombate
cazacarros cid equipo pos = aplicarEfectosTripulacion $ CarroCombate {
    carroId = cid,
    team = equipo,
    tipoCarro = Cazacarros,
    posicionCarro = pos,
    direccionCarro = 0.0,
    velocidadCarro = (7.0, 0.0),
    tamanoCarro = (4.0, 4.0),
    energia = 180,
    blindaje = blindajeBase Cazacarros,
    alcanceVision = visionBase Cazacarros,
    alcanceRadio = radioBase Cazacarros,
    tripulacion = tripulacionViva,
    municiones = replicate 3 (Municion AP 110.0 memoriaMunicionAP) ++ replicate 2 (Municion AE 140.0 memoriaMunicionAE),
    cadencia = 1.5,
    precisionBase = 0.85,
    memoriaCarro = memoriaCazacarros
}

c1 = carroLigero 1 1 (10,10)
c2 = carroPesado 2 1 (40,20)
c3 = cazacarros 3 2 (200,20)

-- Mundo de ejemplo con dos equipos
mundoEjemplo :: Mundo
mundoEjemplo = Mundo {
    carros = [c1, c2, c3],
    proyectiles = [],
    tamanoMundo = (1000, 1000),
    memoria = memoriaMundo
}

-- ============================================================
-- Ejemplo de uso (simulación simplificada)
-- ============================================================

-- Simular que a ataca b: dispara, se crea proyectil y al impactar aplicamos daño
-- Para demo: hacemos impacto instantáneo (sin simular trayectoria)
ataqueInstantaneo :: Int -> CarroCombate -> CarroCombate -> IO (CarroCombate, Maybe CarroCombate)
ataqueInstantaneo pid atacante objetivo =
    case dispararA pid atacante objetivo of
        Nothing -> return (atacante, Nothing) -- sin munición
        Just (proj, atacante') -> do
            -- Calculamos daño según munición y blindaje
            let m = municionProyectil proj
                objetivoAfter = aplicarImpactoDirecto m objetivo
                dmg = energia objetivo - energia objetivoAfter
            objetivoFinal <- aplicarDanioConMuerteAleatoria dmg objetivoAfter
            return (atacante', Just objetivoFinal)

-- Función para mostrar lo que ve cada carro en un mundo
mostrarVisionDe :: Mundo -> IO ()
mostrarVisionDe mundo = mapM_ printVision (carros mundo)
  where
    printVision c = do
        let vistos = carrosVistosPor c mundo
        putStrLn $ "Carro (team " ++ show (team c) ++ ", tipo " ++ show (tipoCarro c) ++
                   ") ve " ++ show (map (tipoCarro) vistos)

-- ============================================================
-- Nota:
-- - Valores (penetración, daños, multiplicadores) son ejemplos y se pueden afinar.
-- - La simulación de trayectoria y tiempos entre disparos no está detallada; el ejemplo
--   `ataqueInstantaneo` asume impacto inmediato para facilitar presentación.
-- - Para ejecutar todo en GHCi: cargar el archivo y usar `mostrarVisionDe mundoEjemplo`
--   o probar `ataqueInstantaneo` entre carros de `mundoEjemplo`.
-- ============================================================


-- Eventos de colisión
data CollisionEvent
  = RobotHit Int Int     -- id del robot, id del proyectil
  | RobotRobot Int Int   -- id de robot 1, id de robot 2
  deriving (Show, Eq)

-- 1. Implementar las siguientes funciones de colisión:

-- checkCollision: Comprueba si dos rectángulos han colisionado utilizando el algoritmo apropiado.

checkCollision :: Position -> Size -> Angle -> Position -> Size -> Angle -> Bool
checkCollision posA sizeA angA posB sizeB angB =
    let va = getRectVertices posA sizeA angA
        vb = getRectVertices posB sizeB angB
    in polygonsIntersectSAT va vb

-- Dado un carro y un proyectil, decide si colisionan (usando checkCollision)
-- Necesitamos extraer posición, tamaño y ángulo de ambos
-- Para esto, voy a asumir que el proyectil tiene un “tamaño” y “ángulo” ficticios (o cero) — modifica según tu caso.
proyectilAsRect :: Proyectil -> (Position, Size, Angle)
proyectilAsRect proj =
  let pos = posicionProyectil proj
      -- Asumimos que el proyectil es un punto o un rectángulo muy pequeño:
      sz = (0.1, 0.1)  -- por ejemplo
      ang = 0
  in (pos, sz, ang)

-- detectRobotProjectileCollisions: compara cada carro con cada proyectil
detectRobotProjectileCollisions :: [CarroCombate] -> [Proyectil] -> [CollisionEvent]
detectRobotProjectileCollisions carros proyectiles =
  [ RobotHit (carroId car) (proyectilId proj)
  | car <- carros
  , proj <- proyectiles
  , let (posC, szC, angC) = (posicionCarro car, tamanoCarro car, direccionCarro car)
        (posP, szP, angP) = proyectilAsRect proj
  , checkCollision posC szC angC posP szP angP
  ]

-- detectRobotRobotCollisions: compara cada par de carros (sin repetir)
detectRobotRobotCollisions :: [CarroCombate] -> [CollisionEvent]
detectRobotRobotCollisions carros =
  [ RobotRobot (carroId c1) (carroId c2)
  | (c1:rest) <- tails carros
  , c2 <- rest
  , let (p1, s1, a1) = (posicionCarro c1, tamanoCarro c1, direccionCarro c1)
        (p2, s2, a2) = (posicionCarro c2, tamanoCarro c2, direccionCarro c2)
  , checkCollision p1 s1 a1 p2 s2 a2
  ]

-- checkCollisions: combinación de ambas
checkCollisions :: [CarroCombate] -> [Proyectil] -> [CollisionEvent]
checkCollisions carros proyectiles =
  detectRobotProjectileCollisions carros proyectiles ++ detectRobotRobotCollisions carros

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

-- Bot de ejemplo: recibe el mundo y su propio carro, devuelve lista de acciones
botEjemplo :: Mundo -> CarroCombate -> [BotAction]
botEjemplo mundo carro =
    let visibles = carrosVistosPor carro mundo
    in case visibles of
        [] -> [Esperar]  -- No ve enemigos, espera
        (enemigo:_) ->   -- Ve al menos un enemigo, dispara al primero
            [DispararA (carroId enemigo)]

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
