import qualified Data.Map.Strict as Map

type Position = (Float, Float)
type Point    = (Float, Float)
type Vector   = (Float, Float)
type Size     = (Float, Float)
type Angle    = Float
type Distance = Float

-- 1. Analiza el funcionamiento del juego y piensa en los tipos que son necesarios. 
-- Realiza una lista durante el análisis visual y posteriormente implementa dichos TADs.

-- Prompt a ChatGPT:
-- *Main> updatePositionR 5 r
-- <interactive>:5:1: error:
--     * No instance for (Show Robot) arising from a use of `print'
--     * In a stmt of an interactive GHCi command: print it

-- Por que da error esta funcion?

-- updatePositionR :: Float -> Robot -> Robot
-- updatePositionR dt (Robot pos dir vel size vida) =
--     Robot (updatePosition dt pos vel) dir vel size vida

-- updatePosition :: Float -> Position -> Vector -> Position
-- updatePosition dt (x, y) (vx, vy) = (x + vx * dt, y + vy * dt)

-- Para solucionar el error, hemos añadido al final de cada tipo deriving (Show), y así ver los resultados directamente en la terminal.

data TipoCarro = Ligero | Pesado | Cazacarros
    deriving (Show, Eq)

data MunicionTipo = AP | AE
    deriving (Show, Eq)

data MemoryValue = VInt Int
    | VFloat Float
    | VBool Bool
    | VStr String
    | VPos Position
    | VSize Size
    | VMunicionTipo MunicionTipo
    | VTipoCarro TipoCarro
    deriving (Show, Eq)

type Memory = Map.Map String MemoryValue

data Municion = Municion {
    tipoMun    :: MunicionTipo,
    calibreMun :: Float    -- por ejemplo mm o valor abstracto
} deriving (Show, Eq)

data CarroCombate = CarroCombate {
    carroId        :: Int,
    tipoCarro      :: TipoCarro,
    posicionCarro  :: Position,
    direccionCarro :: Angle,
    velocidadCarro :: Vector,
    tamanoCarro    :: Size,
    energia        :: Int,         -- "HP"
    blindaje       :: Float,       -- valor de blindaje
    alcanceVision  :: Distance,    -- base (sera modificado por tipo)
    municiones     :: [Municion],
    cadencia       :: Float,       -- tiempo entre disparos (segundos) base
    precisionBase  :: Float        -- 0..1 precision base
} deriving (Show, Eq)

data Proyectil = Proyectil {
    proyectilId        :: Int,
    posicionProyectil  :: Position,
    direccionProyectil :: Angle,
    velocidadProyectil :: Vector,
    municionProyectil  :: Municion
} deriving (Show)

data Mundo = Mundo {
    carros      :: [CarroCombate],
    proyectiles :: [Proyectil],
    tamanoMundo :: Size
} deriving (Show)

-- Eventos de colisión
data CollisionEvent
    = RobotHit Int Int     -- id del robot, id del proyectil
    | RobotRobot Int Int   -- id de robot 1, id de robot 2
    deriving (Show, Eq)

-- 2. Refactoriza las funciones implementadas hasta ahora para usar pattern matching,
-- listas por comprensión y cláusulas where, if-then-else, guardas o case-of cuando proceda.

distanceBetween :: Position -> Position -> Distance -- Con cláusula where y Pattern matching en las tuplas (x1,y1) y (x2,y2).
distanceBetween (x1, y1) (x2, y2) = sqrt dx2dy2
    where
        dx = x2 - x1
        dy = y2 - y1
        dx2dy2 = dx ** 2 + dy ** 2

angleToTarget :: Position -> Position -> Angle -- Con cláusula where y  Pattern matching en las tuplas (x1,y1) y (x2,y2).
angleToTarget (x1, y1) (x2, y2) = atan2 dy dx
    where
        dx = x2 - x1
        dy = y2 - y1

deg2rad :: Angle -> Angle -- No cambiado
deg2rad deg = deg * (pi / 180.0)

rad2deg :: Angle -> Angle -- No cambiado
rad2deg rad = rad * (180.0 / pi)

subVec :: Vector -> Vector -> Vector -- No cambiado
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

getVertices :: (Point, Point, Point, Point, Angle) -> [Point] -- Lista por compresión y let ... in
getVertices (p1, p2, p3, p4, ang) =
    let rotate (x,y) ang = (x * cos ang - y * sin ang, x * sin ang + y * cos ang)
    in [rotate p ang | p <- [p1, p2, p3, p4]]

dot :: Point -> Point -> Float -- No cambiado
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

sub :: Point -> Point -> Point -- No cambiado
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2) 

perp :: Vector -> Vector -- Case-of
perp v = 
    case v of
        (x, y) -> (-y, x)

isInBounds :: Point -> Size -> Bool -- Guardas
isInBounds (x, y) (w, h)
    | x < 0 || y < 0 = False   
    | x > w || y > h = False
    | otherwise = True

-- 3. Implementa las siguientes funciones usando pattern matching con los TADs definidos anteriormente:

-- Prompt a ChatGPT:
-- Dado un juego de robots que disparan proyectiles en un mundo en todas direcciones comprueba que estás funciones sean correctas

-- Para asegurarnos que las funciones estaban bien le preguntamos que verificase cada una de las funciones.

-- detectedAgent: Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar

detectedAgent :: CarroCombate -> CarroCombate -> Distance -> Bool
detectedAgent (CarroCombate _ _ pos1 _ _ _ _ _ _ _ _ _) (CarroCombate _ _ pos2 _ _ _ _ _ _ _ _ _) rango = -- Los valores direccion, velocidad, tamaño y energia son irrelevantes
    distanceBetween pos1 pos2 <= rango

-- isRobotAlive: True si la energía del robot es mayor a 0

isRobotAlive :: CarroCombate -> Bool
isRobotAlive (CarroCombate _ _ _ _ _ _ energia _ _ _ _ _) = energia > 0

-- countActiveRobots: Contar los robots que están vivos
    -- Función para una lista

countActiveRobots :: [CarroCombate] -> Int
countActiveRobots carros = length [c | c <- carros, isRobotAlive c]

    -- Función para un mundo

countActiveRobotsMundo :: Mundo -> Int
countActiveRobotsMundo mundo = length [c | c <- carros mundo, isRobotAlive c]

-- updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada

updateRobotVelocity :: CarroCombate -> Vector -> CarroCombate
updateRobotVelocity (CarroCombate id tipo pos dir _ tam energia blindaje vision municiones cadencia precision) nuevaVel =
    CarroCombate id tipo pos dir nuevaVel tam energia blindaje vision municiones cadencia precision

-- updateVelocity: Actualizar velocidad basada en la acción de movimiento
updateVelocity :: CarroCombate -> Vector -> CarroCombate
updateVelocity (CarroCombate id tipo pos dir vel tam energia blindaje vision municiones cadencia precision) accionBot =
    CarroCombate id tipo pos dir (addVec vel accionBot) tam energia blindaje vision municiones cadencia precision
  where
    addVec :: Vector -> Vector -> Vector
    addVec (x1,y1) (x2,y2) = (x1+x2, y1+y2)
    
-- updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo

    -- Función genérica

updatePosition :: Float -> Position -> Vector -> Position
updatePosition dt (x, y) (vx, vy) = (x + vx * dt, y + vy * dt)

    -- Para un robot

updatePositionR :: Float -> CarroCombate -> CarroCombate
updatePositionR dt (CarroCombate id tipo pos dir vel tam energia blindaje vision municiones cadencia precision) =
    CarroCombate id tipo (updatePosition dt pos vel) dir vel tam energia blindaje vision municiones cadencia precision

    -- Para un proyectil

updatePositionP :: Float -> Proyectil -> Proyectil
updatePositionP dt (Proyectil id pos dir vel mun) =
    Proyectil id (updatePosition dt pos vel) dir vel mun

-- mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)

mul :: (Float, Float) -> (Float, Float) -> (Float, Float)
mul (w, h) (sw, sh) = (w * sw, h * sh)

-- Implementar las siguientes funciones de colisión:

-- checkCollision: Comprueba si dos rectángulos han colisionado utilizando el algoritmo apropiado.

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

checkCollision :: Position -> Size -> Angle -> Position -> Size -> Angle -> Bool
checkCollision posA sizeA angA posB sizeB angB =
    let va = getRectVertices posA sizeA angA
        vb = getRectVertices posB sizeB angB
    in polygonsIntersectSAT va vb

-- detectRobotProjectileCollisions: Verifica qué proyectiles han colisionado con algún agente. Cuando detecte una colisión, debe generar el evento de colisión correspondiente.

detectRobotProjectileCollisions = undefined

-- detectRobotRobotCollisions: Comprueba y detecta las colisiones entre los diferentes robots del juego. Deberá generar el evento de colisión correspondiente.

detectRobotRobotCollisions = undefined

-- checkCollisions: Función principal que coordina todas las comprobaciones de colisión.

checkCollisions :: [CarroCombate] -> [Proyectil] -> [CollisionEvent]
checkCollisions carros proyectiles =
    detectRobotProjectileCollisions carros proyectiles ++ detectRobotRobotCollisions carros

-- Sistema de Memoria para Agentes

-- Los agentes necesitan una memoria (implementada como diccionario) para tomar decisiones inteligentes. 
-- Crea un tipo de datos flexible que pueda almacenar diferentes tipos de información: 
-- enteros, cadenas de texto, puntos/coordenadas, booleanos, etc.

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

memoriaTamanoMundo :: Memory
memoriaTamanoMundo = Map.fromList
    [ ("tamanoMundo", VSize (50, 50)) ]