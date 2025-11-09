type Point = (Float, Float)
type Vector = (Float, Float)
type Angle = Float
type Distance = Float
type Position = Point
type Size = (Float, Float)

-- 1. Analiza el funcionamiento del juego y piensa en los tipos que son necesarios. 
-- Realiza una lista durante el análisis visual y posteriormente implementa dichos TADs.

-- Robot. Tienen los campos de posición, dirección, velocidad, tamaño y energia.
data Robot = 
    Robot {
        posicionRobot :: Position, direccionRobot :: Angle, velocidadRobot :: Vector, tamanoRobot :: Size, energia :: Int
    }

    deriving (Show)

-- Proyectiles. Tienen los campos de posición, dirección y velocidad.
data Proyectil =
    Proyectil {
        posicionProyectil :: Position, direccionProyectil :: Angle, velocidadProyectil :: Vector
    }

    deriving (Show)
    
-- Mundo. Contiene a los jugadores, los proyectiles y el tamaño del espacio disponible.
data Mundo = 
    Mundo {
        jugadores :: [Robot], proyectiles :: [Proyectil], tamanoMundo :: Size
    }

    deriving (Show)

-- 2. Refactoriza las funciones implementadas hasta ahora para usar pattern matching,
-- listas por comprensión y cláusulas where, if-then-else, guardas o case-of cuando proceda.

distanceBetween :: Position -> Position -> Distance -- Con cláusula where
distanceBetween (x1, y1) (x2, y2) = sqrt dx2dy2
    where
        dx = x2 - x1
        dy = y2 - y1
        dx2dy2 = dx ** 2 + dy ** 2

angleToTarget :: Position -> Position -> Angle -- Con cláusula where
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

-- detectedAgent: Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar

detectedAgent :: Robot -> Robot -> Distance -> Bool
detectedAgent (Robot pos1 _ _ _ _) (Robot pos2 _ _ _ _) rango = -- Los valores direccion, velocidad, tamaño y energia son irrelevantes
    distanceBetween pos1 pos2 <= rango

-- isRobotAlive: True si la energía del robot es mayor a 0

isRobotAlive :: Robot -> Bool
isRobotAlive (Robot _ _ _ _ energia) = energia > 0

-- countActiveRobots: Contar los robots que están vivos

-- Función para una lista

countActiveRobots :: [Robot] -> Int
countActiveRobots robots = length [r | r <- robots, isRobotAlive r]

-- Función para un mundo

countActiveRobots :: Mundo -> Int
countActiveRobots mundo = length [r | r <- jugadores mundo, isRobotAlive r]

-- updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada
-- updateVelocity: Actualizar velocidad basada en la acción de movimiento
-- updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo

-- Función genérica

updatePosition :: Float -> Position -> Vector -> Position
updatePosition dt (x, y) (vx, vy) = (x + vx * dt, y + vy * dt)

-- Para un robot

updatePositionR :: Float -> Robot -> Robot
updatePositionR dt (Robot pos dir vel size vida) =
    Robot (updatePosition dt pos vel) dir vel size vida

-- Para un proyectil

updatePositionP :: Float -> Proyectil -> Proyectil
updatePositionP dt (Proyectil pos dir vel) =
    Proyectil (updatePosition dt pos vel) dir vel

-- mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)

mul :: (Float, Float) -> (Float, Float) -> (Float, Float)
mul (w, h) (sw, sh) = (w * sw, h * sh)