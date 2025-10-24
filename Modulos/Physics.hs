module Physics where

import Types

-- Convierte radianes a grados
rad2deg :: Float -> Float
rad2deg rad = rad * (180.0 / pi)

-- Convierte grados a radianes
deg2rad :: Float -> Float
deg2rad deg = deg * (pi / 180.0)

-- Operaciones vectoriales
addVec, subVec :: Vector -> Vector -> Vector
addVec (x1,y1) (x2,y2) = (x1+x2, y1+y2)
subVec (x1,y1) (x2,y2) = (x1-x2, y1-y2)

-- Producto escalar
dot :: Vector -> Vector -> Float
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

--  Calcula el vector perpendicular
perp :: Vector -> Vector
perp (x,y) = (-y, x)

-- Calcula el vector normal
normalize :: Vector -> Vector
normalize (x, y) =
    let mag = sqrt (x*x + y*y)
    in if mag == 0 then (0,0) else (x / mag, y / mag)

-- Vértices de un rectángulo centrado en (cx,cy), tamaño (w,h), rotado un ángulo
-- Devuelve las coordenadas de los vértices de un rectángulo, dados su centro, tamaño y ángulo de rotación
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
    -- Hace una lista con las distancias entre cada punto del rectángulo y el eje
    let projs = [dot p axis | p <- poly]
    -- Obtiene solo el mínimo y máximo
    in (minimum projs, maximum projs)

-- Comprobar solapamiento en un eje
overlapOnAxis :: [Position] -> [Position] -> Vector -> Bool
overlapOnAxis polyA polyB axis =
    -- Obtiene los máximos y mínimos en cada polígono
    let (amin, amax) = projectPolygon polyA axis
        (bmin, bmax) = projectPolygon polyB axis
    -- Compara que el máximo de a no sea menor que el mínimo de b y viceversa
    in not (amax < bmin || bmax < amin)

-- Sistema de detección de colisiones entre los tanques y los proyectiles, utilizando el algoritmo SAT — Separating Axis Theorem
polygonsIntersectSAT :: [Position] -> [Position] -> Bool
polygonsIntersectSAT a b =
  all (axisOverlap a b) axes
  where
    -- Calculamos las aristas de cada polígono
    edges vs = zip vs (tail (cycle vs))
    -- Obtenemos los vectores normales perpendiculares de cada arista
    normals vs = [ normalize (perp (subVec v2 v1)) | (v1,v2) <- edges vs ]
    -- Añadimos todos los ejes en una lista que le pasaremos a la función overlapOnAxis
    axes = normals a ++ normals b
    -- Prueba que no solapa cada eje con ninguna de las dos proyecciones
    axisOverlap pa pb axis = overlapOnAxis pa pb axis

-- Distancia euclidiana entre dos puntos
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) = sqrt dx2dy2
  where
    dx = x2 - x1
    dy = y2 - y1
    dx2dy2 = dx ** 2 + dy ** 2

-- Actualizar posición según velocidad y delta time
updatePosition :: Float -> Position -> Vector -> Position
updatePosition dt (x, y) (vx, vy) = (x + vx * dt, y + vy * dt)