module Physics.Physics where

import Types.Types

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