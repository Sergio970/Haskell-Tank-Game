module Collisions where

import Types (Position, Size, Angle)
import Physics (distanceBetween) -- if needed for future
import Data.List (tails)
import Unidad (Mundo(..), CarroCombate, Proyectil, posicionCarro, tamanoCarro, direccionCarro, carroId, proyectiles, carros)
import Unidad (posicionProyectil, proyectilId) -- accessors come from record fields

data CollisionEvent
  = RobotHit Int Int
  | RobotRobot Int Int
  | FronteraCarro Int
  | FronteraProyectil Int
  deriving (Show, Eq)

-- Replace with your real geometry when available
getRectVertices :: Position -> Size -> Angle -> [(Float,Float)]
getRectVertices (x,y) (w,h) _ = let hw=w/2; hh=h/2 in [(x-hw,y-hh),(x+hw,y-hh),(x+hw,y+hh),(x-hw,y+hh)]

polygonsIntersectSAT :: [(Float,Float)] -> [(Float,Float)] -> Bool
polygonsIntersectSAT a b =
  let (ax,ay) = head a; (bx,by) = head b in not (null a) && not (null b) -- placeholder

checkCollision :: Position -> Size -> Angle -> Position -> Size -> Angle -> Bool
checkCollision posA sizeA angA posB sizeB angB =
  let va = getRectVertices posA sizeA angA
      vb = getRectVertices posB sizeB angB
  in polygonsIntersectSAT va vb

proyectilAsRect :: Proyectil -> (Position, Size, Angle)
proyectilAsRect p = (posicionProyectil p, (0.1,0.1), 0)

detectRobotProjectileCollisions :: [CarroCombate] -> [Proyectil] -> [CollisionEvent]
detectRobotProjectileCollisions cs ps = concat
  [ if checkCollision (posicionCarro c) (tamanoCarro c) (direccionCarro c)
                      (posicionProyectil p) (0.1,0.1) 0
      then [RobotHit (carroId c) (proyectilId p)] else []
  | c <- cs, p <- ps ]

detectRobotRobotCollisions :: [CarroCombate] -> [CollisionEvent]
detectRobotRobotCollisions cs = concat
  [ if checkCollision (posicionCarro c1) (tamanoCarro c1) (direccionCarro c1)
                      (posicionCarro c2) (tamanoCarro c2) (direccionCarro c2)
      then [RobotRobot (carroId c1) (carroId c2)] else []
  | (c1:rest) <- tails cs, c2 <- rest ]

detectWorldCollisions :: Mundo -> [CollisionEvent]
detectWorldCollisions m = []

checkCollisions :: Mundo -> [CollisionEvent]
checkCollisions m =
  let cs = carros m
      ps = proyectiles m
  in detectRobotProjectileCollisions cs ps ++ detectRobotRobotCollisions cs
