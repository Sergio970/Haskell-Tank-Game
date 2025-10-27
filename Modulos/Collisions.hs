module Collisions where

import Types (Position, Size, Angle)
import Physics (distanceBetween, getRectVertices, polygonsIntersectSAT)
import Data.List (tails)
import Unidad (Mundo(..), CarroCombate, Proyectil, posicionCarro, tamanoCarro, direccionCarro, carroId, proyectiles, carros)
import Unidad (posicionProyectil, proyectilId)

data CollisionEvent
    = RobotHit Int Int          -- robot_id proyectil_id
    | RobotRobot Int Int        -- robot1_id robot2_id
    | FronteraCarro Int         -- robot_id
    | FronteraProyectil Int     -- proyectil_id
    deriving (Show, Eq)

-- Verifica colisión entre dos rectángulos usando SAT
checkCollision :: Position -> Size -> Angle -> Position -> Size -> Angle -> Bool
checkCollision posA sizeA angA posB sizeB angB =
    let va = getRectVertices posA sizeA angA
        vb = getRectVertices posB sizeB angB
    in polygonsIntersectSAT va vb

-- Detecta colisiones entre robots y proyectiles
detectRobotProjectileCollisions :: [CarroCombate] -> [Proyectil] -> [CollisionEvent]
detectRobotProjectileCollisions cs ps = concat
    [ if checkCollision (posicionCarro c) (tamanoCarro c) (direccionCarro c)
                        (posicionProyectil p) (2.0, 2.0) 0
      then [RobotHit (carroId c) (proyectilId p)] 
      else []
    | c <- cs, p <- ps ]

-- Detecta colisiones entre robots
detectRobotRobotCollisions :: [CarroCombate] -> [CollisionEvent]
detectRobotRobotCollisions cs = concat
    [ if checkCollision (posicionCarro c1) (tamanoCarro c1) (direccionCarro c1)
                        (posicionCarro c2) (tamanoCarro c2) (direccionCarro c2)
      then [RobotRobot (carroId c1) (carroId c2)] 
      else []
    | (c1:rest) <- tails cs, c2 <- rest ]

-- Detecta colisiones con los límites del mundo
detectWorldCollisions :: Mundo -> [CollisionEvent]
detectWorldCollisions m = 
    let (tamX, tamY) = tamanoMundo m
        cs = carros m
        ps = proyectiles m
        -- Verifica si un carro está fuera de los límites
        carroFuera c = let (x, y) = posicionCarro c
                       in x < (-tamX/2) || x > (tamX/2) || y < (-tamY/2) || y > (tamY/2)
        -- Verifica si un proyectil está fuera de los límites
        proyectilFuera p = let (x, y) = posicionProyectil p
                           in x < (-tamX/2) || x > (tamX/2) || y < (-tamY/2) || y > (tamY/2)
        carrosColisionados = [FronteraCarro (carroId c) | c <- cs, carroFuera c]
        proyectilesColisionados = [FronteraProyectil (proyectilId p) | p <- ps, proyectilFuera p]
    in carrosColisionados ++ proyectilesColisionados

-- Función principal que detecta todas las colisiones
checkCollisions :: Mundo -> [CollisionEvent]
checkCollisions m =
    let cs = carros m
        ps = proyectiles m
    in detectRobotProjectileCollisions cs ps 
       ++ detectRobotRobotCollisions cs 
       ++ detectWorldCollisions m