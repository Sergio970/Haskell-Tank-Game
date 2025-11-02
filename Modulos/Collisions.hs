module Collisions where

import Types (Position, Size, Angle)
import Physics (distanceBetween, getRectVertices, polygonsIntersectSAT)
import Data.List (tails)
import Unidad

data CollisionEvent
    = RobotHit Int Int          -- robot_id proyectil_id
    | RobotRobot Int Int        -- robot1_id robot2_id
    | FronteraCarro Int         -- robot_id
    | FronteraProyectil Int     -- proyectil_id
    | RobotMeteorito Int Int       -- robot_id meteorito_id
    | ProyectilMeteorito Int Int   -- ← proyectil_id meteorito_id
    | RobotEstela Int Int
    | RobotBomba Int Int           -- ← robot_id bomba_id
    | MeteoritoBomba Int Int       -- ← meteorito_id bomba_id
    deriving (Show, Eq)

-- Verifica colisión entre dos rectángulos usando SAT
checkCollision :: Position -> Size -> Angle -> Position -> Size -> Angle -> Bool
checkCollision posA sizeA angA posB sizeB angB =
    let va = getRectVertices posA sizeA angA
        vb = getRectVertices posB sizeB angB
    in polygonsIntersectSAT va vb

checkCollisionCircle :: Position -> Float -> Position -> Float -> Bool
checkCollisionCircle (x1, y1) r1 (x2, y2) r2 =
  let dx = x2 - x1
      dy = y2 - y1
      dist = sqrt (dx*dx + dy*dy)
  in dist < (r1 + r2)

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

-- Detecta colisiones entre robots y meteoritos (círculo simple)
detectRobotMeteoritoCollisions :: [CarroCombate] -> [Meteorito] -> [CollisionEvent]
detectRobotMeteoritoCollisions cs mets = concat
  [ if checkCollisionCircle (posicionCarro c) 15 (posicionMeteorito m) (tamanoMeteorito m)
    then [RobotMeteorito (carroId c) (meteoritoId m)]
    else []
  | c <- cs, m <- mets ]

detectProyectilMeteoritoCollisions :: [Proyectil] -> [Meteorito] -> [CollisionEvent]
detectProyectilMeteoritoCollisions ps mets = concat
  [ if checkCollisionCircle (posicionProyectil p) 2.0 (posicionMeteorito m) (tamanoMeteorito m)
    then [ProyectilMeteorito (proyectilId p) (meteoritoId m)]
    else []
  | p <- ps, m <- mets ]

detectRobotEstelaCollisions :: [CarroCombate] -> [Meteorito] -> [CollisionEvent]
detectRobotEstelaCollisions cs mets =
  let todasEstelas = [(e, met) | met <- mets, e <- estelas met]
  in concat
    [ if checkCollisionCircle (posicionCarro c) 15 (estelaPos e) (estelaRadio e)
      then [RobotEstela (carroId c) (estelaId e)]
      else []
    | c <- cs, (e, met) <- todasEstelas ]

-- Nuevas: detecciones con bombas
detectRobotBombaCollisions :: [CarroCombate] -> [Bomba] -> [CollisionEvent]
detectRobotBombaCollisions cs bs = concat
  [ if checkCollisionCircle (posicionCarro c) 15 (posicionBomba b) (radioBomba b)
    then [RobotBomba (carroId c) (bombaId b)]
    else []
  | c <- cs, b <- bs ]

detectMeteoritoBombaCollisions :: [Meteorito] -> [Bomba] -> [CollisionEvent]
detectMeteoritoBombaCollisions mets bs = concat
  [ if checkCollisionCircle (posicionMeteorito m) (tamanoMeteorito m) (posicionBomba b) (radioBomba b)
    then [MeteoritoBomba (meteoritoId m) (bombaId b)]
    else []
  | m <- mets, b <- bs ]

-- Función principal que detecta todas las colisiones
checkCollisions :: Mundo -> [CollisionEvent]
checkCollisions m =
    let cs  = carros m
        ps  = proyectiles m
        obs = obstaculos m
        bs  = bombas m
    in  detectRobotProjectileCollisions cs ps 
     ++ detectRobotRobotCollisions cs 
     ++ detectWorldCollisions m
     ++ detectRobotMeteoritoCollisions cs obs
     ++ detectProyectilMeteoritoCollisions ps obs
     ++ detectRobotEstelaCollisions cs obs
     ++ detectRobotBombaCollisions cs bs
     ++ detectMeteoritoBombaCollisions obs bs