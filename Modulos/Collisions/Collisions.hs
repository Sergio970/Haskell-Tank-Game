module Collisions.Collisions where

import Types.Types
import Entities.Unidad
import Physics.Physics (getRectVertices)
import Data.List (tails)

data CollisionEvent
  = RobotHit Int Int     -- id del carro, id del proyectil
  | RobotRobot Int Int   -- id de carro 1, id de carro 2
  deriving (Show, Eq)

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