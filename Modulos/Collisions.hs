module Collisions where

import Types
import Unidad
import Physics (getRectVertices)
import Data.List (tails)

import Data.Maybe (mapMaybe)

data CollisionEvent
  = RobotHit Int Int     -- id del carro, id del proyectil
  | RobotRobot Int Int   -- id de carro 1, id de carro 2
  deriving (Show, Eq)

-- checkCollision: Comprueba si dos rectángulos han colisionado utilizando el algoritmo apropiado.

checkCollision :: Position -> Size -> Angle -> Position -> Size -> Angle -> Bool
checkCollision posA sizeA angA posB sizeB angB =
  -- Añade en variables las listas de vértices que forman el rectángulo de colisión de los tanques a y b
    let va = getRectVertices posA sizeA angA
        vb = getRectVertices posB sizeB angB
    in polygonsIntersectSAT va vb

-- Dado un carro y un proyectil, decide si colisionan (usando checkCollision)
-- Necesitamos extraer posición, tamaño y ángulo de ambos
-- Para esto, voy a asumir que el proyectil tiene un “tamaño” y “ángulo” ficticios (o cero)

proyectilAsRect :: Proyectil -> (Position, Size, Angle)
proyectilAsRect proj =
  -- Extrae la posición almacenada en el Proyectil (campo posicionProyectil)
  let pos = posicionProyectil proj
      -- Asumimos que el proyectil es un punto o un rectángulo muy pequeño:
      sz = (0.1, 0.1)  -- Mejor usar el tamaño que tienen los proyectiles
      ang = 0 -- Lo mismo para la dirección que lleva el proyectil
  in (pos, sz, ang)

{-
Eventos múltiples por proyectil
Ahora mismo un mismo proyectil puede chocar con varios carros y generará varios RobotHit.
Para que el proyectil desaparezca al primer impacto, debería:
  buscar el primer carro colisionado por cada proyectil (find) y emitir un único evento por proyectil; o
  ordenar por prioridad y filtrar duplicados.

Eliminar proyectiles después de la colisión

Suele ser útil que checkCollisions o el loop del mundo devuelvan también qué proyectiles eliminar 
(o qué cambios aplicar). Ahora solo produces eventos; 
la fase posterior debe consumir esos eventos y actualizar el Mundo 
(restar vida, borrar proyectil, etc.).
-}

-- detectRobotProjectileCollisions: compara cada carro con cada proyectil

detectRobotProjectileCollisions :: [CarroCombate] -> [Proyectil] -> [CollisionEvent]
detectRobotProjectileCollisions carros proyectiles =
  concatMap (\car -> mapMaybe (colisionCon car) proyectiles) carros
  where
    colisionCon car proj =
      let (posC, szC, angC) = (posicionCarro car, tamanoCarro car, direccionCarro car)
          (posP, szP, angP) = proyectilAsRect proj
      in if checkCollision posC szC angC posP szP angP
            then Just (RobotHit (carroId car) (proyectilId proj))
            else Nothing


-- detectRobotRobotCollisions: compara cada par de carros (sin repetir)

detectRobotRobotCollisions :: [CarroCombate] -> [CollisionEvent]
detectRobotRobotCollisions carros =
  concatMap (\(c1, rest) -> mapMaybe (colisionEntre c1) rest) (paresUnicos carros)
  where
    paresUnicos xs = zip xs (tail <$> tails xs)
    colisionEntre c1 c2 =
      let (p1, s1, a1) = (posicionCarro c1, tamanoCarro c1, direccionCarro c1)
          (p2, s2, a2) = (posicionCarro c2, tamanoCarro c2, direccionCarro c2)
      in if checkCollision p1 s1 a1 p2 s2 a2
            then Just (RobotRobot (carroId c1) (carroId c2))
            else Nothing

-- checkCollisions: combinación de ambas
checkCollisions :: [CarroCombate] -> [Proyectil] -> [CollisionEvent]
checkCollisions carros proyectiles =
    detectRobotProjectileCollisions carros proyectiles ++ detectRobotRobotCollisions carros
