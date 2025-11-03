module Bot where

import Data.List (minimumBy, sortBy, find)
import Data.Maybe (listToMaybe, fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Unidad
import Types (TipoCarro(..), Value(..))
import Physics (distanceBetween, normalize, rad2deg, addVec, subVec)

-- Acciones que puede tomar el bot
data BotAction = DispararA Int | Mover (Float, Float) | Girar Float
  deriving (Show, Eq)

-- ==========================================================
-- ESTRATEGIA PRINCIPAL: Selecciona según tipo de carro
-- ==========================================================

botEstrategico :: Mundo -> CarroCombate -> Maybe [BotAction]
botEstrategico mundo carro
  | energia carro <= 0 = Nothing
  | otherwise =
    let metsVisibles = obtenerMeteoritosVisibles mundo carro
    in
      -- Si hay meteoritos visibles, evadirlos PRIMERO
      if not (null metsVisibles)
      then
        case calcularDireccionEscape carro metsVisibles of
          Just (dirX, dirY) ->
            -- Mientras se elude, si hay enemigo a la vista, disparar
            case enemigoMasCercano carro (carros mundo) of
              Just enemigo ->
                let puedeVer = veEntre carro enemigo (carros mundo)
                in
                  if puedeVer
                  then
                    let apuntar = apuntarHacia carro enemigo
                    in Just [Mover (dirX, dirY), apuntar]
                  else
                    Just [Mover (dirX, dirY)]
              Nothing ->
                Just [Mover (dirX, dirY)]
          Nothing ->
            -- No se pudo calcular, aplicar estrategia normal
            case tipoCarro carro of
              Ligero -> Just (estrategiaFlankeo mundo carro)
              Cazacarros -> Just (estrategiaSniper mundo carro)
              Pesado -> Just (estrategiaTanque mundo carro)
      else
        -- Sin meteoritos, estrategia normal
        case tipoCarro carro of
          Ligero -> Just (estrategiaFlankeo mundo carro)
          Cazacarros -> Just (estrategiaSniper mundo carro)
          Pesado -> Just (estrategiaTanque mundo carro)

-- ==========================================================
-- ESTRATEGIA 1: FLANQUEO Y COBERTURA (Ligero)
-- ==========================================================

estrategiaFlankeo :: Mundo -> CarroCombate -> [BotAction]
estrategiaFlankeo mundo carro =
  let memoria = memoriaCarro carro
      
      -- Aliados para coordinación
      aliados = filter (\c -> team c == team carro && carroId c /= carroId carro) (carros mundo)
      
  in case enemigoMasCercano carro (carros mundo) of
    Just enemigo ->
      let puedeVer = veEntre carro enemigo (carros mundo)
          dist = distanceBetween (posicionCarro carro) (posicionCarro enemigo)
      in
        -- Comportamiento según salud
        if energia carro < 30
          then ejecutarRetirada mundo carro enemigo aliados  -- Retirada táctica
        else if puedeVer
          then ejecutarFlankeo mundo carro enemigo dist      -- Flanquear y atacar
          else ejecutarBusqueda mundo carro enemigo memoria  -- Buscar última posición conocida
    
    Nothing -> [Girar 5]  -- Explorar si no hay enemigos

-- Ejecutar maniobra de flanqueo
ejecutarFlankeo :: Mundo -> CarroCombate -> CarroCombate -> Float -> [BotAction]
ejecutarFlankeo _ carro enemigo dist =
  let (ex, ey) = posicionCarro enemigo
      (cx, cy) = posicionCarro carro
      
      -- Calcular posición de flanqueo (90 grados al costado del enemigo)
      angulo = atan2 (ey - cy) (ex - cx)
      anguloFlankeo = angulo + pi/2  -- 90 grados a la derecha
      
      distanciaOptima = 80.0  -- Distancia ideal para ligeros
      
      -- Posición de flanqueo
      flankX = ex + distanciaOptima * cos anguloFlankeo
      flankY = ey + distanciaOptima * sin anguloFlankeo
      
      -- Vector hacia posición de flanqueo
      (dx, dy) = normalize (flankX - cx, flankY - cy)
      
      apuntar = apuntarHacia carro enemigo
      
  in if dist < 60  -- Muy cerca, retroceder disparando
       then [apuntar, DispararA (carroId enemigo), Mover (-dx * 0.5, -dy * 0.5)]
     else if dist > 120  -- Muy lejos, acercarse
       then [Mover (dx, dy)]
     else  -- Distancia óptima, flanquear y disparar
       [Mover (dx * 0.7, dy * 0.7), apuntar, DispararA (carroId enemigo)]

-- Retirada táctica cuando está bajo de salud
ejecutarRetirada :: Mundo -> CarroCombate -> CarroCombate -> [CarroCombate] -> [BotAction]
ejecutarRetirada _ carro enemigo aliados =
  let (ex, ey) = posicionCarro enemigo
      (cx, cy) = posicionCarro carro
      
      -- Huir en dirección opuesta al enemigo
      (dx, dy) = normalize (cx - ex, cy - ey)
      
      -- Buscar aliado más cercano para reagruparse
      aliadoCercano = listToMaybe $ sortBy (comparing (distanceBetween (posicionCarro carro) . posicionCarro)) aliados
      
  in case aliadoCercano of
    Just aliado ->
      let (ax, ay) = posicionCarro aliado
          (dxa, dya) = normalize (ax - cx, ay - cy)
          -- Combinar huida con reagrupación
          (finalX, finalY) = normalize (dx * 0.7 + dxa * 0.3, dy * 0.7 + dya * 0.3)
      in [Mover (finalX, finalY)]
    Nothing ->
      [Mover (dx, dy)]

-- Buscar enemigo en última posición conocida
ejecutarBusqueda :: Mundo -> CarroCombate -> CarroCombate -> Map.Map String Value -> [BotAction]
ejecutarBusqueda _ carro enemigo _ =
  let posActual = posicionCarro enemigo
      
      -- Moverse hacia última posición conocida
      (ex, ey) = posActual
      (cx, cy) = posicionCarro carro
      (dx, dy) = normalize (ex - cx, ey - cy)
      
  in [Mover (dx, dy)]

-- ==========================================================
-- ESTRATEGIA 2: SNIPER (Cazacarros)
-- ==========================================================

estrategiaSniper :: Mundo -> CarroCombate -> [BotAction]
estrategiaSniper mundo carro =
  let distanciaOptima = 150.0  -- Distancia ideal para cazacarros
      memoria = memoriaCarro carro
      
  in case enemigoMasCercano carro (carros mundo) of
    Just enemigo ->
      let puedeVer = veEntre carro enemigo (carros mundo)
          dist = distanceBetween (posicionCarro carro) (posicionCarro enemigo)
          (ex, ey) = posicionCarro enemigo
          (cx, cy) = posicionCarro carro
          
          apuntar = apuntarHacia carro enemigo
          
      in if not puedeVer
           then ejecutarBusqueda mundo carro enemigo memoria
         else if dist < distanciaOptima - 30  -- Muy cerca, retroceder
           then
             let (dx, dy) = normalize (cx - ex, cy - ey)
             in [Mover (dx * 0.8, dy * 0.8), apuntar, DispararA (carroId enemigo)]
         else if dist > distanciaOptima + 30  -- Muy lejos, acercarse
           then
             let (dx, dy) = normalize (ex - cx, ey - cy)
             in [Mover (dx * 0.5, dy * 0.5), apuntar]
         else  -- Distancia perfecta, posicionarse y disparar
           let -- Movimiento lateral para evitar ser blanco fácil
               angulo = atan2 (ey - cy) (ex - cx)
               perpAngulo = angulo + pi/2
               (strafeX, strafeY) = (cos perpAngulo * 0.3, sin perpAngulo * 0.3)
           in [Mover (strafeX, strafeY), apuntar, DispararA (carroId enemigo)]
    
    Nothing -> [Girar 3]

-- ==========================================================
-- ESTRATEGIA 3: TANQUE PESADO (CORREGIDO)
-- ==========================================================

estrategiaTanque :: Mundo -> CarroCombate -> [BotAction]
estrategiaTanque mundo carro =
  let aliados = filter (\c -> team c == team carro && carroId c /= carroId carro) (carros mundo)
      
  in case enemigoMasCercano carro (carros mundo) of
    Just enemigo ->
      let puedeVer = veEntre carro enemigo (carros mundo)
          dist = distanceBetween (posicionCarro carro) (posicionCarro enemigo)
          (ex, ey) = posicionCarro enemigo
          (cx, cy) = posicionCarro carro
          
          apuntar = apuntarHacia carro enemigo
          
      in if not puedeVer
           then
             -- Avanzar hacia última posición conocida
             let (dx, dy) = normalize (ex - cx, ey - cy)
             in [Mover (dx * 0.6, dy * 0.6)]
         else
           -- Comportamiento según distancia - SIEMPRE dispara si puede ver
           if dist < 80  -- Distancia de combate cercano
             then
               -- Verificar si hay aliados cerca para coordinar
               let aliadosCerca = filter (\a -> distanceBetween (posicionCarro carro) (posicionCarro a) < 100) aliados
               in if length aliadosCerca >= 1
                    then [apuntar, DispararA (carroId enemigo)]  -- Con apoyo, mantener posición y disparar
                    else  -- Sin apoyo, avanzar lentamente pero SIEMPRE disparar
                      let (dx, dy) = normalize (ex - cx, ey - cy)
                      in [Mover (dx * 0.4, dy * 0.4), apuntar, DispararA (carroId enemigo)]
           else  -- Lejos, avanzar pero SIEMPRE disparar si puede ver
             let (dx, dy) = normalize (ex - cx, ey - cy)
             in [Mover (dx * 0.7, dy * 0.7), apuntar, DispararA (carroId enemigo)]
    
    Nothing -> [Girar 2]

-- ==========================================================
-- UTILIDADES
-- ==========================================================

enemigoMasCercano :: CarroCombate -> [CarroCombate] -> Maybe CarroCombate
enemigoMasCercano carro todos =
  let enemigos = filter (\e -> team e /= team carro && energia e > 0) todos
  in if null enemigos
       then Nothing
       else Just (minimumBy (comparing (distanceBetween (posicionCarro carro) . posicionCarro)) enemigos)

-- Apuntar el cañón hacia un objetivo
apuntarHacia :: CarroCombate -> CarroCombate -> BotAction
apuntarHacia carro objetivo =
  let (x1, y1) = posicionCarro carro
      (x2, y2) = posicionCarro objetivo
      angObjetivo = rad2deg (atan2 (y2 - y1) (x2 - x1))
      angActual = getdireccionCanon carro
      diferencia = normalizarAngulo (angObjetivo - angActual)
  in Girar (diferencia * 0.5)  -- Girar suavemente para precisión

-- Normalizar ángulo entre -180 y 180
normalizarAngulo :: Float -> Float
normalizarAngulo ang
  | ang > 180  = ang - 360
  | ang < -180 = ang + 360
  | otherwise  = ang

-- Calcular vector de movimiento hacia un objetivo
direccionHacia :: CarroCombate -> CarroCombate -> (Float, Float)
direccionHacia carro objetivo =
  let (x1, y1) = posicionCarro carro
      (x2, y2) = posicionCarro objetivo
      (dx, dy) = (x2 - x1, y2 - y1)
  in normalize (dx, dy)

-- Detectar meteoritos cercanos
obtenerMeteoritosVisibles :: Mundo -> CarroCombate -> [Meteorito]
obtenerMeteoritosVisibles m car =
  let (cx, cy) = posicionCarro car
      radioVision = 120.0
  in filter (\met ->
    let (mx, my) = posicionMeteorito met
        dx = mx - cx
        dy = my - cy
        dist = sqrt (dx*dx + dy*dy)
    in dist < radioVision
    ) (obstaculos m)

-- Calcular dirección de escape (alejarse de meteoritos)
calcularDireccionEscape :: CarroCombate -> [Meteorito] -> Maybe (Float, Float)
calcularDireccionEscape car mets
  | null mets = Nothing
  | otherwise =
    let (cx, cy) = posicionCarro car
        oposiciones = map (\met ->
          let (mx, my) = posicionMeteorito met
              (dx, dy) = normalize (cx - mx, cy - my)
          in (dx, dy)
          ) mets
        (totalX, totalY) = foldl (\(sx, sy) (x, y) -> (sx + x, sy + y)) (0, 0) oposiciones
        numMets = fromIntegral (length mets)
    in Just $ normalize (totalX / numMets, totalY / numMets)

obtenerObstaculosEstaticosVisibles :: Mundo -> CarroCombate -> [ObstaculoEstatico]
obtenerObstaculosEstaticosVisibles m car =
  filter (\obs -> distanceBetween (posicionCarro car) (posicionObstaculoEstatico obs) < 100)
         (obstaculosEstaticos m)
