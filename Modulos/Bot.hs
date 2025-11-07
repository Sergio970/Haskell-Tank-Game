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
          bombasMundo = bombas mundo
          enemigos = filter (\c -> team c /= team carro && energia c > 0) (carros mundo)
      in
        -- PRIORIDAD 1: Cazar meteoritos cerca de enemigos (oportunidad táctica)
        case detectarOportunidadMeteorito mundo carro enemigos metsVisibles of
          Just acciones -> Just acciones
        
        -- PRIORIDAD 2: Empujar enemigos hacia bombas (solo pesados)
          Nothing -> case tipoCarro carro of
            Pesado -> 
              case detectarOportunidadEmpuje mundo carro enemigos bombasMundo of
                Just acciones -> Just acciones
                Nothing -> aplicarEstrategiaNormal mundo carro metsVisibles
            _ -> aplicarEstrategiaNormal mundo carro metsVisibles

-- Aplicar estrategia normal según tipo
aplicarEstrategiaNormal :: Mundo -> CarroCombate -> [Meteorito] -> Maybe [BotAction]
aplicarEstrategiaNormal mundo carro metsVisibles =
  if not (null metsVisibles)
  then
    case calcularDireccionEscape carro metsVisibles of
      Just (dirX, dirY) ->
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
        case tipoCarro carro of
          Ligero -> Just (estrategiaFlankeo mundo carro)
          Cazacarros -> Just (estrategiaSniper mundo carro)
          Pesado -> Just (estrategiaTanque mundo carro)
  else
    case tipoCarro carro of
      Ligero -> Just (estrategiaFlankeo mundo carro)
      Cazacarros -> Just (estrategiaSniper mundo carro)
      Pesado -> Just (estrategiaTanque mundo carro)

-- ==========================================================
-- CAZAR METEORITOS CERCA DE ENEMIGOS
-- ==========================================================

detectarOportunidadMeteorito :: Mundo -> CarroCombate -> [CarroCombate] -> [Meteorito] -> Maybe [BotAction]
detectarOportunidadMeteorito mundo carro enemigos metsVisibles
  | null enemigos || null metsVisibles = Nothing
  | otherwise =
      let oportunidades = 
            [ (met, enemigo, dist) 
            | met <- metsVisibles
            , enemigo <- enemigos
            , let dist = distanceBetween (posicionMeteorito met) (posicionCarro enemigo)
            , dist < 80  -- Meteorito está peligrosamente cerca del enemigo
            , vida met <= 40  -- Meteorito es destruible con 2 disparos
            , distanceBetween (posicionCarro carro) (posicionMeteorito met) < 200  -- Estamos en rango
            ]
      in
        case oportunidades of
          [] -> Nothing
          ((mejorMet, enemigo, _):_) ->
            -- Oportunidad encontrada: disparar al meteorito
            let apuntarMet = apuntarHaciaMeteorito carro mejorMet
                (cx, cy) = posicionCarro carro
                (mx, my) = posicionMeteorito mejorMet
                -- Posicionarse mejor si es necesario
                dist = distanceBetween (cx, cy) (mx, my)
            in
              if dist > 150
              then
                -- Acercarse mientras apunta
                let (dx, dy) = normalize (mx - cx, my - cy)
                in Just [Mover (dx * 0.6, dy * 0.6), apuntarMet, DispararA (-meteoritoId mejorMet)]
              else
                -- Ya estamos en posición, disparar repetidamente
                Just [apuntarMet, DispararA (-meteoritoId mejorMet)]

-- Apuntar hacia un meteorito
apuntarHaciaMeteorito :: CarroCombate -> Meteorito -> BotAction
apuntarHaciaMeteorito carro met =
  let (x1, y1) = posicionCarro carro
      (x2, y2) = posicionMeteorito met
      angObjetivo = rad2deg (atan2 (y2 - y1) (x2 - x1))
      angActual = getdireccionCanon carro
      diferencia = normalizarAngulo (angObjetivo - angActual)
  in Girar (diferencia * 0.5)

-- ==========================================================
-- EMPUJAR ENEMIGOS HACIA BOMBAS
-- ==========================================================

detectarOportunidadEmpuje :: Mundo -> CarroCombate -> [CarroCombate] -> [Bomba] -> Maybe [BotAction]
detectarOportunidadEmpuje mundo carro enemigos todasBombas
  | null enemigos || null todasBombas = Nothing
  | tipoCarro carro /= Pesado = Nothing  -- Solo tanques pesados pueden empujar efectivamente
  | otherwise =
      let bombasInactivas = filter (not . activaBomba) todasBombas
          oportunidades = 
            [ (enemigo, bomba, distE, distC)
            | enemigo <- enemigos
            , bomba <- bombasInactivas
            , let distE = distanceBetween (posicionCarro enemigo) (posicionBomba bomba)
            , distE < 100  -- Enemigo cerca de una bomba
            , let distC = distanceBetween (posicionCarro carro) (posicionCarro enemigo)
            , distC < 150  -- Estamos en rango de empuje
            , energia enemigo < 80  -- Enemigo vulnerable
            ]
      in
        case sortBy (comparing (\(_, _, de, _) -> de)) oportunidades of
          [] -> Nothing
          ((enemigoObjetivo, bombaObjetivo, _, _):_) ->
            -- Calcular vector de empuje: carro → enemigo → bomba
            let (cx, cy) = posicionCarro carro
                (ex, ey) = posicionCarro enemigoObjetivo
                (bx, by) = posicionBomba bombaObjetivo
                
                -- Vector desde enemigo hacia bomba
                (dbx, dby) = normalize (bx - ex, by - ey)
                
                -- Vector desde carro hacia enemigo
                (dex, dey) = normalize (ex - cx, ey - cy)
                
                -- Calcular ángulo entre vectores
                dotProduct = dbx * dex + dby * dey
                
                -- Solo empujar si estamos bien alineados (dotProduct > 0.5)
                dist = distanceBetween (cx, cy) (ex, ey)
            in
              if dotProduct > 0.5 && dist > 30
              then
                -- Avanzar hacia el enemigo para empujarlo hacia la bomba
                Just [Mover (dex, dey)]
              else if dotProduct > 0.5 && dist <= 30
              then
                -- Ya estamos empujando, mantener presión
                Just [Mover (dex * 0.7, dey * 0.7)]
              else
                -- Reposicionarse para mejor ángulo de empuje
                let anguloBomba = atan2 (by - ey) (bx - ex)
                    -- Posición óptima: detrás del enemigo respecto a la bomba
                    distanciaOptima = 40.0
                    posOptX = ex - dbx * distanciaOptima
                    posOptY = ey - dby * distanciaOptima
                    (dx, dy) = normalize (posOptX - cx, posOptY - cy)
                in Just [Mover (dx, dy)]

-- ==========================================================
-- ESTRATEGIA 1: FLANQUEO Y COBERTURA (Ligero)
-- ==========================================================

estrategiaFlankeo :: Mundo -> CarroCombate -> [BotAction]
estrategiaFlankeo mundo carro =
  let memoria = memoriaCarro carro
      aliados = filter (\c -> team c == team carro && carroId c /= carroId carro) (carros mundo)
  in case enemigoMasCercano carro (carros mundo) of
    Just enemigo ->
      let puedeVer = veEntre carro enemigo (carros mundo)
          dist = distanceBetween (posicionCarro carro) (posicionCarro enemigo)
      in
        if energia carro < 30
        then ejecutarRetirada mundo carro enemigo aliados
        else if puedeVer
        then ejecutarFlankeo mundo carro enemigo dist
        else ejecutarBusqueda mundo carro enemigo memoria
    Nothing -> [Girar 5]

ejecutarFlankeo :: Mundo -> CarroCombate -> CarroCombate -> Float -> [BotAction]
ejecutarFlankeo _ carro enemigo dist =
  let (ex, ey) = posicionCarro enemigo
      (cx, cy) = posicionCarro carro
      angulo = atan2 (ey - cy) (ex - cx)
      anguloFlankeo = angulo + pi/2
      distanciaOptima = 80.0
      flankX = ex + distanciaOptima * cos anguloFlankeo
      flankY = ey + distanciaOptima * sin anguloFlankeo
      (dx, dy) = normalize (flankX - cx, flankY - cy)
      apuntar = apuntarHacia carro enemigo
  in if dist < 60
     then [apuntar, DispararA (carroId enemigo), Mover (-dx * 0.5, -dy * 0.5)]
     else if dist > 120
     then [Mover (dx, dy)]
     else [Mover (dx * 0.7, dy * 0.7), apuntar, DispararA (carroId enemigo)]

ejecutarRetirada :: Mundo -> CarroCombate -> CarroCombate -> [CarroCombate] -> [BotAction]
ejecutarRetirada _ carro enemigo aliados =
  let (ex, ey) = posicionCarro enemigo
      (cx, cy) = posicionCarro carro
      (dx, dy) = normalize (cx - ex, cy - ey)
      aliadoCercano = listToMaybe $ sortBy (comparing (distanceBetween (posicionCarro carro) . posicionCarro)) aliados
  in case aliadoCercano of
    Just aliado ->
      let (ax, ay) = posicionCarro aliado
          (dxa, dya) = normalize (ax - cx, ay - cy)
          (finalX, finalY) = normalize (dx * 0.7 + dxa * 0.3, dy * 0.7 + dya * 0.3)
      in [Mover (finalX, finalY)]
    Nothing ->
      [Mover (dx, dy)]

ejecutarBusqueda :: Mundo -> CarroCombate -> CarroCombate -> Map.Map String Value -> [BotAction]
ejecutarBusqueda _ carro enemigo _ =
  let posActual = posicionCarro enemigo
      (ex, ey) = posActual
      (cx, cy) = posicionCarro carro
      (dx, dy) = normalize (ex - cx, ey - cy)
  in [Mover (dx, dy)]

-- ==========================================================
-- ESTRATEGIA 2: SNIPER (Cazacarros)
-- ==========================================================

estrategiaSniper :: Mundo -> CarroCombate -> [BotAction]
estrategiaSniper mundo carro =
  let distanciaOptima = 150.0
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
         else if dist < distanciaOptima - 30
         then
           let (dx, dy) = normalize (cx - ex, cy - ey)
           in [Mover (dx * 0.8, dy * 0.8), apuntar, DispararA (carroId enemigo)]
         else if dist > distanciaOptima + 30
         then
           let (dx, dy) = normalize (ex - cx, ey - cy)
           in [Mover (dx * 0.5, dy * 0.5), apuntar]
         else
           let angulo = atan2 (ey - cy) (ex - cx)
               perpAngulo = angulo + pi/2
               (strafeX, strafeY) = (cos perpAngulo * 0.3, sin perpAngulo * 0.3)
           in [Mover (strafeX, strafeY), apuntar, DispararA (carroId enemigo)]
    Nothing -> [Girar 3]

-- ==========================================================
-- ESTRATEGIA 3: TANQUE PESADO
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
           let (dx, dy) = normalize (ex - cx, ey - cy)
           in [Mover (dx * 0.6, dy * 0.6)]
         else
           if dist < 80
           then
             let aliadosCerca = filter (\a -> distanceBetween (posicionCarro carro) (posicionCarro a) < 100) aliados
             in if length aliadosCerca >= 1
                then [apuntar, DispararA (carroId enemigo)]
                else
                  let (dx, dy) = normalize (ex - cx, ey - cy)
                  in [Mover (dx * 0.4, dy * 0.4), apuntar, DispararA (carroId enemigo)]
           else
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

apuntarHacia :: CarroCombate -> CarroCombate -> BotAction
apuntarHacia carro objetivo =
  let (x1, y1) = posicionCarro carro
      (x2, y2) = posicionCarro objetivo
      angObjetivo = rad2deg (atan2 (y2 - y1) (x2 - x1))
      angActual = getdireccionCanon carro
      diferencia = normalizarAngulo (angObjetivo - angActual)
  in Girar (diferencia * 0.5)

normalizarAngulo :: Float -> Float
normalizarAngulo ang
  | ang > 180 = ang - 360
  | ang < -180 = ang + 360
  | otherwise = ang

direccionHacia :: CarroCombate -> CarroCombate -> (Float, Float)
direccionHacia carro objetivo =
  let (x1, y1) = posicionCarro carro
      (x2, y2) = posicionCarro objetivo
      (dx, dy) = (x2 - x1, y2 - y1)
  in normalize (dx, dy)

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