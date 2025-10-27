module Main where

import qualified Data.Map.Strict as Map
import System.Random (randomRIO)
import Data.Maybe (mapMaybe)
import Control.Monad (foldM, replicateM)
import Data.List (partition, nub)

import Objeto (Objeto(..))
import Types (TipoCarro(..), MunicionTipo(..), Vector, Size, Position, Angle, Distance, Value(..))
import Unidad
import Bot (botEstrategico, BotAction(..))
import Physics (updatePosition, vectorNulo, normalize)
import Collisions (CollisionEvent(..), checkCollisions)
import Rendering
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss (Display(..), Color(..))
import GameTypes

--------------------------------
-- Creación de carros
--------------------------------

carroLigero :: Int -> Int -> (Float, Float) -> CarroCombate
carroLigero cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto { posicion  = pos, direccion = 0, direccionCanon = 0, velocidad = (10,0), tamano = (4,3)
           , atributos = CarroAtributos
               { carroIdE = cid, equipoE = equipo, tipoCarroE = Ligero, energiaE = 100
               , blindajeE = blindajeBase Ligero
               , alcanceVisionE = visionBase Ligero
               , alcanceRadioE = radioBase Ligero
               , tripulacionE = tripulacionViva
               , municionesE = replicate 12 (Municion AP 1.0 Map.empty)
               , cadenciaE = 1.0, precisionBaseE = 0.9, memoriaCarroE = memoriaLigero } }

carroPesado :: Int -> Int -> (Float, Float) -> CarroCombate
carroPesado cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto { posicion  = pos, direccion = 0, direccionCanon = 0, velocidad = (4,0), tamano = (7,5)
           , atributos = CarroAtributos
               { carroIdE = cid, equipoE = equipo, tipoCarroE = Pesado, energiaE = 200
               , blindajeE = blindajeBase Pesado
               , alcanceVisionE = visionBase Pesado
               , alcanceRadioE = radioBase Pesado
               , tripulacionE = tripulacionViva
               , municionesE = replicate 10 (Municion AP 1.0 Map.empty)
               , cadenciaE = 2.0, precisionBaseE = 0.8, memoriaCarroE = memoriaPesado } }

cazacarros :: Int -> Int -> (Float, Float) -> CarroCombate
cazacarros cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto { posicion  = pos, direccion = 0, direccionCanon = 0, velocidad = (7,0), tamano = (6,4)
           , atributos = CarroAtributos
               { carroIdE = cid, equipoE = equipo, tipoCarroE = Cazacarros, energiaE = 150
               , blindajeE = blindajeBase Cazacarros
               , alcanceVisionE = visionBase Cazacarros
               , alcanceRadioE = radioBase Cazacarros
               , tripulacionE = tripulacionViva
               , municionesE = replicate 10 (Municion AP 1.0 Map.empty)
               , cadenciaE = 1.5, precisionBaseE = 0.85, memoriaCarroE = memoriaCazacarros } }

--------------------------------
-- Mundo inicial
--------------------------------

posicionPorEquipo :: Int -> Size -> IO Position
posicionPorEquipo equipo (tamX, tamY) = do
  let mitadX = tamX / 2
      margen = tamX / 6
      (minX, maxX) = if equipo == 1
                     then (-mitadX, -margen)
                     else (margen, mitadX)
  x <- randomRIO (minX, maxX)
  y <- randomRIO (-tamY/2, tamY/2)
  pure (x, y)

carroAleatorioEquipo :: Int -> Int -> Size -> IO CarroCombate
carroAleatorioEquipo cid equipo tam = do
  tipo <- randomRIO (0 :: Int, 2)
  pos <- posicionPorEquipo equipo tam
  pure $ case tipo of
    0 -> carroLigero cid equipo pos
    1 -> carroPesado cid equipo pos
    _ -> cazacarros cid equipo pos

mundoAleatorio :: IO Mundo
mundoAleatorio = do
  let numPorEquipo = 4
  tamX <- randomRIO (800, 1000)
  tamY <- randomRIO (600, 800)
  carrosEq1 <- mapM (\cid -> carroAleatorioEquipo cid 1 (tamX, tamY)) [1..numPorEquipo]
  carrosEq2 <- mapM (\cid -> carroAleatorioEquipo (cid + 100) 2 (tamX, tamY)) [1..numPorEquipo]
  let todos = carrosEq1 ++ carrosEq2
  pure Mundo { carros = todos, proyectiles = [], tamanoMundo = (tamX, tamY), memoria = memoriaMundo }

--------------------------------
-- Sistema de combate
--------------------------------

dispararSimple :: Int -> CarroCombate -> CarroCombate -> Proyectil
dispararSimple pid atacante objetivo =
  let (x1, y1) = posicionCarro atacante
      (x2, y2) = posicionCarro objetivo
      (dx, dy) = normalize (x2 - x1, y2 - y1)
      speed    = 400
  in Proyectil
       { proyectilId        = pid
       , posicionProyectil  = (x1, y1)
       , direccionProyectil = 0
       , velocidadProyectil = (dx * speed, dy * speed)
       , municionProyectil  = Municion AP 1.0 Map.empty
       , disparadorTeam     = team atacante
       , memoriaProj        = Map.fromList [("ttl", VFloat 3.0)]
       }

aplicarAccionesBot :: Float -> Mundo -> Float -> (CarroCombate, [BotAction]) -> IO Mundo
aplicarAccionesBot dt m tiempoActual (carro, acts) = foldM aplicar m acts
  where
    aplicar mundo (DispararA objetivoId) =
      case buscarCarro objetivoId (carros mundo) of
        Just obj -> do
          let memoriaCar = memoriaCarro carro
              ultimoDisparo = case Map.lookup "ultimoDisparo" memoriaCar of
                Just (VFloat t) -> t
                _               -> 0.0
              cooldown = cadencia carro
              
          if tiempoActual - ultimoDisparo >= cooldown && not (null (municiones carro))
            then do
              let pid = length (proyectiles mundo) + 1
                  proj = dispararSimple pid carro obj
                  memoriaActualizada = Map.insert "ultimoDisparo" (VFloat tiempoActual) memoriaCar
                  carroActualizado = setMemoriaCarro memoriaActualizada carro
              pure $ mundo { proyectiles = proj : proyectiles mundo
                           , carros = reemplazarCarroEnLista carroActualizado (carros mundo) }
            else pure mundo
        Nothing  -> pure mundo
        
    aplicar mundo (Mover (dx, dy)) =
      let speed   = case tipoCarro carro of
                      Ligero     -> 25
                      Cazacarros -> 18
                      Pesado     -> 12
          nuevaVel = (dx * speed, dy * speed)
          carro'   = carro { velocidad = nuevaVel }
      in pure $ reemplazarCarro carro' (carros mundo) mundo
      
    aplicar mundo (Girar ang) =
      let angActual = getdireccionCanon carro
          maxGiro = 5.0
          angFinal = max (-maxGiro) (min maxGiro ang)
          c' = carro { direccionCanon = angActual + angFinal }
      in pure $ reemplazarCarro c' (carros mundo) mundo

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

buscarCarro :: Int -> [CarroCombate] -> Maybe CarroCombate
buscarCarro cid = safeHead . filter ((== cid) . carroId)

buscarProyectil :: Int -> [Proyectil] -> Maybe Proyectil
buscarProyectil pid = safeHead . filter ((== pid) . proyectilId)

reemplazarCarro :: CarroCombate -> [CarroCombate] -> Mundo -> Mundo
reemplazarCarro c cs m = m { carros = c : filter ((/= carroId c) . carroId) cs }

reemplazarCarroEnLista :: CarroCombate -> [CarroCombate] -> [CarroCombate]
reemplazarCarroEnLista c cs = c : filter ((/= carroId c) . carroId) cs

limpiarProyectiles :: Float -> Mundo -> Mundo
limpiarProyectiles dt m =
  let (tamX, tamY) = tamanoMundo m
      dentro (x, y) = x > -tamX/2 && x < tamX/2 && y > -tamY/2 && y < tamY/2
      stepTTL p =
        case Map.lookup "ttl" (memoriaProj p) of
          Just (VFloat t) -> p { memoriaProj = Map.insert "ttl" (VFloat (t - dt)) (memoriaProj p) }
          _               -> p
      vivosTTL p =
        case Map.lookup "ttl" (memoriaProj p) of
          Just (VFloat t) -> t > 0
          _               -> True
      ps1 = map stepTTL (proyectiles m)
      ps2 = filter (\p -> dentro (posicionProyectil p) && vivosTTL p) ps1
  in m { proyectiles = ps2 }

aplicarEventosColision :: [CollisionEvent] -> Mundo -> GameState -> IO (Mundo, [Explosion])
aplicarEventosColision eventos m gs = foldM procesar (m, []) eventos
  where
    procesar (mundo, exps) (RobotHit cid pid) =
      case (buscarCarro cid (carros mundo), buscarProyectil pid (proyectiles mundo)) of
        (Just car, Just proj) ->
          if disparadorTeam proj == team car
            then pure (mundo, exps)
            else
              let municion = municionProyectil proj
                  blindajeCarro = blindaje car
                  danoBase = case tipoMun municion of
                               AP -> 20
                               AE -> 15
                  factorBlindaje = max 0.3 (1.0 - blindajeCarro / 300.0)
                  danoFinal = round (fromIntegral danoBase * factorBlindaje)
                  
                  carDanado = setEnergia (max 0 (energia car - danoFinal)) car
                  mundoSinProj = mundo { proyectiles = filter ((/= pid) . proyectilId) (proyectiles mundo) }
                  
                  -- Crear explosión de impacto
                  explosion = Explosion 
                    { explosionPos = posicionProyectil proj
                    , explosionTime = 0.5
                    , explosionType = ImpactExplosion
                    }
              in pure (reemplazarCarro carDanado (carros mundoSinProj) mundoSinProj, explosion : exps)
        _ -> pure (mundo, exps)
    
    procesar (mundo, exps) (RobotRobot cid1 cid2) =
      case (buscarCarro cid1 (carros mundo), buscarCarro cid2 (carros mundo)) of
        (Just c1, Just c2) -> do
          let equipo1 = team c1
              equipo2 = team c2
              (x1, y1) = posicionCarro c1
              (x2, y2) = posicionCarro c2
              (vx1, vy1) = velocidadCarro c1
              (vx2, vy2) = velocidadCarro c2
              (dx, dy) = normalize (x1 - x2, y1 - y2)
              pushForce = 12.0
              
              (c1', c2') = if equipo1 == equipo2
                          then ( c1 { velocidad = (vx1 + dx * pushForce, vy1 + dy * pushForce) }
                               , c2 { velocidad = (vx2 - dx * pushForce, vy2 - dy * pushForce) } )
                          else let dano = 10
                                   c1Danado = setEnergia (max 0 (energia c1 - dano)) c1
                                   c2Danado = setEnergia (max 0 (energia c2 - dano)) c2
                               in ( c1Danado { velocidad = (vx1 + dx * pushForce, vy1 + dy * pushForce) }
                                  , c2Danado { velocidad = (vx2 - dx * pushForce, vy2 - dy * pushForce) } )
          
          pure (reemplazarCarro c2' (carros mundo) (reemplazarCarro c1' (carros mundo) mundo), exps)
        _ -> pure (mundo, exps)
    
    procesar (mundo, exps) (FronteraCarro cid) =
      case buscarCarro cid (carros mundo) of
        Just c -> do
          let (x, y) = posicionCarro c
              (vx, vy) = velocidadCarro c
              (tamX, tamY) = tamanoMundo mundo
              margin = 15
              x' = max (-tamX/2 + margin) (min (tamX/2 - margin) x)
              y' = max (-tamY/2 + margin) (min (tamY/2 - margin) y)
              vx' = if x <= (-tamX/2 + margin) || x >= (tamX/2 - margin) then -vx * 0.7 else vx
              vy' = if y <= (-tamY/2 + margin) || y >= (tamY/2 - margin) then -vy * 0.7 else vy
              c' = c { posicion = (x', y'), velocidad = (vx', vy') }
          pure (reemplazarCarro c' (carros mundo) mundo, exps)
        Nothing -> pure (mundo, exps)
    
    procesar (mundo, exps) (FronteraProyectil pid) =
      pure (mundo { proyectiles = filter ((/= pid) . proyectilId) (proyectiles mundo) }, exps)

updateGame :: Float -> GameState -> IO GameState
updateGame dt gs =
  case modo gs of
    Menu -> pure gs
    Victoria _ -> pure gs
    Jugando -> do
      let m0 = mundo gs
          tiempoActual = tiempo gs
          vivos = filter (\c -> energia c > 0) (carros m0)
          muertosAntes = filter (\c -> energia c <= 0) (carros m0)

      m1 <- foldM (\mw c ->
                     case botEstrategico mw c of
                       Just as -> aplicarAccionesBot dt mw tiempoActual (c, as)
                       Nothing -> pure mw
                  ) m0 vivos

      let cs' = map (\c -> c { posicion = updatePosition dt (posicion c) (velocidad c) }) (carros m1)
          ps' = map (\p -> p { posicionProyectil = updatePosition dt (posicionProyectil p) (velocidadProyectil p) }) (proyectiles m1)
          m2  = m1 { carros = cs', proyectiles = ps' }
          m3  = limpiarProyectiles dt m2
          eventos = checkCollisions m3
      
      (m4, nuevasExplosiones) <- aplicarEventosColision eventos m3 gs
      
      let muertosDespues = filter (\c -> energia c <= 0) (carros m4)
          nuevasMuertes = filter (\c -> all ((/= carroId c) . carroId) muertosAntes) muertosDespues
          explosionesDeathActual = [ Explosion 
                                       { explosionPos = posicionCarro c
                                       , explosionTime = 1.0
                                       , explosionType = DeathExplosion
                                       }
                                   | c <- nuevasMuertes
                                   ]
          
          m5 = m4 { carros = filter ((> 0) . energia) (carros m4) }
          
          -- Actualizar explosiones existentes
          explosionesActualizadas = [ e { explosionTime = explosionTime e - dt }
                                    | e <- explosions gs
                                    , explosionTime e - dt > 0
                                    ]
          
          todasExplosiones = explosionesActualizadas ++ nuevasExplosiones ++ explosionesDeathActual

          equiposVivos = nub (map team (carros m5))

      if length equiposVivos == 1
        then
          let ganador = head equiposVivos
          in pure gs { mundo = m5, explosions = todasExplosiones, modo = Victoria ganador }
        else
          pure gs { mundo = m5, tiempo = tiempoActual + dt, explosions = todasExplosiones }

--------------------------------
-- MAIN
--------------------------------

main :: IO ()
main = do
  putStrLn "Iniciando Haskell Tank Game..."
  m0 <- mundoAleatorio
  let initial = GameState
                  { mundo = m0
                  , tiempo = 0
                  , ronda = 0
                  , modo = Menu
                  , explosions = []
                  , bgIndex = 1          -- fondo por defecto
                  }
  playIO window backgroundColor fps initial renderGame GameTypes.handleEvent updateGame
