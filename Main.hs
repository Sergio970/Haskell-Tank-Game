module Main where

import qualified Data.Map.Strict as Map
import System.Random (randomRIO)
import Data.Maybe (mapMaybe)
import Control.Monad (foldM, replicateM)
import Data.List (partition)

import Objeto (Objeto(..))
import Types (TipoCarro(..), MunicionTipo(..), Vector, Size, Position, Angle, Distance, Value(..))
import Unidad
  ( CarroCombate(..)
  , CarroAtributos(..)
  , Mundo(..)
  , Tripulacion(..)
  , EstadoTripulante(..)
  , Proyectil(..)
  , Municion(..)
  , posicionCarro
  , direccionCarro
  , getdireccionCanon
  , velocidadCarro
  , tamanoCarro
  , carroId
  , team
  , tipoCarro
  , energia
  , tripulacion
  , setEnergia
  , setTripulacion
  , aplicarEfectosTripulacion
  , blindajeBase
  , visionBase
  , radioBase
  , memoriaLigero
  , memoriaPesado
  , memoriaCazacarros
  , memoriaMundo
  , tripulacionViva
  )
import Bot (botEstrategico, BotAction(..))
import Physics (updatePosition, vectorNulo, normalize)
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
               , municionesE = replicate 8 (Municion AP 1.0 Map.empty) -- no usamos el calibre ya
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
               , municionesE = replicate 8 (Municion AP 1.0 Map.empty)
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
               , municionesE = replicate 8 (Municion AP 1.0 Map.empty)
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
-- Bucle / lógica simple y estable
--------------------------------

-- Disparo simple: crea un proyectil rápido que vive 2.5 s máx
dispararSimple :: Int -> CarroCombate -> CarroCombate -> Proyectil
dispararSimple pid atacante objetivo =
  let (x1, y1) = posicionCarro atacante
      (x2, y2) = posicionCarro objetivo
      (dx, dy) = normalize (x2 - x1, y2 - y1)
      speed    = 320
  in Proyectil
       { proyectilId        = pid
       , posicionProyectil  = (x1, y1)
       , direccionProyectil = 0         -- no lo usamos para mover
       , velocidadProyectil = (dx * speed, dy * speed)
       , municionProyectil  = Municion AP 1.0 Map.empty
       , disparadorTeam     = team atacante
       , memoriaProj        = Map.fromList [("ttl", VFloat 2.5)]  -- segundos de vida
       }

-- Aplicar acciones del bot (solo 3 acciones)
aplicarAccionesBot :: Float -> Mundo -> (CarroCombate, [BotAction]) -> IO Mundo
aplicarAccionesBot _ m (carro, acts) = foldM aplicar m acts
  where
    aplicar mundo (DispararA objetivoId) =
      case buscarCarro objetivoId (carros mundo) of
        Just obj ->
          let pid = length (proyectiles mundo) + 1
              proj = dispararSimple pid carro obj
          in pure $ mundo { proyectiles = proj : proyectiles mundo }
        Nothing  -> pure mundo
    aplicar mundo (Mover (dx, dy)) =
      let speed   = 20
          nuevaVel = (dx * speed, dy * speed)
          carro'   = carro { velocidad = nuevaVel }
      in pure $ reemplazarCarro carro' (carros mundo) mundo
    aplicar mundo (Girar ang) =
      let c' = carro { direccionCanon = getdireccionCanon carro + ang }
      in pure $ reemplazarCarro c' (carros mundo) mundo

-- Helpers
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

buscarCarro :: Int -> [CarroCombate] -> Maybe CarroCombate
buscarCarro cid = safeHead . filter ((== cid) . carroId)

reemplazarCarro :: CarroCombate -> [CarroCombate] -> Mundo -> Mundo
reemplazarCarro c cs m = m { carros = c : filter ((/= carroId c) . carroId) cs }

-- Filtro de proyectiles fuera de los límites o con TTL agotado
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

-- Daño fijo al impactar y eliminación de proyectil
aplicarColisionesSimples :: Mundo -> Mundo
aplicarColisionesSimples m =
  let ps = proyectiles m
      cs = carros m

      -- un hit si el proyectil pasa cerca (radio 8) del centro del carro
      hit p c =
        let (px,py) = posicionProyectil p
            (cx,cy) = posicionCarro c
            dx = px - cx; dy = py - cy
        in dx*dx + dy*dy <= (8*8)

      -- procesamos impactos y construimos nuevas listas
      (impactados, sinImpacto) = partition
        (\p -> any (\c -> team c /= disparadorTeam p && energia c > 0 && hit p c) cs)
        ps

      -- aplicar daño fijo 25
      aplicarDmg p c =
        if team c == disparadorTeam p || not (hit p c) then c
        else let nuevaE = max 0 (energia c - 25)
             in setEnergia nuevaE c

      cs' = foldl (\acc p -> map (aplicarDmg p) acc) cs impactados
  in m { carros = filter ((>0) . energia) cs', proyectiles = sinImpacto }

updateGame :: Float -> GameState -> IO GameState
updateGame dt gs =
  case modo gs of
    Menu -> pure gs  -- el menú solo se pinta; el input se maneja en handleEvent
    Jugando -> do
      let m0 = mundo gs
          vivos = filter (\c -> energia c > 0) (carros m0)

      -- decisiones bot + aplicación
      m1 <- foldM (\mw c ->
                     case botEstrategico mw c of
                       Just as -> aplicarAccionesBot dt mw (c, as)
                       Nothing -> pure mw
                  ) m0 vivos

      -- avanzar física básica
      let cs' = map (\c -> c { posicion = updatePosition dt (posicion c) (velocidad c) }) (carros m1)
          ps' = map (\p -> p { posicionProyectil = updatePosition dt (posicionProyectil p) (velocidadProyectil p) }) (proyectiles m1)
          m2  = m1 { carros = cs', proyectiles = ps' }
          m3  = limpiarProyectiles dt m2
          m4  = aplicarColisionesSimples m3

      pure gs { mundo = m4, tiempo = tiempo gs + dt }

--------------------------------
-- MAIN (con menú)
--------------------------------

main :: IO ()
main = do
  putStrLn "Iniciando Haskell Tank Game..."
  m0 <- mundoAleatorio
  -- Arranca en menú
  let initial = GameState { mundo = m0, tiempo = 0, ronda = 0, modo = Menu }
  playIO window backgroundColor fps initial renderGame GameTypes.handleEvent updateGame

