module Main where

import qualified Data.Map.Strict as Map
import System.Random (randomRIO)
import Data.Maybe (mapMaybe)
import Control.Monad (foldM, replicateM)

import Objeto (Objeto(..))
import Types (TipoCarro(..), MunicionTipo(..), Vector, Size, Position)
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
  , blindaje
  , alcanceVision
  , alcanceRadio
  , tripulacion
  , municiones
  , cadencia
  , precisionBase
  , memoriaCarro
  , setPosicion
  , setEnergia
  , setMemoriaCarro
  , setTripulacion
  , aplicarEfectosTripulacion
  , aplicarDanioConMuerteAleatoria
  , aplicarImpactoDirecto
  , calcularDanio
  , dispararA
  , agregarProyectil
  , removerProyectil
  , blindajeBase
  , visionBase
  , radioBase
  , memoriaLigero
  , memoriaPesado
  , memoriaCazacarros
  , memoriaMunicionAP
  , memoriaMunicionAE
  , memoriaMundo
  , tripulacionViva
  , tickSeconds
  )



import Bot (botEstrategico, BotAction(..))
import Collisions (CollisionEvent(..), checkCollisions)
import Physics (updatePosition, vectorNulo, normalize)
import Rendering
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss (Display(..), Color(..))
import GameTypes


-- ============================
--   Creación de carros
-- ============================

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
               , municionesE = replicate 3 (Municion AP 75.0 Map.empty)
                                ++ replicate 2 (Municion AE 120.0 Map.empty)
               , cadenciaE = 1.0, precisionBaseE = 0.9, memoriaCarroE = memoriaLigero } }

carroPesado :: Int -> Int -> (Float, Float) -> CarroCombate
carroPesado cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto { posicion  = pos, direccion = 0, direccionCanon = 0, velocidad = (4,0), tamano = (7,5)
           , atributos = CarroAtributos
               { carroIdE = cid, equipoE = equipo, tipoCarroE = Pesado, energiaE = 300
               , blindajeE = blindajeBase Pesado
               , alcanceVisionE = visionBase Pesado
               , alcanceRadioE = radioBase Pesado
               , tripulacionE = tripulacionViva
               , municionesE = replicate 4 (Municion AP 120.0 Map.empty)
                                ++ replicate 3 (Municion AE 150.0 Map.empty)
               , cadenciaE = 2.0, precisionBaseE = 0.8, memoriaCarroE = memoriaPesado } }

cazacarros :: Int -> Int -> (Float, Float) -> CarroCombate
cazacarros cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto { posicion  = pos, direccion = 0, direccionCanon = 0, velocidad = (7,0), tamano = (6,4)
           , atributos = CarroAtributos
               { carroIdE = cid, equipoE = equipo, tipoCarroE = Cazacarros, energiaE = 180
               , blindajeE = blindajeBase Cazacarros
               , alcanceVisionE = visionBase Cazacarros
               , alcanceRadioE = radioBase Cazacarros
               , tripulacionE = tripulacionViva
               , municionesE = replicate 3 (Municion AP 110.0 Map.empty)
                                ++ replicate 2 (Municion AE 140.0 Map.empty)
               , cadenciaE = 1.5, precisionBaseE = 0.85, memoriaCarroE = memoriaCazacarros } }


-- ============================
--   Mundo aleatorio
-- ============================

numCarros :: Int
numCarros = 6

carroAleatorio :: Int -> IO CarroCombate
carroAleatorio cid = do
  tipo <- randomRIO (0 :: Int, 2)
  equipo <- randomRIO (1 :: Int, 2)
  pos <- posicionAleatoria (400, 300)
  pure $ case tipo of
    0 -> carroLigero cid equipo pos
    1 -> carroPesado cid equipo pos
    _ -> cazacarros cid equipo pos

posicionAleatoria :: Size -> IO Position
posicionAleatoria (w, h) = do
  x <- randomRIO (-w/2, w/2)
  y <- randomRIO (-h/2, h/2)
  pure (x, y)

mundoAleatorio :: IO Mundo
mundoAleatorio = do
  let numPorEquipo = 4
  tamX <- randomRIO (400, 800)
  tamY <- randomRIO (300, 600)

  -- Generar tanques del equipo 1
  carrosEq1 <- mapM (\cid -> carroAleatorioEquipo cid 1 (tamX, tamY)) [1..numPorEquipo]
  -- Generar tanques del equipo 2
  carrosEq2 <- mapM (\cid -> carroAleatorioEquipo (cid + 100) 2 (tamX, tamY)) [1..numPorEquipo]

  let todos = carrosEq1 ++ carrosEq2
  pure Mundo { carros = todos, proyectiles = [], tamanoMundo = (tamX, tamY), memoria = memoriaMundo }


-- Genera una posición aleatoria dentro de una zona específica según el equipo
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

-- Genera un carro aleatorio dentro de su zona de equipo
carroAleatorioEquipo :: Int -> Int -> Size -> IO CarroCombate
carroAleatorioEquipo cid equipo tam = do
  tipo <- randomRIO (0 :: Int, 2)
  pos <- posicionPorEquipo equipo tam
  pure $ case tipo of
    0 -> carroLigero cid equipo pos
    1 -> carroPesado cid equipo pos
    _ -> cazacarros cid equipo pos




-- ============================
--   Lógica de simulación
-- ============================

-- ============================
--   Bucle de torneo con bots y colisiones
-- ============================

bucleTorneo :: Float -> Mundo -> IO Mundo
bucleTorneo dt m0 = do
  -- Decisiones de bots
  let decisiones = mapMaybe (\c -> fmap (\acts -> (c, acts)) (botEstrategico m0 c)) (carros m0)

  -- Aplicar acciones de bots (disparar, mover, girar)
  m1 <- foldM (aplicarAccionesBot dt) m0 decisiones

  -- Actualizar física básica
  let m2 = actualizarTanques dt m1
      m3 = actualizarProyectiles dt m2
      m4 = limpiarProyectilesFuera m3
      eventos = checkCollisions m4

  -- Aplicar colisiones y daños
  m4 <- aplicarEventos eventos m3

  -- Reactivar tanques inactivos
  let m5 = reactivarTanques m4

  pure m5


updateGame :: Float -> GameState -> IO GameState
updateGame dt gs = do
  let m = mundo gs
      carrosVivos = filter (\c -> energia c > 0) (carros m)
  
  -- Ejecutar el bot para cada tanque
  mConAcciones <- foldM (\mundoActual carro -> do
                            case botEstrategico mundoActual carro of
                              Just acciones -> aplicarAccionesBot dt mundoActual (carro, acciones)
                              Nothing       -> pure mundoActual
                        ) m carrosVivos

  -- Avanzar física básica del mundo
  mAvanzado <- bucleTorneo dt mConAcciones
  pure gs { mundo = mAvanzado, tiempo = tiempo gs + dt }

-- ============================
--   Acciones y físicas
-- ============================

aplicarAccionesBot :: Float -> Mundo -> (CarroCombate, [BotAction]) -> IO Mundo
aplicarAccionesBot dt m (carro, acts) = foldM aplicar m acts
  where
    aplicar mundo (DispararA objetivoId) =
      case buscarCarro objetivoId (carros mundo) of
        Just obj ->
          let pid              = length (proyectiles mundo) + 1
              (x1, y1)         = posicionCarro carro
              (x2, y2)         = posicionCarro obj
              dir              = normalize (x2 - x1, y2 - y1)
              proj             = Proyectil
                { proyectilId        = pid
                , posicionProyectil  = (x1, y1)
                , velocidadProyectil = (fst dir * 300, snd dir * 300)
                , municionProyectil  = Municion AP 75.0 Map.empty
                , direccionProyectil = atan2 (snd dir) (fst dir)
                , disparadorTeam     = team carro
                , memoriaProj        = Map.empty
                }
              mundoConProj     = agregarProyectil proj mundo
          in do
            putStrLn $ "Proyectil creado en " ++ show (posicionProyectil proj)
            pure mundoConProj
        Nothing -> pure mundo

    aplicar mundo (Mover (dx, dy)) =
      let speed    = 20
          nuevaVel = (dx * speed, dy * speed)
          carro'   = carro { velocidad = nuevaVel }
      in pure $ reemplazarCarro carro' (carros mundo) mundo

    aplicar mundo (Girar ang) =
      let c' = carro { direccionCanon = getdireccionCanon carro + ang }
      in pure $ reemplazarCarro c' (carros mundo) mundo

    aplicar mundo _ = pure mundo











reactivarTanques :: Mundo -> Mundo
reactivarTanques m =
  let moverSiQuieto c =
        if energia c > 0 && vectorNulo (velocidad c)
          then c { direccion = direccion c + 10 }
          else c
  in m { carros = map moverSiQuieto (carros m) }

actualizarTanques :: Float -> Mundo -> Mundo
actualizarTanques dt m =
  let cs = carros m
      cs' = map (\c -> c { posicion = updatePosition dt (posicion c) (velocidad c) }) cs
  in m { carros = cs' }

actualizarProyectiles :: Float -> Mundo -> Mundo
actualizarProyectiles dt m =
  let ps = proyectiles m
      ps' = map (\p -> p { posicionProyectil = updatePosition dt (posicionProyectil p) (velocidadProyectil p) }) ps
  in m { proyectiles = ps' }

-- No elimina proyectiles tan rápido (permite que salgan un poco del mapa antes de borrarlos)
limpiarProyectilesFuera :: Mundo -> Mundo
limpiarProyectilesFuera m =
  let (tamX, tamY) = tamanoMundo m
      margenExtra = 200  -- <- deja que salgan un poco antes de eliminarse
      dentro (x, y) =
        x > (-tamX/2 - margenExtra)
        && x < (tamX/2 + margenExtra)
        && y > (-tamY/2 - margenExtra)
        && y < (tamY/2 + margenExtra)
      psFiltrados = filter (dentro . posicionProyectil) (proyectiles m)
  in m { proyectiles = psFiltrados }


aplicarEventos :: [CollisionEvent] -> Mundo -> IO Mundo
aplicarEventos eventos m = foldM procesar m eventos
  where
    procesar mundo (RobotHit cid pid) =
      case (buscarCarro cid (carros mundo), buscarProyectil pid (proyectiles mundo)) of
        (Just car, Just proj) ->
          let mun = municionProyectil proj
          in case aplicarImpactoDirecto mun car of
               Just carDanado ->
                 let danoReal = energia car - energia carDanado
                 in do carFinal <- aplicarDanioConMuerteAleatoria danoReal carDanado
                       pure $ removerProyectil pid $ reemplazarCarro carFinal (carros mundo) mundo
               Nothing -> pure $ removerProyectil pid mundo
        _ -> pure mundo
    procesar mundo _ = pure mundo


-- ============================
--   Helpers
-- ============================

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

buscarCarro :: Int -> [CarroCombate] -> Maybe CarroCombate
buscarCarro cid = safeHead . filter ((== cid) . carroId)

buscarProyectil :: Int -> [Proyectil] -> Maybe Proyectil
buscarProyectil pid = safeHead . filter ((== pid) . proyectilId)

reemplazarCarro :: CarroCombate -> [CarroCombate] -> Mundo -> Mundo
reemplazarCarro c cs m = m { carros = c : filter ((/= carroId c) . carroId) cs }


-- ============================
--   MAIN
-- ============================

main :: IO ()
main = do
  putStrLn "Iniciando Haskell Tank Game (modo gráfico)..."
  m0 <- mundoAleatorio
  let initial = GameState { mundo = m0, tiempo = 0, ronda = 0 }
  playIO window backgroundColor fps initial renderGame handleEvent updateGame
