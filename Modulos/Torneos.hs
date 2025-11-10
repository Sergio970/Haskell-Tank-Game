module Torneos where

import System.Random (randomRIO)
import Control.Monad (foldM, replicateM, replicateM_)
import Data.List (sortBy, nub, partition)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Graphics.Gloss.Interface.IO.Game (playIO, Event(..), Key(..), SpecialKey(..), KeyState(..))
import Graphics.Gloss (Display(..), Color(..), makeColor)
import Text.Read (readMaybe)
import Data.Char (toLower, isSpace)
import Data.List (dropWhileEnd)
import Control.Exception (try, IOException)

import Types (TipoCarro(..), MunicionTipo(..), Vector, Size, Position, Value(..))
import Unidad hiding (buscarObstaculoEstatico)
import Objeto (Objeto(..))
import Bot (botEstrategico, BotAction(..))
import Physics (updatePosition, vectorNulo, normalize, distanceBetween)
import Collisions (CollisionEvent(..), checkCollisions)
import GameTypes
import Rendering
import qualified Estadisticas as E

-- ==========================================================
-- TIPO ALIAS
-- ==========================================================

type GameStateMundo = GameState Mundo E.EstadisticasBot E.EstadisticasTorneo

-- ==========================================================
-- CREACIÓN DE CARROS
-- ==========================================================

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

crearCarroPorTipo :: Int -> Int -> (Float, Float) -> TipoCarro -> CarroCombate
crearCarroPorTipo cid equipo pos tipo = case tipo of
  Ligero     -> carroLigero cid equipo pos
  Pesado     -> carroPesado cid equipo pos
  Cazacarros -> cazacarros cid equipo pos

-- ==========================================================
-- CREACIÓN DE NUEVO TORNEO
-- ==========================================================

crearNuevoTorneo :: IO GameStateMundo
crearNuevoTorneo = do
  mundo <- mundoDesdeConfig

  let statsIniciales = Map.fromList
        [ (carroId c, E.inicializarEstadisticasBot
                        (carroId c)
                        (team c)
                        (show $ tipoCarro c))
        | c <- carros mundo
        ]

  pure GameState
    { mundo = mundo
    , tiempo = 0.0
    , ronda = 1
    , modo = Jugando
    , explosions = []
    , bgIndex = 1
    , proximoMeteoritoId = 100
    , tiempoProxMeteorito = 2.0
    , actualTorneo = 1
    , torneosSobrantes = 0
    , tiempoEsperaVictoria = 3.0
    , estadisticasBots = statsIniciales
    , historialTorneos = []
    }

-- ==========================================================
-- GENERACIÓN DE MUNDO
-- ==========================================================

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

  bombas <- mapM (\i -> generarBomba i (tamX, tamY)) [1..6]
  obstaculosEst <- generarObstaculosEspaciados 8 (tamX, tamY)

  pure Mundo
    { carros = todos
    , proyectiles = []
    , obstaculos = []
    , bombas = bombas
    , obstaculosEstaticos = obstaculosEst
    , tamanoMundo = (tamX, tamY)
    , memoria = memoriaMundo
    }

generarBomba :: Int -> Size -> IO Bomba
generarBomba bid (tamX, tamY) = do
  x <- randomRIO (-tamX/2 + 20, tamX/2 - 20)
  y <- randomRIO (-tamY/2 + 20, tamY/2 - 20)
  radio <- randomRIO (6.0, 12.0)
  pure $ Bomba { bombaId = bid, posicionBomba = (x,y), radioBomba = radio, activaBomba = False, tiempoBomba = 3.0 }

generarMeteorito :: Int -> Size -> IO Meteorito
generarMeteorito mid (tamX, tamY) = do
  lado <- randomRIO (0 :: Int, 1)
  let (posX, dirX) = if lado == 0
                    then (-tamX/2 + 20, 150)
                    else (tamX/2 - 20, -150)
  posY <- randomRIO (-tamY/2, tamY/2)
  velY <- randomRIO (-20, 20)
  tamano <- randomRIO (15, 40)
  rotInicial <- randomRIO (0, 360)
  velRot <- randomRIO (-5, 5)
  let vidaMet = round (tamano * 5)
  pure Meteorito
    { meteoritoId = mid
    , posicionMeteorito = (posX, posY)
    , velocidadMeteorito = (dirX, velY)
    , tamanoMeteorito = tamano
    , rotacionMeteorito = rotInicial
    , velocidadRotacion = velRot
    , vida = vidaMet
    , estelas = []
    }

generarMeteoritos :: Int -> Size -> IO [Meteorito]
generarMeteoritos n tam = mapM (\i -> generarMeteorito i tam) [1..n]

generarObstaculoEstatico :: Int -> Size -> IO ObstaculoEstatico
generarObstaculoEstatico oid (tamX, tamY) = do
  x <- randomRIO (-tamX/2 + 60, tamX/2 - 60)
  y <- randomRIO (-tamY/2 + 60, tamY/2 - 60)
  radio <- randomRIO (20,30)
  tipo <- randomRIO (1, 5)
  pure ObstaculoEstatico
    { obstaculoEstaticoId = oid
    , posicionObstaculoEstatico = (x, y)
    , tamanoObstaculoEstatico = radio
    , tipoVisual = tipo
    }

generarObstaculosEspaciados :: Int -> Size -> IO [ObstaculoEstatico]
generarObstaculosEspaciados n tam = generarConDistancia [] n 0
  where
    distanciaMinima = 100.0
    maxIntentos = 50

    generarConDistancia acc 0 _ = pure acc
    generarConDistancia acc restantes intentos
      | intentos > maxIntentos =
          generarConDistancia acc (restantes - 1) 0
      | otherwise = do
          candidato <- generarObstaculoEstatico (n - restantes + 1) tam
          let (cx, cy) = posicionObstaculoEstatico candidato
              muyCerca obs =
                let (ox, oy) = posicionObstaculoEstatico obs
                    dx = ox - cx
                    dy = oy - cy
                    dist = sqrt (dx*dx + dy*dy)
                    radioTotal = tamanoObstaculoEstatico candidato + tamanoObstaculoEstatico obs
                in dist < (distanciaMinima + radioTotal)

          if null acc || not (any muyCerca acc)
            then generarConDistancia (candidato:acc) (restantes - 1) 0
            else generarConDistancia acc restantes (intentos + 1)

-- ==========================================================
-- SISTEMA DE COMBATE
-- ==========================================================

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
      , memoriaProj        = Map.fromList [("ttl", VFloat 3.0), ("disparadorId", VInt (carroId atacante))]
      }

--  Ahora registra disparos
aplicarAccionesBot
  :: Float
  -> Mundo
  -> Float
  -> (CarroCombate, [BotAction])
  -> Map.Map Int E.EstadisticasBot
  -> IO (Mundo, Map.Map Int E.EstadisticasBot)
aplicarAccionesBot dt m tiempoActual (carro, acts) stats0 =
  foldM aplicar (m, stats0) acts
  where
    aplicar (mundo, stats) (DispararA objetivoId) =
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

                  -- ✅ Registrar disparo sobre el mapa de stats que ya viene actualizado
                  statsActualizados = E.registrarDisparo (carroId carro) stats

              pure ( mundo { proyectiles = proj : proyectiles mundo
                           , carros = reemplazarCarroEnLista carroActualizado (carros mundo) }
                   , statsActualizados )
            else pure (mundo, stats)
        Nothing  -> pure (mundo, stats)

    aplicar (mundo, stats) (Mover (dx, dy)) =
      let speed   = case tipoCarro carro of
                      Ligero     -> 25
                      Cazacarros -> 18
                      Pesado     -> 12
          nuevaVel = (dx * speed, dy * speed)
          carro'   = carro { velocidad = nuevaVel }
      in pure (reemplazarCarro carro' (carros mundo) mundo, stats)

    aplicar (mundo, stats) (Girar ang) =
      let angActual = getdireccionCanon carro
          maxGiro = 5.0
          angFinal = max (-maxGiro) (min maxGiro ang)
          c' = carro { direccionCanon = angActual + angFinal }
      in pure (reemplazarCarro c' (carros mundo) mundo, stats)

-- ==========================================================
-- HELPERS
-- ==========================================================

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

buscarCarro :: Int -> [CarroCombate] -> Maybe CarroCombate
buscarCarro cid = safeHead . filter ((== cid) . carroId)

buscarProyectil :: Int -> [Proyectil] -> Maybe Proyectil
buscarProyectil pid = safeHead . filter ((== pid) . proyectilId)

buscarMeteorito :: Int -> [Meteorito] -> Maybe Meteorito
buscarMeteorito mid = safeHead . filter ((== mid) . meteoritoId)

buscarBomba :: Int -> [Bomba] -> Maybe Bomba
buscarBomba bid = safeHead . filter ((== bid) . bombaId)

buscarObstaculoEstatico :: Int -> [ObstaculoEstatico] -> Maybe ObstaculoEstatico
buscarObstaculoEstatico oid = safeHead . filter ((== oid) . obstaculoEstaticoId)

reemplazarBomba :: Bomba -> [Bomba] -> Mundo -> Mundo
reemplazarBomba b bs m = m { bombas = b : filter ((/= bombaId b) . bombaId) bs }

reemplazarCarro :: CarroCombate -> [CarroCombate] -> Mundo -> Mundo
reemplazarCarro c cs m = m { carros = c : filter ((/= carroId c) . carroId) cs }

reemplazarCarroEnLista :: CarroCombate -> [CarroCombate] -> [CarroCombate]
reemplazarCarroEnLista c cs = c : filter ((/= carroId c) . carroId) cs

reemplazarMeteorito :: Meteorito -> [Meteorito] -> Mundo -> Mundo
reemplazarMeteorito met mets m =
  m { obstaculos = met : filter ((/= meteoritoId met) . meteoritoId) mets }

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

-- ==========================================================
-- ACTUALIZAR METEORITOS
-- ==========================================================

actualizarMeteoritos :: Float -> Mundo -> Mundo
actualizarMeteoritos dt m =
  let mets = obstaculos m
      (tamX, tamY) = tamanoMundo m
      mets' = map (actualizarMeteorito dt (tamX, tamY)) mets
      metsConEstelas = map (\met ->
        let nuevasEstelas = generarEstelasDesdeMeteorito met
        in met { estelas = nuevasEstelas ++ estelas met }
        ) mets'
      metsVivos = filter (\met -> vida met > 0) metsConEstelas
  in m { obstaculos = metsVivos }

actualizarMeteorito :: Float -> Size -> Meteorito -> Meteorito
actualizarMeteorito dt (tamX, tamY) met =
  let (x, y) = posicionMeteorito met
      (vx, vy) = velocidadMeteorito met
      x' = x + vx * dt
      y' = y + vy * dt
      y'' = max (-tamY/2) (min (tamY/2) y')
      estelas' = map (\e -> e { estelaVida = estelaVida e - dt }) (estelas met)
      estelasVivas = filter (\e -> estelaVida e > 0) estelas'
  in met
      { posicionMeteorito = (x', y'')
      , velocidadMeteorito = (vx, vy)
      , rotacionMeteorito = rotacionMeteorito met + velocidadRotacion met
      , estelas = estelasVivas
      }

generarEstelasDesdeMeteorito :: Meteorito -> [Estela]
generarEstelasDesdeMeteorito met =
  let (x, y) = posicionMeteorito met
  in [Estela
      { estelaId = 100000 + meteoritoId met
      , estelaPos = (x, y)
      , estelaVida = 0.5
      , estelaRadio = tamanoMeteorito met * 0.7
      , estelaIntensidad = 0.3
      }
    ]

actualizarMeteoritosEnPartida :: Float -> GameStateMundo -> IO GameStateMundo
actualizarMeteoritosEnPartida dt gs = do
  let tiempoProx = tiempoProxMeteorito gs - dt
      proximoId = proximoMeteoritoId gs
      m = mundo gs
      (tamX, tamY) = tamanoMundo m

  if tiempoProx <= 0
    then do
      nuevoMet <- generarMeteorito proximoId (tamX, tamY)
      intervalo <- randomRIO (1.0, 4.0)
      let mundoActualizado = m { obstaculos = nuevoMet : obstaculos m }
      pure gs
        { mundo = mundoActualizado
        , proximoMeteoritoId = proximoId + 1
        , tiempoProxMeteorito = intervalo
        }
    else
      pure gs { tiempoProxMeteorito = tiempoProx }

-- ==========================================================
-- COLISIONES CON ESTADÍSTICAS
-- ==========================================================

aplicarEventosColisionConStats :: [CollisionEvent] -> Mundo -> GameStateMundo -> Map.Map Int E.EstadisticasBot -> IO (Mundo, [Explosion], Map.Map Int E.EstadisticasBot)
aplicarEventosColisionConStats eventos m gs statsInicial = foldM procesar (m, [], statsInicial) eventos
  where
    procesar (mundo, exps, stats) (RobotHit cid pid) =
      case (buscarCarro cid (carros mundo), buscarProyectil pid (proyectiles mundo)) of
        (Just car, Just proj) ->
          if disparadorTeam proj == team car
            then pure (mundo, exps, stats)
            else do
              let municion = municionProyectil proj
                  blindajeCarro = blindaje car
                  danoBase = case tipoMun municion of
                              AP -> 20
                              AE -> 15
                  factorBlindaje = max 0.3 (1.0 - blindajeCarro / 300.0)
                  danoFinal = round (fromIntegral danoBase * factorBlindaje)

                  carDanado = setEnergia (max 0 (energia car - danoFinal)) car
                  mundoSinProj = mundo { proyectiles = filter ((/= pid) . proyectilId) (proyectiles mundo) }

                  explosion = Explosion
                    { explosionPos = posicionProyectil proj
                    , explosionTime = 0.5
                    , explosionType = ImpactExplosion
                    }

                  disparadorId = case Map.lookup "disparadorId" (memoriaProj proj) of
                    Just (VInt did) -> Just did
                    _ -> Nothing

                  statsActualizados = case disparadorId of
                    Just did -> E.registrarImpacto did danoFinal stats
                    Nothing  -> stats

              pure (reemplazarCarro carDanado (carros mundoSinProj) mundoSinProj, explosion : exps, statsActualizados)
        _ -> pure (mundo, exps, stats)

    procesar (mundo, exps, stats) (RobotObstaculoEstatico cid oid) =
      case (buscarCarro cid (carros mundo), buscarObstaculoEstatico oid (obstaculosEstaticos mundo)) of
        (Just car, Just obs) -> do
          let (cx, cy) = posicionCarro car
              (ox, oy) = posicionObstaculoEstatico obs
              (dx, dy) = normalize (cx - ox, cy - oy)
              pushForce = 30.0
              (vx, vy) = velocidadCarro car
              car' = car { velocidad = (vx + dx * pushForce, vy + dy * pushForce) }
          pure (reemplazarCarro car' (carros mundo) mundo, exps, stats)
        _ -> pure (mundo, exps, stats)

    procesar (mundo, exps, stats) (RobotRobot cid1 cid2) =
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

              (c1', c2') =
                if equipo1 == equipo2
                  then
                    ( c1 { velocidad = (vx1 + dx * pushForce, vy1 + dy * pushForce) }
                    , c2 { velocidad = (vx2 - dx * pushForce, vy2 - dy * pushForce) }
                    )
                  else
                    let dano     = 10
                        c1Danado = setEnergia (max 0 (energia c1 - dano)) c1
                        c2Danado = setEnergia (max 0 (energia c2 - dano)) c2
                    in
                    ( c1Danado { velocidad = (vx1 + dx * pushForce, vy1 + dy * pushForce) }
                    , c2Danado { velocidad = (vx2 - dx * pushForce, vy2 - dy * pushForce) }
                    )

          pure (reemplazarCarro c2' (carros mundo) (reemplazarCarro c1' (carros mundo) mundo), exps, stats)
        _ -> pure (mundo, exps, stats)

    procesar (mundo, exps, stats) (FronteraCarro cid) =
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
          pure (reemplazarCarro c' (carros mundo) mundo, exps, stats)
        Nothing -> pure (mundo, exps, stats)

    procesar (mundo, exps, stats) (FronteraProyectil pid) =
      pure (mundo { proyectiles = filter ((/= pid) . proyectilId) (proyectiles mundo) }, exps, stats)

    procesar (mundo, exps, stats) (RobotMeteorito cid mid) =
      case (buscarCarro cid (carros mundo), buscarMeteorito mid (obstaculos mundo)) of
        (Just car, Just met) -> do
          let dano = round (tamanoMeteorito met / 2)
              carDanado = setEnergia (max 0 (energia car - dano)) car
              (x1, y1) = posicionCarro car
              (x2, y2) = posicionMeteorito met
              (dx, dy) = normalize (x1 - x2, y1 - y2)
              pushForce = 20.0
              (vx, vy) = velocidadCarro car
              carEmpujado = carDanado { velocidad = (vx + dx * pushForce, vy + dy * pushForce) }
              explosion = Explosion (posicionCarro car) 0.3 ImpactExplosion
          pure (reemplazarCarro carEmpujado (carros mundo) mundo, explosion : exps, stats)
        _ -> pure (mundo, exps, stats)

    procesar (mundo, exps, stats) (ProyectilMeteorito pid mid) =
      case (buscarProyectil pid (proyectiles mundo), buscarMeteorito mid (obstaculos mundo)) of
        (Just proj, Just met) -> do
          let dano = 20
              metDanado = met { vida = vida met - dano }
              mundoSinProj = mundo { proyectiles = filter ((/= pid) . proyectilId) (proyectiles mundo) }
              mundoActualizado = if vida metDanado <= 0
                                then mundoSinProj { obstaculos = filter ((/= mid) . meteoritoId) (obstaculos mundoSinProj) }
                                else reemplazarMeteorito metDanado (obstaculos mundoSinProj) mundoSinProj
              explosion = Explosion (posicionProyectil proj) 0.4 ImpactExplosion
              explosionExtra = if vida metDanado <= 0
                              then [Explosion (posicionMeteorito met) 1.0 DeathExplosion]
                              else []
          pure (mundoActualizado, explosion : (explosionExtra ++ exps), stats)
        _ -> pure (mundo, exps, stats)

    procesar (mundo, exps, stats) (RobotEstela cid eid) =
      case buscarCarro cid (carros mundo) of
        Just car -> do
          let todasEstelas = concat [estelas met | met <- obstaculos mundo]
              estelaOpt = safeHead (filter ((== eid) . estelaId) todasEstelas)
          case estelaOpt of
            Just est -> do
              let dano = max 1 (round (2.0 * estelaIntensidad est))
                  carDanado = setEnergia (max 0 (energia car - dano)) car
              pure (reemplazarCarro carDanado (carros mundo) mundo, exps, stats)
            Nothing -> pure (mundo, exps, stats)
        _ -> pure (mundo, exps, stats)

    procesar (mundo, exps, stats) (RobotBomba _cid bid) =
      case buscarBomba bid (bombas mundo) of
        Just b ->
          if activaBomba b
            then pure (mundo, exps, stats)
            else
              let b' = b { activaBomba = True, tiempoBomba = 3.0 }
              in pure (reemplazarBomba b' (bombas mundo) mundo, exps, stats)
        Nothing -> pure (mundo, exps, stats)

    procesar (mundo, exps, stats) (MeteoritoBomba _mid bid) =
      case buscarBomba bid (bombas mundo) of
        Just b ->
          if activaBomba b
            then pure (mundo, exps, stats)
            else
              let b' = b { activaBomba = True, tiempoBomba = 3.0 }
              in pure (reemplazarBomba b' (bombas mundo) mundo, exps, stats)
        Nothing -> pure (mundo, exps, stats)

-- ==========================================================
-- ACTUALIZAR BOMBAS
-- ==========================================================

actualizarBombasEnMundo :: Float -> Mundo -> (Mundo, [Explosion])
actualizarBombasEnMundo dt m =
  let step b = if activaBomba b
            then b { tiempoBomba = tiempoBomba b - dt }
            else b
      bs1 = map step (bombas m)
      (detonan, vivas) = partition (\b -> activaBomba b && tiempoBomba b <= 0) bs1

      aplicarDanioBomba :: Bomba -> [CarroCombate] -> [CarroCombate]
      aplicarDanioBomba b cs =
        let (bx, by) = posicionBomba b
            radioImpacto = radioBomba b * 10.0
            maxDano = 80
        in [ if distanceBetween (posicionCarro c) (bx, by) <= radioImpacto
            then
              let dist = distanceBetween (posicionCarro c) (bx, by)
                  factor = max 0 (1.0 - dist / radioImpacto)
                  dano = round (fromIntegral maxDano * factor)
              in setEnergia (max 0 (energia c - dano)) c
            else c
           | c <- cs ]

      carrosDanados = foldr aplicarDanioBomba (carros m) detonan

      exps = [ Explosion
                { explosionPos = posicionBomba b
                , explosionTime = 0.8
                , explosionType = DeathExplosion
                }
            | b <- detonan ]
  in ( m { bombas = vivas, carros = carrosDanados }, exps )

-- ==========================================================
-- UPDATE GAME - FUNCIÓN PRINCIPAL
-- ==========================================================

updateGame :: Float -> GameStateMundo -> IO GameStateMundo
updateGame dt gs =
  case modo gs of
    Menu -> pure gs

    Victoria ganador -> do
      let tiempoRestante = tiempoEsperaVictoria gs - dt

      if tiempoRestante <= 0
      then do
        if torneosSobrantes gs > 0
        then do
          putStrLn $ "----------------------------------------"
          putStrLn $ "Iniciando Torneo " ++ show (actualTorneo gs + 1) ++ "..."
          putStrLn $ "----------------------------------------"
          nuevoMundo <- mundoDesdeConfig

          let statsIniciales = Map.fromList
                [ (carroId c, E.inicializarEstadisticasBot
                                (carroId c)
                                (team c)
                                (show $ tipoCarro c))
                | c <- carros nuevoMundo
                ]

          pure gs
            { mundo = nuevoMundo
            , tiempo = 0.0
            , ronda = ronda gs + 1
            , modo = Jugando
            , explosions = []
            , proximoMeteoritoId = 100
            , tiempoProxMeteorito = 2.0
            , actualTorneo = actualTorneo gs + 1
            , torneosSobrantes = torneosSobrantes gs - 1
            , tiempoEsperaVictoria = 3.0
            , estadisticasBots = statsIniciales
            }
        else do
          putStrLn ""
          putStrLn "=========================================="
          putStrLn "¡TODOS LOS TORNEOS COMPLETADOS!"
          putStrLn "=========================================="
          E.guardarEstadisticasAgregadas (historialTorneos gs)
          putStrLn "Estadísticas guardadas en estadisticas.txt"
          pure gs { modo = FinTorneos }
      else
        pure gs { tiempoEsperaVictoria = tiempoRestante }

    Jugando -> do
      gsConMeteoritos <- actualizarMeteoritosEnPartida dt gs
      let m0 = mundo gsConMeteoritos
          tiempoActual = tiempo gsConMeteoritos
          vivos = filter (\c -> energia c > 0) (carros m0)
          muertosAntes = filter (\c -> energia c <= 0) (carros m0)

      --Aplicar acciones de bots con tracking de estadísticas
      (m1, statsConDisparos) <- foldM
        (\(mw, st) c ->
            case botEstrategico mw c of
              Just as -> aplicarAccionesBot dt mw tiempoActual (c, as) st
              Nothing -> pure (mw, st)
        )
        (m0, estadisticasBots gsConMeteoritos)
        vivos

      let (tamX, tamY) = tamanoMundo m1
          margin = 20  -- margen para que no toquen el borde

          limitarPos (x,y) =
            let x' = max (-tamX/2 + margin) (min (tamX/2 - margin) x)
                y' = max (-tamY/2 + margin) (min (tamY/2 - margin) y)
            in (x', y')

          cs' = map (\c ->
                       let posNueva = updatePosition dt (posicion c) (velocidad c)
                        in c { posicion = limitarPos posNueva }
                    ) (carros m1)

          ps' = map (\p ->
                       let posNueva = updatePosition dt (posicionProyectil p) (velocidadProyectil p)
                        in p { posicionProyectil = posNueva }
                    ) (proyectiles m1)

          m2 = m1 { carros = cs', proyectiles = ps' }

          m3  = limpiarProyectiles dt m2
          m4  = actualizarMeteoritos dt m3
          eventos = checkCollisions m4

      --Actualizar estadísticas de tiempo vivo
      let estadosVivos = [(carroId c, energia c > 0) | c <- carros m4]
          statsConTiempo = E.registrarTiempoVivo dt estadosVivos statsConDisparos


      --Aplicar colisiones con tracking de impactos
      (m5, nuevasExplosiones, statsConImpactos) <- aplicarEventosColisionConStats eventos m4 gs statsConTiempo

      let (m5b, explosionesBombas) = actualizarBombasEnMundo dt m5
          muertosDespues = filter (\c -> energia c <= 0) (carros m4)
          nuevasMuertes = filter (\c -> all ((/= carroId c) . carroId) muertosAntes) muertosDespues

          statsConMuertes =
            foldr (\c stats -> E.registrarMuerte (carroId c) stats)
                  statsConImpactos nuevasMuertes

          explosionesDeathActual =
            [ Explosion
                { explosionPos = posicionCarro c
                , explosionTime = 1.0
                , explosionType = DeathExplosion
                }
            | c <- nuevasMuertes
            ]

          m6 = m5b { carros = filter ((> 0) . energia) (carros m5b) }

          explosionesActualizadas =
            [ e { explosionTime = explosionTime e - dt }
            | e <- explosions gs
            , explosionTime e - dt > 0
            ]

          todasExplosiones = explosionesActualizadas
                          ++ nuevasExplosiones
                          ++ explosionesBombas
                          ++ explosionesDeathActual

          equiposVivos = nub (map team (carros m5b))

      limite <- tiempoLimiteDesdeConfig
      let tiempoSiguiente = tiempoActual + dt

      if tiempoSiguiente >= limite
        then finalizarTorneoConStats gs m6 tiempoSiguiente statsConMuertes todasExplosiones 0
        else if length equiposVivos <= 1
          then do
            let ganador = if null equiposVivos then 0 else head equiposVivos
            finalizarTorneoConStats gs m6 tiempoSiguiente statsConMuertes todasExplosiones ganador
          else
            pure gs
              { mundo = m6
              , tiempo = tiempoSiguiente
              , explosions = todasExplosiones
              , estadisticasBots = statsConMuertes
              }

    FinTorneos -> pure gs

-- ==========================================================
-- FINALIZAR TORNEO CON ESTADÍSTICAS
-- ==========================================================

finalizarTorneoConStats :: GameStateMundo -> Mundo -> Float -> Map.Map Int E.EstadisticasBot -> [Explosion] -> Int -> IO GameStateMundo
finalizarTorneoConStats gs m6 tiempoSiguiente stats exps ganador = do
  mundoInicial <- mundoDesdeConfig

  let vivosFinal   = carros m6
      estadisticas = contarPorEquipo vivosFinal
      iniciales    = contarPorEquipo (carros mundoInicial)

      totalDisp = sum [ E.disparosRealizados s | s <- Map.elems stats ]
      totalImp  = sum [ E.impactosAcertados s | s <- Map.elems stats ]

      torneoStats = E.EstadisticasTorneo
        { E.numeroTorneo    = actualTorneo gs
        , E.duracionTorneo  = tiempoSiguiente
        , E.equipoGanador   = ganador
        , E.tanquesIniciales = iniciales
        , E.tanquesFinales   = estadisticas
        , E.estadisticasBots = Map.elems stats
        , E.totalDisparos    = totalDisp
        , E.totalImpactos    = totalImp
        }

  E.guardarEstadisticasTorneo torneoStats

  putStrLn $ " Torneo " ++ show (actualTorneo gs)
            ++ " completado - Ganador: Equipo "
            ++ show ganador

  pure gs
    { mundo = m6
    , tiempo = tiempoSiguiente
    , explosions = exps
    , modo = Victoria ganador
    , tiempoEsperaVictoria = 3.0
    , historialTorneos = torneoStats : historialTorneos gs
    }

contarPorEquipo :: [CarroCombate] -> [(Int, Int)]
contarPorEquipo carros =
  let equipos = nub $ map team carros
      contar eq = (eq, length $ filter (\c -> team c == eq) carros)
  in map contar equipos

-- ==========================================================
-- REINICIAR JUEGO
-- ==========================================================

handleEventWithReset :: Event -> GameStateMundo -> IO GameStateMundo
handleEventWithReset (EventKey (Char 'r') Down _ _) gs =
  case modo gs of
    Menu -> pure gs
    _ -> do
      putStrLn $ "Reiniciando partida... (Ronda " ++ show (ronda gs + 1) ++ ")"
      reiniciarJuego gs

handleEventWithReset event gs = GameTypes.handleEvent event gs

reiniciarJuego :: GameStateMundo -> IO GameStateMundo
reiniciarJuego gs = do
  m0 <- mundoDesdeConfig

  let statsIniciales = Map.fromList
        [ (carroId c, E.inicializarEstadisticasBot
                        (carroId c)
                        (team c)
                        (show $ tipoCarro c))
        | c <- carros m0
        ]

  pure GameState
    { mundo = m0
    , tiempo = 0
    , ronda = ronda gs + 1
    , modo = Jugando
    , explosions = []
    , bgIndex = bgIndex gs
    , proximoMeteoritoId = 1000
    , tiempoProxMeteorito = 2.0
    , actualTorneo = 1
    , torneosSobrantes = 0
    , tiempoEsperaVictoria = 3.0
    , estadisticasBots = statsIniciales
    , historialTorneos = []
    }

-- ==========================================================
-- CONFIGURACIÓN DESDE ARCHIVO
-- ==========================================================

mundoDesdeConfig :: IO Mundo
mundoDesdeConfig = do
  let rutas = ["config.txt", "Modulos/config.txt", "./config.txt"]
  intentarLeer rutas
  where
    intentarLeer [] = do
      putStrLn "No se pudo abrir config.txt. Usando mundoAleatorio."
      mundoAleatorio

    intentarLeer (path:resto) = do
      eres <- try (readFile path) :: IO (Either IOException String)
      case eres of
        Left _ -> intentarLeer resto
        Right contenido -> do
          putStrLn $ "Config cargado desde: " ++ path
          case parseConfigDetallado contenido of
            Left errs -> do
              putStrLn "Errores en config:"
              mapM_ (\e -> putStrLn ("  - " ++ e)) errs
              putStrLn "Usando mundoAleatorio."
              mundoAleatorio
            Right cfg -> construirMundoDesdeCfg cfg

rondasDesdeConfig :: IO Int
rondasDesdeConfig = do
  let path = "config.txt"
  eres <- try (readFile path) :: IO (Either IOException String)
  case eres of
    Left _ -> pure 1
    Right contenido ->
      case parseConfigDetallado contenido of
        Right cfg -> pure (max 1 (cfgRounds cfg))
        Left _    -> pure 1

tiempoLimiteDesdeConfig :: IO Float
tiempoLimiteDesdeConfig = do
  let path = "config.txt"
  eres <- try (readFile path) :: IO (Either IOException String)
  case eres of
    Left _ -> pure 120.0
    Right contenido ->
      case parseConfigDetallado contenido of
        Right cfg -> pure (max 1.0 (cfgTimeLimit cfg))
        Left _    -> pure 120.0

-- ==========================================================
-- PARSEO DE CONFIGURACIÓN
-- ==========================================================

data ConfigInterna = ConfigInterna
  { cfgTamX      :: Float
  , cfgTamY      :: Float
  , cfgEquipo1   :: [TipoCarro]
  , cfgEquipo2   :: [TipoCarro]
  , cfgBombs     :: Int
  , cfgObstacles :: Int
  , cfgRounds    :: Int
  , cfgTimeLimit :: Float
  } deriving (Show)

construirMundoDesdeCfg :: ConfigInterna -> IO Mundo
construirMundoDesdeCfg cfg = do
  let tamX = cfgTamX cfg
      tamY = cfgTamY cfg
      tipos1 = cfgEquipo1 cfg
      tipos2 = cfgEquipo2 cfg

  carrosEq1 <- mapM
    (\(i,t) -> do pos <- posicionPorEquipo 1 (tamX, tamY)
                  pure $ crearCarroPorTipo i 1 pos t)
    (zip [1..] tipos1)

  carrosEq2 <- mapM
    (\(i,t) -> do pos <- posicionPorEquipo 2 (tamX, tamY)
                  pure $ crearCarroPorTipo (i+100) 2 pos t)
    (zip [1..] tipos2)

  bombas <- mapM (\i -> generarBomba i (tamX, tamY)) [1 .. cfgBombs cfg]
  obstaculosEst <- generarObstaculosEspaciados (cfgObstacles cfg) (tamX, tamY)

  pure Mundo
    { carros = carrosEq1 ++ carrosEq2
    , proyectiles = []
    , obstaculos = []
    , bombas = bombas
    , obstaculosEstaticos = obstaculosEst
    , tamanoMundo = (tamX, tamY)
    , memoria = memoriaMundo
    }

parseConfigDetallado :: String -> Either [String] ConfigInterna
parseConfigDetallado raw =
  let ls = map limpiarLinea (lines raw)
      lsValid = filter (not . null) ls
      kvs = mapMaybe lineaKV lsValid
      lk k = lookupCI k kvs

      errs = concat
        [ falta "tamX" (lk "tamx")
        , falta "tamY" (lk "tamy")
        ]

      tamXv = lk "tamx" >>= readMaybe
      tamYv = lk "tamy" >>= readMaybe
      (eq1Errs, eq1) = leerListaTipos (lk "equipo1")
      (eq2Errs, eq2) = leerListaTipos (lk "equipo2")

      bombsV = lk "bombs" >>= readMaybe
      obstV  = lk "obstacles" >>= readMaybe
      roundsV = case (lk "rondas", lk "rounds") of
                  (Just s, _) -> readMaybe s
                  (_, Just s) -> readMaybe s
                  _           -> Nothing

      timeV = case (lk "tiempomax", lk "timelimit", lk "duracion") of
                (Just s, _, _) -> readMaybe s
                (_, Just s, _) -> readMaybe s
                (_, _, Just s) -> readMaybe s
                _              -> Nothing

      bombsD  = maybe 6 id bombsV
      obstD   = maybe 8 id obstV
      roundsD = maybe 1 id roundsV
      timeD   = maybe 120.0 id timeV

      eq1Final = if null eq1 then replicate 4 Ligero else eq1
      eq2Final = if null eq2 then replicate 4 Pesado else eq2

      allErrs = errs ++ eq1Errs ++ eq2Errs

  in case (tamXv, tamYv) of
      (Just tx, Just ty) ->
        if null allErrs
          then Right ConfigInterna
                  { cfgTamX = tx
                  , cfgTamY = ty
                  , cfgEquipo1 = eq1Final
                  , cfgEquipo2 = eq2Final
                  , cfgBombs = max 0 bombsD
                  , cfgObstacles = max 0 obstD
                  , cfgRounds = max 1 roundsD
                  , cfgTimeLimit = max 1.0 timeD
                  }
          else Left allErrs
      _ -> Left (allErrs ++ ["No se pudieron leer tamX/tamY como números"])

limpiarLinea :: String -> String
limpiarLinea = trim . quitarComentario
  where
    quitarComentario s =
      case break (`elem` ['#']) s of
        (a,_) -> a

lineaKV :: String -> Maybe (String,String)
lineaKV s =
  case break (=='=') s of
    (k,'=':v) -> Just (map toLower (trim k), trim v)
    _         -> Nothing

lookupCI :: String -> [(String,String)] -> Maybe String
lookupCI k = lookup k

falta :: String -> Maybe a -> [String]
falta campo (Just _) = []
falta campo Nothing  = ["Falta clave obligatoria: " ++ campo]

leerListaTipos :: Maybe String -> ([String], [TipoCarro])
leerListaTipos Nothing = (["Lista de tipos ausente (se usarán defaults)"], [])
leerListaTipos (Just txt) =
  let tokens = filter (not . null) $ map trim $ splitMulti [',',';',' '] txt
      (errs, tipos) = foldr (\tok (es,ts) ->
                              case parseTipo tok of
                                Just t  -> (es, t:ts)
                                Nothing -> (("Tipo desconocido: " ++ tok):es, ts)
                            ) ([],[]) tokens
  in (errs, tipos)

parseTipo :: String -> Maybe TipoCarro
parseTipo s =
  case map toLower s of
    "ligero"     -> Just Ligero
    "l"          -> Just Ligero
    "pesado"     -> Just Pesado
    "p"          -> Just Pesado
    "cazacarros" -> Just Cazacarros
    "caza"       -> Just Cazacarros
    "c"          -> Just Cazacarros
    _            -> Nothing

splitMulti :: [Char] -> String -> [String]
splitMulti ds = foldr f [""]
  where
    f c (a:as)
      | c `elem` ds = "" : a : as
      | otherwise   = (c:a):as
    f _ [] = []

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- ==========================================================
-- EJECUTAR TORNEOS
-- ==========================================================

ejecutarTorneos :: Int -> IO ()
ejecutarTorneos n = do
  replicateM_ n ejecutarTorneo

ejecutarTorneo :: IO ()
ejecutarTorneo = do
  putStrLn "Generando mundo aleatorio..."
  m0 <- mundoAleatorio

  let statsIniciales = Map.fromList
        [ (carroId c, E.inicializarEstadisticasBot
                        (carroId c)
                        (team c)
                        (show $ tipoCarro c))
        | c <- carros m0
        ]

  let initial = GameState
                  { mundo = m0
                  , tiempo = 0
                  , ronda = 1
                  , modo = Jugando
                  , explosions = []
                  , bgIndex = 1
                  , proximoMeteoritoId = 1000
                  , tiempoProxMeteorito = 2.0
                  , actualTorneo = 1
                  , torneosSobrantes = 0
                  , tiempoEsperaVictoria = 3.0
                  , estadisticasBots = statsIniciales
                  , historialTorneos = []
                  }

  putStrLn "Iniciando torneo..."
  playIO (InWindow "Tank Game" (1000, 1000) (80, 80))
        (makeColor 0.05 0.05 0.15 1.0)
        60
        initial
        renderGame
        handleEventWithReset
        updateGame

  putStrLn "Torneo finalizado."
