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
import Graphics.Gloss.Interface.IO.Game (playIO, Event(..), Key(..), SpecialKey(..), KeyState(..))
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
  
  -- Generar 6 bombas iniciales
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





-- Genera una bomba aleatoria
generarBomba :: Int -> Size -> IO Bomba
generarBomba bid (tamX, tamY) = do
  x <- randomRIO (-tamX/2 + 20, tamX/2 - 20)
  y <- randomRIO (-tamY/2 + 20, tamY/2 - 20)
  radio <- randomRIO (6.0, 12.0)
  pure $ Bomba { bombaId = bid, posicionBomba = (x,y), radioBomba = radio, activaBomba = False, tiempoBomba = 3.0 }

-- Genera un meteorito aleatorio
generarMeteorito :: Int -> Size -> IO Meteorito
generarMeteorito mid (tamX, tamY) = do
  -- Decidir si aparece en izquierda o derecha
  lado <- randomRIO (0 :: Int, 1)
  
  let (posX, dirX) = if lado == 0
                     then (-tamX/2 + 20, 150)   -- Izquierda → derecha
                     else (tamX/2 - 20, -150)   -- Derecha → izquierda
  
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

-- Genera múltiples meteoritos
generarMeteoritos :: Int -> Size -> IO [Meteorito]
generarMeteoritos n tam = mapM (\i -> generarMeteorito i tam) [1..n]

--------------------------------
-- Sistema de combate
--------------------------------

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



-- Genera obstáculos con separación mínima entre ellos
generarObstaculosEspaciados :: Int -> Size -> IO [ObstaculoEstatico]
generarObstaculosEspaciados n tam = generarConDistancia [] n 0
  where
    distanciaMinima = 100.0  -- Distancia mínima entre obstáculos (ajustable)
    maxIntentos = 50         -- Intentos máximos por obstáculo
    
    generarConDistancia acc 0 _ = pure acc
    generarConDistancia acc restantes intentos
      | intentos > maxIntentos = 
          -- Si falla muchas veces, aceptar el candidato de todos modos
          generarConDistancia acc (restantes - 1) 0
      | otherwise = do
          candidato <- generarObstaculoEstatico (n - restantes + 1) tam
          let (cx, cy) = posicionObstaculoEstatico candidato
              muyCerca obs = 
                let (ox, oy) = posicionObstaculoEstatico obs
                    dx = ox - cx
                    dy = oy - cy
                    dist = sqrt (dx*dx + dy*dy)
                    -- Sumar radios para evitar superposición
                    radioTotal = tamanoObstaculoEstatico candidato + tamanoObstaculoEstatico obs
                in dist < (distanciaMinima + radioTotal)
          
          if null acc || not (any muyCerca acc)
            then generarConDistancia (candidato:acc) (restantes - 1) 0
            else generarConDistancia acc restantes (intentos + 1)  -- Reintentar


procesar (mundo, exps) (RobotObstaculoEstatico cid oid) =
  case (buscarCarro cid (carros mundo), buscarObstaculoEstatico oid (obstaculosEstaticos mundo)) of
    (Just c, Just obs) -> do
      let (cx, cy) = posicionCarro c
          (ox, oy) = posicionObstaculoEstatico obs
          (dx, dy) = normalize (cx - ox, cy - oy)
          pushForce = 25.0
          (vx, vy) = velocidadCarro c
          c' = c { velocidad = (vx + dx * pushForce, vy + dy * pushForce) }
      pure (reemplazarCarro c' (carros mundo) mundo, exps)

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

buscarMeteorito :: Int -> [Meteorito] -> Maybe Meteorito
buscarMeteorito mid = safeHead . filter ((== mid) . meteoritoId)

-- Nuevos helpers: bombas
buscarBomba :: Int -> [Bomba] -> Maybe Bomba
buscarBomba bid = safeHead . filter ((== bid) . bombaId)

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

-- Actualizar meteoritos
actualizarMeteoritos :: Float -> Mundo -> Mundo
actualizarMeteoritos dt m =
  let mets = obstaculos m
      (tamX, tamY) = tamanoMundo m
      
      -- Actualizar posición y estelas
      mets' = map (actualizarMeteorito dt (tamX, tamY)) mets
      
      -- Generar nuevas estelas
      metsConEstelas = map (\met ->
        let nuevasEstelas = generarEstelasDesdeMeteorito met
        in met { estelas = nuevasEstelas ++ estelas met }
        ) mets'
      
      -- Filtrar destruidos
      metsVivos = filter (\met -> vida met > 0) metsConEstelas
  in m { obstaculos = metsVivos }

actualizarMeteorito :: Float -> Size -> Meteorito -> Meteorito
actualizarMeteorito dt (tamX, tamY) met =
  let (x, y) = posicionMeteorito met
      (vx, vy) = velocidadMeteorito met
      
      x' = x + vx * dt
      y' = y + vy * dt
      y'' = max (-tamY/2) (min (tamY/2) y')
      
      -- Actualizar estelas del meteorito
      estelas' = map (\e -> e { estelaVida = estelaVida e - dt }) (estelas met)
      estelasVivas = filter (\e -> estelaVida e > 0) estelas'
  in met 
      { posicionMeteorito = (x', y'')
      , velocidadMeteorito = (vx, vy)
      , rotacionMeteorito = rotacionMeteorito met + velocidadRotacion met
      , estelas = estelasVivas  -- se actualizan junto con el meteorito
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

actualizarMeteoritosEnPartida :: Float -> GameState -> IO GameState
actualizarMeteoritosEnPartida dt gs = do
  let tiempoProx = tiempoProxMeteorito gs - dt
      proximoId = proximoMeteoritoId gs
      m = mundo gs
      (tamX, tamY) = tamanoMundo m
  
  if tiempoProx <= 0
    then do
      -- Generar nuevo meteorito
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

    procesar (mundo, exps) (RobotObstaculoEstatico cid oid) =
      case (buscarCarro cid (carros mundo), buscarObstaculoEstatico oid (obstaculosEstaticos mundo)) of
        (Just car, Just obs) -> do
          let (cx, cy) = posicionCarro car
              (ox, oy) = posicionObstaculoEstatico obs
              (dx, dy) = normalize (cx - ox, cy - oy)
              pushForce = 30.0  -- Fuerza de empuje al chocar
              (vx, vy) = velocidadCarro car
              car' = car { velocidad = (vx + dx * pushForce, vy + dy * pushForce) }
          pure (reemplazarCarro car' (carros mundo) mundo, exps)
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

    procesar (mundo, exps) (RobotMeteorito cid mid) =
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
          pure (reemplazarCarro carEmpujado (carros mundo) mundo, explosion : exps)
        _ -> pure (mundo, exps)

    procesar (mundo, exps) (ProyectilMeteorito pid mid) =
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
          pure (mundoActualizado, explosion : (explosionExtra ++ exps))
        _ -> pure (mundo, exps)

    procesar (mundo, exps) (RobotEstela cid eid) =
      case buscarCarro cid (carros mundo) of
        Just car -> do
          let todasEstelas = concat [estelas met | met <- obstaculos mundo]
              estelaOpt = safeHead (filter ((== eid) . estelaId) todasEstelas)
          case estelaOpt of
            Just est -> do
              let dano = max 1 (round (2.0 * estelaIntensidad est))
                  carDanado = setEnergia (max 0 (energia car - dano)) car
              pure (reemplazarCarro carDanado (carros mundo) mundo, exps)
            Nothing -> pure (mundo, exps)
        _ -> pure (mundo, exps)

    -- Activar bomba por contacto con tanque
    procesar (mundo, exps) (RobotBomba _cid bid) =
      case buscarBomba bid (bombas mundo) of
        Just b ->
          if activaBomba b
            then pure (mundo, exps)
            else
              let b' = b { activaBomba = True, tiempoBomba = 3.0 }
              in pure (reemplazarBomba b' (bombas mundo) mundo, exps)
        Nothing -> pure (mundo, exps)

    -- Activar bomba por contacto con meteorito
    procesar (mundo, exps) (MeteoritoBomba _mid bid) =
      case buscarBomba bid (bombas mundo) of
        Just b ->
          if activaBomba b
            then pure (mundo, exps)
            else
              let b' = b { activaBomba = True, tiempoBomba = 3.0 }
              in pure (reemplazarBomba b' (bombas mundo) mundo, exps)
        Nothing -> pure (mundo, exps)

-- Actualizar bombas: decrementa temporizador y detona
actualizarBombasEnMundo :: Float -> Mundo -> (Mundo, [Explosion])
actualizarBombasEnMundo dt m =
  let step b = if activaBomba b then b { tiempoBomba = tiempoBomba b - dt } else b
      bs1 = map step (bombas m)
      (detonan, vivas) = partition (\b -> activaBomba b && tiempoBomba b <= 0) bs1
      exps = [ Explosion { explosionPos = posicionBomba b, explosionTime = 0.6, explosionType = ImpactExplosion }
             | b <- detonan
             ]
  in (m { bombas = vivas }, exps)

updateGame :: Float -> GameState -> IO GameState
updateGame dt gs =
  case modo gs of
    Menu -> pure gs
    Victoria _ -> pure gs
    Jugando -> do
      gsConMeteoritos <- actualizarMeteoritosEnPartida dt gs
      let m0 = mundo gsConMeteoritos
          tiempoActual = tiempo gsConMeteoritos
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
          m4  = actualizarMeteoritos dt m3
          eventos = checkCollisions m4
      
      (m5, nuevasExplosiones) <- aplicarEventosColision eventos m4 gs
      
      -- Nuevas: actualizar bombas tras posibles activaciones
      let (m5b, explosionesBombas) = actualizarBombasEnMundo dt m5

          muertosDespues = filter (\c -> energia c <= 0) (carros m4)
          nuevasMuertes = filter (\c -> all ((/= carroId c) . carroId) muertosAntes) muertosDespues
          explosionesDeathActual = [ Explosion 
                                       { explosionPos = posicionCarro c
                                       , explosionTime = 1.0
                                       , explosionType = DeathExplosion
                                       }
                                   | c <- nuevasMuertes
                                   ]
          
          m6 = m5b { carros = filter ((> 0) . energia) (carros m5b) }
          
          -- Actualizar explosiones existentes
          explosionesActualizadas = [ e { explosionTime = explosionTime e - dt }
                                    | e <- explosions gs
                                    , explosionTime e - dt > 0
                                    ]
          
          todasExplosiones = explosionesActualizadas
                             ++ nuevasExplosiones
                             ++ explosionesBombas
                             ++ explosionesDeathActual

          equiposVivos = nub (map team (carros m5b))

      if length equiposVivos == 1
        then
          let ganador = head equiposVivos
          in pure gsConMeteoritos { mundo = m6, explosions = todasExplosiones, modo = Victoria ganador }
        else
          pure gsConMeteoritos { mundo = m6, tiempo = tiempoActual + dt, explosions = todasExplosiones }


-- Reinicia el juego con un nuevo mundo aleatorio
reiniciarJuego :: GameState -> IO GameState
reiniciarJuego gs = do
  m0 <- mundoAleatorio  -- Genera un nuevo mundo
  pure GameState
    { mundo = m0
    , tiempo = 0
    , ronda = ronda gs + 1  -- Incrementa el contador de rondas
    , modo = Jugando        -- Vuelve directamente al juego
    , explosions = []
    , bgIndex = bgIndex gs  -- Mantiene el fondo seleccionado
    , proximoMeteoritoId = 1000
    , tiempoProxMeteorito = 2.0
    }
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
                  , bgIndex = 1
                  , proximoMeteoritoId = 1000
                  , tiempoProxMeteorito = 2.0
                  }
  playIO window backgroundColor fps initial renderGame handleEventWithReset updateGame

handleEventWithReset :: Event -> GameState -> IO GameState
handleEventWithReset (EventKey (Char 'r') Down _ _) gs = 
  case modo gs of
    Menu -> pure gs  -- En el menú no hace nada
    _ -> do
      putStrLn $ "Reiniciando partida... (Ronda " ++ show (ronda gs + 1) ++ ")"
      reiniciarJuego gs

handleEventWithReset event gs = GameTypes.handleEvent event gs
