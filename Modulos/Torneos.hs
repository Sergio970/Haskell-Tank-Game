module Torneos where

import System.Random (randomRIO)
import Control.Monad (foldM, replicateM, replicateM_)
import Data.List (sortBy, nub)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Data.List (partition, nub)
import qualified Data.Map.Strict as Map
import Graphics.Gloss.Interface.IO.Game (playIO, Event(..), Key(..), SpecialKey(..), KeyState(..))
import Graphics.Gloss (Display(..), Color(..))
import Text.Read (readMaybe)
import Data.Char (toLower, isSpace)
import Data.List (dropWhileEnd)
import Control.Exception (try, IOException)            -- nuevo

import Types (TipoCarro(..), MunicionTipo(..), Vector, Size, Position, Value(..))
import Unidad
import Objeto (Objeto(..))
import Bot (botEstrategico, BotAction(..))
import Physics (updatePosition, vectorNulo, normalize, distanceBetween)
import Collisions (CollisionEvent(..), checkCollisions)
import GameTypes
import Rendering

-- ==========================================================
-- TIPOS DE DATOS PARA TORNEOS
-- ==========================================================

-- Resultado de un torneo individual
data ResultadoTorneo = ResultadoTorneo
  { numeroTorneo :: Int
  , ganadorTorneo :: Int          -- ID del equipo ganador
  , tiempoTotal :: Float          -- Duración del torneo
  , estadisticasFinales :: [(Int, Int)]  -- (equipoId, tanques_vivos)
  } deriving (Show)

-- Estado del sistema de torneos
data EstadoTorneos = EstadoTorneos
  { torneoActual :: Int           -- Número del torneo actual
  , torneosRestantes :: Int       -- Cuántos torneos quedan por ejecutar
  , resultados :: [ResultadoTorneo]  -- Historial de resultados
  , enPartida :: Bool             -- ¿Hay una partida en curso?
  , tiempoEspera :: Float         -- Tiempo entre torneos (3 segundos)
  , estadoJuego :: Maybe GameState  -- Estado del juego actual
  } deriving (Show)

-- ==========================================================
-- INICIALIZACIÓN DEL SISTEMA DE TORNEOS
-- ==========================================================

-- Crear el estado inicial para N torneos consecutivos
inicializarTorneos :: Int -> EstadoTorneos
inicializarTorneos numTorneos = EstadoTorneos
  { torneoActual = 1
  , torneosRestantes = numTorneos
  , resultados = []
  , enPartida = False
  , tiempoEspera = 0.0
  , estadoJuego = Nothing
  }

-- ==========================================================
-- LÓGICA PRINCIPAL DE ACTUALIZACIÓN
-- ==========================================================

-- Actualizar el sistema de torneos cada frame
actualizarTorneos :: Float -> EstadoTorneos -> IO EstadoTorneos
actualizarTorneos dt estado
  -- Caso 1: Todos los torneos completados
  | torneosRestantes estado <= 0 = do
      putStrLn "\n========================================="
      putStrLn "¡TODOS LOS TORNEOS COMPLETADOS!"
      putStrLn "========================================="
      mostrarResumenFinal (resultados estado)
      pure estado
  
  -- Caso 2: Hay una partida en curso
  | enPartida estado = do
      case estadoJuego estado of
        Just gs -> do
          -- Actualizar el juego
          gsActualizado <- updateGame dt gs
          
          -- Verificar si el torneo terminó
          let terminado = torneoTerminado gsActualizado
          
          if terminado
          then do
            -- Torneo finalizado, procesar resultado
            let resultado = procesarFinTorneo (torneoActual estado) gsActualizado
            putStrLn $ "\n Torneo " ++ show (torneoActual estado) ++ " completado"
            mostrarResultado resultado
            
            pure estado
              { enPartida = False
              , tiempoEspera = 3.0  -- 3 segundos de espera
              , resultados = resultado : resultados estado
              , estadoJuego = Nothing
              }
          else
            -- Partida continúa
            pure estado { estadoJuego = Just gsActualizado }
        
        Nothing -> pure estado
  
  -- Caso 3: Esperando entre torneos
  | tiempoEspera estado > 0 = do
      let nuevoTiempo = tiempoEspera estado - dt
      
      if nuevoTiempo <= 0
      then do
        -- Iniciar nuevo torneo
        putStrLn $ "\n Iniciando Torneo " ++ show (torneoActual estado + 1) ++ "..."
        nuevoEstadoJuego <- crearNuevoTorneo
        
        pure estado
          { torneoActual = torneoActual estado + 1
          , torneosRestantes = torneosRestantes estado - 1
          , enPartida = True
          , tiempoEspera = 0.0
          , estadoJuego = Just nuevoEstadoJuego
          }
      else
        pure estado { tiempoEspera = nuevoTiempo }
  
  -- Caso 4: Iniciar primer torneo
  | otherwise = do
      putStrLn $ "\n Iniciando Torneo " ++ show (torneoActual estado) ++ "..."
      nuevoEstadoJuego <- crearNuevoTorneo
      
      pure estado
        { enPartida = True
        , estadoJuego = Just nuevoEstadoJuego
        }

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

-- Helper: crear carro según TipoCarro
crearCarroPorTipo :: Int -> Int -> (Float, Float) -> TipoCarro -> CarroCombate
crearCarroPorTipo cid equipo pos tipo = case tipo of
  Ligero     -> carroLigero cid equipo pos
  Pesado     -> carroPesado cid equipo pos
  Cazacarros -> cazacarros cid equipo pos

-- ==========================================================
-- CREACIÓN Y ACTUALIZACIÓN DE TORNEOS (VERSIÓN PLACEHOLDER)
-- ==========================================================

crearNuevoTorneo :: IO GameState
crearNuevoTorneo = do
  mundo <- mundoDesdeConfig
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
    }

-- Actualizar juego (MINIMALISTA - solo incrementa tiempo y aplica fricción)
actualizarJuegoSimple :: Float -> GameState -> GameState
actualizarJuegoSimple dt gs =
  let m = mundo gs
      tiempoNuevo = tiempo gs + dt
      
      -- Aplicar fricción básica a carros
      carrosConFriccion = map aplicarFriccion (carros m)
      
      mundoActualizado = m { carros = carrosConFriccion }
  in gs { mundo = mundoActualizado, tiempo = tiempoNuevo }
  where
    aplicarFriccion carro =
      let (vx, vy) = velocidadCarro carro
          friccion = 0.95
          vx' = vx * friccion
          vy' = vy * friccion
      in setVelocidadCarro (vx', vy') carro

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

-- ==========================================================
-- CONDICIÓN DE FINALIZACIÓN
-- ==========================================================

-- Verificar si un torneo ha terminado
torneoTerminado :: GameState -> Bool
torneoTerminado gs =
  let equiposVivos = contarEquiposVivos (mundo gs)
  in length equiposVivos <= 1  -- Solo queda 1 equipo (o ninguno)

-- Contar cuántos equipos tienen tanques vivos
contarEquiposVivos :: Mundo -> [Int]
contarEquiposVivos m =
  let carrosVivos = filter (\c -> energia c > 0) (carros m)
      equipos = map team carrosVivos
  in nub equipos

-- ==========================================================
-- PROCESAMIENTO DE RESULTADOS
-- ==========================================================

-- Procesar el resultado de un torneo finalizado
procesarFinTorneo :: Int -> GameState -> ResultadoTorneo
procesarFinTorneo numTorneo gs =
  let m = mundo gs
      carrosVivos = filter (\c -> energia c > 0) (carros m)
      
      -- Estadísticas por equipo
      estadisticas = contarPorEquipo carrosVivos
      
      -- Determinar ganador (equipo con más tanques vivos)
      ganador = if null estadisticas
                then 0  -- Empate (todos muertos)
                else fst $ head $ sortBy (comparing (negate . snd)) estadisticas
  in ResultadoTorneo
    { numeroTorneo = numTorneo
    , ganadorTorneo = ganador
    , tiempoTotal = tiempo gs
    , estadisticasFinales = estadisticas
    }

-- Contar tanques vivos por equipo
contarPorEquipo :: [CarroCombate] -> [(Int, Int)]
contarPorEquipo carros =
  let equipos = nub $ map team carros
      contar eq = (eq, length $ filter (\c -> team c == eq) carros)
  in map contar equipos

-- ==========================================================
-- VISUALIZACIÓN DE RESULTADOS
-- ==========================================================

-- Mostrar resultado de un torneo individual
mostrarResultado :: ResultadoTorneo -> IO ()
mostrarResultado resultado = do
  putStrLn $ "  Ganador: Equipo " ++ show (ganadorTorneo resultado)
  putStrLn $ "  Tiempo: " ++ show (round (tiempoTotal resultado)) ++ "s"
  putStrLn "  Tanques sobrevivientes:"
  mapM_ (\(eq, n) -> putStrLn $ "    Equipo " ++ show eq ++ ": " ++ show n ++ " tanques")
    (estadisticasFinales resultado)

-- Mostrar resumen final de todos los torneos
mostrarResumenFinal :: [ResultadoTorneo] -> IO ()
mostrarResumenFinal resultados = do
  let total = length resultados
  putStrLn $ "\nTotal de torneos: " ++ show total
  putStrLn "\nGanadores por torneo:"
  mapM_ (\r -> putStrLn $ "  Torneo " ++ show (numeroTorneo r) 
                        ++ ": Equipo " ++ show (ganadorTorneo r))
    (reverse resultados)
  
  -- Estadísticas generales
  putStrLn "\nEstadísticas generales:"
  let victoriasEquipo1 = length $ filter (\r -> ganadorTorneo r == 1) resultados
      victoriasEquipo2 = length $ filter (\r -> ganadorTorneo r == 2) resultados
      empates = length $ filter (\r -> ganadorTorneo r == 0) resultados
  
  putStrLn $ "  Equipo 1: " ++ show victoriasEquipo1 ++ " victorias"
  putStrLn $ "  Equipo 2: " ++ show victoriasEquipo2 ++ " victorias"
  if empates > 0
    then putStrLn $ "  Empates: " ++ show empates
    else pure ()
  
  -- Tiempo promedio
  let tiempoPromedio = sum (map tiempoTotal resultados) / fromIntegral total
  putStrLn $ "\nTiempo promedio por torneo: " ++ show (round tiempoPromedio) ++ "segundos."

-- =====================================================
-- Ejecutar múltiples torneos
-- =====================================================

ejecutarTorneos :: Int -> IO ()
ejecutarTorneos n = do
  replicateM_ n ejecutarTorneo

-- =====================================================
-- Un solo torneo
-- =====================================================

ejecutarTorneo :: IO ()
ejecutarTorneo = do
  putStrLn "Generando mundo aleatorio..."
  m0 <- mundoAleatorio
  let initial = GameState
                  { mundo = m0
                  , tiempo = 0
                  , ronda = 1
                  , modo = Menu
                  , explosions = []
                  , bgIndex = 1
                  , proximoMeteoritoId = 1000
                  , tiempoProxMeteorito = 2.0
                  , actualTorneo = 1 
                  , torneosSobrantes = 0
                  , tiempoEsperaVictoria = 3.0
                  }
  putStrLn "Iniciando torneo..."
  playIO window backgroundColor fps initial renderGame handleEventWithReset updateGame
  putStrLn "Torneo finalizado."

--------------------------------
-- Sistema de combate
--------------------------------

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
      (dx, dy) = normalize (x2 - x1, y1 - y2)
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

-- Actualizar bombas: cuenta atrás + explosión con daño radial
actualizarBombasEnMundo :: Float -> Mundo -> (Mundo, [Explosion])
actualizarBombasEnMundo dt m =
  let
    -- Reducir el tiempo de bombas activas
    step b = if activaBomba b
             then b { tiempoBomba = tiempoBomba b - dt }
             else b
    bs1 = map step (bombas m)

    -- Bombas que explotan
    (detonan, vivas) = partition (\b -> activaBomba b && tiempoBomba b <= 0) bs1

    -- Daño a los carros por explosión
    aplicarDanioBomba :: Bomba -> [CarroCombate] -> [CarroCombate]
    aplicarDanioBomba b cs =
      let (bx, by) = posicionBomba b
          radioImpacto = radioBomba b * 10.0
          maxDano = 80  -- daño máximo en el centro
      in [ if distanceBetween (posicionCarro c) (bx, by) <= radioImpacto
           then
             let dist = distanceBetween (posicionCarro c) (bx, by)
                 factor = max 0 (1.0 - dist / radioImpacto)
                 dano = round (fromIntegral maxDano * factor)
             in setEnergia (max 0 (energia c - dano)) c
           else c
         | c <- cs ]

    -- Aplicar daño de todas las bombas que detonan
    carrosDanados = foldr aplicarDanioBomba (carros m) detonan

    -- Crear explosiones visuales
    exps = [ Explosion
              { explosionPos = posicionBomba b
              , explosionTime = 0.8
              , explosionType = DeathExplosion
              }
           | b <- detonan ]

  in ( m { bombas = vivas, carros = carrosDanados }, exps )

updateGame :: Float -> GameState -> IO GameState
updateGame dt gs = 
  case modo gs of

    Menu -> pure gs

    -- ===========================
    -- MODO: Victoria entre torneos
    -- ===========================
    Victoria ganador -> do
      let tiempoRestante = tiempoEsperaVictoria gs - dt
      
      if tiempoRestante <= 0
      then do
        -- ¿Quedan torneos?
        if torneosSobrantes gs > 0
        then do
          putStrLn $ " Iniciando Torneo " ++ show (actualTorneo gs + 1) ++ "..."
          nuevoMundo <- mundoDesdeConfig
          
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
            , tiempoEsperaVictoria = 0.0
            }
        else do
          putStrLn "¡TODOS LOS TORNEOS COMPLETADOS!"
          pure gs { modo = FinTorneos }
      else
        pure gs { tiempoEsperaVictoria = tiempoRestante }

    -- ===========================
    -- MODO: Jugando
    -- ===========================
    Jugando -> do
      gsConMeteoritos <- actualizarMeteoritosEnPartida dt gs
      let m0 = mundo gsConMeteoritos
          tiempoActual = tiempo gsConMeteoritos
          vivos = filter (\c -> energia c > 0) (carros m0)
          muertosAntes = filter (\c -> energia c <= 0) (carros m0)

      m1 <- foldM
        (\mw c -> case botEstrategico mw c of
                    Just as -> aplicarAccionesBot dt mw tiempoActual (c, as)
                    Nothing -> pure mw
        )
        m0
        vivos

      let cs' = map (\c -> c { posicion = updatePosition dt (posicion c) (velocidad c) }) (carros m1)
          ps' = map (\p -> p { posicionProyectil = updatePosition dt (posicionProyectil p) (velocidadProyectil p) }) (proyectiles m1)
          m2  = m1 { carros = cs', proyectiles = ps' }
          m3  = limpiarProyectiles dt m2
          m4  = actualizarMeteoritos dt m3
          eventos = checkCollisions m4
      
      (m5, nuevasExplosiones) <- aplicarEventosColision eventos m4 gs
      
      let (m5b, explosionesBombas) = actualizarBombasEnMundo dt m5

          muertosDespues = filter (\c -> energia c <= 0) (carros m4)
          nuevasMuertes = filter (\c -> all ((/= carroId c) . carroId) muertosAntes) muertosDespues
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
        then do
          -- Fin por tiempo: elegir ganador por mayoría de tanques vivos (empate=0)
          let vivosFinal = carros m6
              estadisticas = contarPorEquipo vivosFinal
              ganadorTiempo = if null estadisticas
                              then 0
                              else fst $ head $ sortBy (comparing (negate . snd)) estadisticas
          pure gs
            { mundo = m6
            , tiempo = tiempoSiguiente
            , explosions = todasExplosiones
            , modo = Victoria ganadorTiempo
            , tiempoEsperaVictoria = 3.0
            }
        else if length equiposVivos <= 1
          then do
            let ganador = if null equiposVivos then 0 else head equiposVivos
            putStrLn $ " Torneo " ++ show (actualTorneo gs)
                      ++ " completado - Ganador: Equipo "
                      ++ show ganador
            pure gs
              { mundo = m6
              , tiempo = tiempoSiguiente
              , explosions = todasExplosiones
              , modo = Victoria ganador
              , tiempoEsperaVictoria = 3.0
              }
          else
            pure gs
              { mundo = m6
              , tiempo = tiempoSiguiente
              , explosions = todasExplosiones
              }

    -- ===========================
    -- MODO: Fin de todos los torneos
    -- ===========================
    FinTorneos -> pure gs

handleEventWithReset :: Event -> GameState -> IO GameState
handleEventWithReset (EventKey (Char 'r') Down _ _) gs = 
  case modo gs of
    Menu -> pure gs  -- En el menú no hace nada
    _ -> do
      putStrLn $ "Reiniciando partida... (Ronda " ++ show (ronda gs + 1) ++ ")"
      reiniciarJuego gs

handleEventWithReset event gs = GameTypes.handleEvent event gs

-- Reinicia el juego con un nuevo mundo aleatorio
reiniciarJuego :: GameState -> IO GameState
reiniciarJuego gs = do
  m0 <- mundoDesdeConfig  -- Genera un nuevo mundo
  pure GameState
    { mundo = m0
    , tiempo = 0
    , ronda = ronda gs + 1  -- Incrementa el contador de rondas
    , modo = Jugando        -- Vuelve directamente al juego
    , explosions = []
    , bgIndex = bgIndex gs  -- Mantiene el fondo seleccionado
    , proximoMeteoritoId = 1000
    , tiempoProxMeteorito = 2.0
    , actualTorneo = 1 
    , torneosSobrantes = 0
    , tiempoEsperaVictoria = 3.0
    }

-- Lee el archivo de configuracion y crea un mundo
mundoDesdeConfig :: IO Mundo
mundoDesdeConfig = do
  let path = "config.txt"
  eres <- try (readFile path) :: IO (Either IOException String)
  case eres of
    Left _ -> do
      putStrLn "No se pudo abrir Modulos/config.txt. Usando mundoAleatorio."
      mundoAleatorio
    Right contenido ->
      case parseConfigDetallado contenido of
        Left errs -> do
          putStrLn "Errores en config:"
          mapM_ (\e -> putStrLn ("  - " ++ e)) errs
          putStrLn "Usando mundoAleatorio."
          mundoAleatorio
        Right cfg -> construirMundoDesdeCfg cfg

--Lee número de rondas desde config (default 1)
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

-- Lee límite de tiempo por torneo (segundos, default 120)
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

-- Estructura interna del parseo
data ConfigInterna = ConfigInterna
  { cfgTamX      :: Float
  , cfgTamY      :: Float
  , cfgEquipo1   :: [TipoCarro]
  , cfgEquipo2   :: [TipoCarro]
  , cfgBombs     :: Int
  , cfgObstacles :: Int
  , cfgRounds    :: Int
  , cfgTimeLimit :: Float    -- + límite de tiempo en segundos
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

-- ==========================================================
-- PARSEO DE CONFIGURACIÓN
-- ==========================================================

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
      -- tiempo máx: acepta 'tiempoMax', 'timelimit' o 'duracion'
      timeV = case (lk "tiempomax", lk "timelimit", lk "duracion") of
                (Just s, _, _) -> readMaybe s
                (_, Just s, _) -> readMaybe s
                (_, _, Just s) -> readMaybe s
                _              -> Nothing
      -- Defaults
      bombsD  = maybe 6 id bombsV
      obstD   = maybe 8 id obstV
      roundsD = maybe 1 id roundsV
      timeD   = maybe 120.0 id timeV
      -- Si listas vacías, genera 4 por equipo (defaults)
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

-- Normaliza y elimina comentarios
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
leerListaTipos Nothing = (["Lista de tipos ausente (se usarán defaults aleatorios)"], [])
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