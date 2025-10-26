module Main where

import qualified Data.Map.Strict as Map

import System.Random (randomRIO)
import Data.Maybe (mapMaybe)
import Control.Monad (foldM, replicateM)

import Objeto (Objeto(..))
import Types (TipoCarro(Ligero, Pesado, Cazacarros), MunicionTipo(AP, AE), Vector, Size, Position)

import Unidad

import Bot (botEstrategico, BotAction(..))
import Collisions (CollisionEvent(..), checkCollisions)
import Physics (updatePosition, vectorNulo)

  -- ============================================================
  -- =========        Funciones de ejemplo              =========
  -- ============================================================

  -- Tripulación totalmente viva
tripulacionViva :: Tripulacion
tripulacionViva = Tripulacion Vivo Vivo Vivo Vivo Vivo

  -- Crear Carros de ejemplo con ID incluido (usando Objeto + CarroAtributos)
carroLigero :: Int -> Int -> (Float,Float) -> CarroCombate
carroLigero cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto
      { posicion  = pos
      , direccion = 0.0
      , direccionCanon = 0.0
      , velocidad = (10.0, 0.0)
      , tamano    = (4.0, 3.0)
      , atributos = CarroAtributos
        { carroIdE        = cid
        , equipoE         = equipo
        , tipoCarroE      = Ligero
        , energiaE        = 100
        , blindajeE       = blindajeBase Ligero
        , alcanceVisionE  = visionBase Ligero
        , alcanceRadioE   = radioBase Ligero
        , tripulacionE    = tripulacionViva
        , municionesE     =
            replicate 3 (Municion AP 75.0 Map.empty) ++
            replicate 2 (Municion AE 120.0 Map.empty)
        , cadenciaE       = 1.0
        , precisionBaseE  = 0.9
        , memoriaCarroE   = memoriaLigero              
        }
      }

carroPesado :: Int -> Int -> (Float,Float) -> CarroCombate
carroPesado cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto
      { posicion  = pos
      , direccion = 0.0
      , direccionCanon = 0.0
      , velocidad = (4.0, 0.0)
      , tamano    = (7.0, 5.0)
      , atributos = CarroAtributos
          { carroIdE        = cid
          , equipoE         = equipo
          , tipoCarroE      = Pesado
          , energiaE        = 300
          , blindajeE       = blindajeBase Pesado
          , alcanceVisionE  = visionBase Pesado
          , alcanceRadioE   = radioBase Pesado
          , tripulacionE    = tripulacionViva
          , municionesE     =
              replicate 4 (Municion AP 120.0 Map.empty) ++
              replicate 3 (Municion AE 150.0 Map.empty)
          , cadenciaE       = 2.0
          , precisionBaseE  = 0.8
          , memoriaCarroE   = memoriaPesado
          }
      }

cazacarros :: Int -> Int -> (Float,Float) -> CarroCombate
cazacarros cid equipo pos =
  aplicarEfectosTripulacion $
    Objeto
      { posicion  = pos
      , direccion = 0.0
      , direccionCanon = 0.0
      , velocidad = (7.0, 0.0)
      , tamano    = (6.0, 4.0)
      , atributos = CarroAtributos
          { carroIdE        = cid
          , equipoE         = equipo
          , tipoCarroE      = Cazacarros
          , energiaE        = 180
          , blindajeE       = blindajeBase Cazacarros
          , alcanceVisionE  = visionBase Cazacarros
          , alcanceRadioE   = radioBase Cazacarros
          , tripulacionE    = tripulacionViva              
          , municionesE     =
              replicate 3 (Municion AP 110.0 Map.empty) ++
              replicate 2 (Municion AE 140.0 Map.empty)
          , cadenciaE       = 1.5
          , precisionBaseE  = 0.85
          , memoriaCarroE   = memoriaCazacarros
          }
      }

-- ============================================================
-- Generador de carros de combate aleatorios
-- ============================================================

-- Número de carros totales en el mapa
numCarros :: Int
numCarros = 6

-- Genera un carro aleatorio de un tipo y equipo al azar
carroAleatorio :: Int -> IO CarroCombate
carroAleatorio cid = do
  tipo <- randomRIO (0 :: Int, 2)      -- 0=Ligero, 1=Pesado, 2=Cazacarros
  equipo <- randomRIO (1 :: Int, 2)    -- dos equipos
  pos <- posicionAleatoria (400, 300)  -- dentro de los límites del mapa
  return $ case tipo of
    0 -> carroLigero cid equipo pos
    1 -> carroPesado cid equipo pos
    2 -> cazacarros cid equipo pos
    _ -> carroLigero cid equipo pos

-- Genera una posición aleatoria dentro del tamaño del mundo
posicionAleatoria :: Size -> IO Position
posicionAleatoria (w, h) = do
  x <- randomRIO (-w/2, w/2)
  y <- randomRIO (-h/2, h/2)
  return (x, y)

-- ============================================================
-- Generador de mundo aleatorio
-- ============================================================

mundoAleatorio :: IO Mundo
mundoAleatorio = do
  tamX <- randomRIO (400, 800)
  tamY <- randomRIO (300, 600)
  -- numCarros <- randomRIO (6, 12)  Poner número aleatorio de carros en el mundo
  carrosAleatorios <- replicateM numCarros (carroAleatorio =<< randomRIO (1, 9999))
  return Mundo
    { carros = carrosAleatorios
    , proyectiles = []
    , tamanoMundo = (tamX, tamY)
    , memoria = memoriaMundo
    }

-- ============================================================
-- Ejemplo de uso (simulación simplificada)
-- ============================================================

ataqueInstantaneo :: Int -> CarroCombate -> CarroCombate -> IO (CarroCombate, Maybe CarroCombate)
ataqueInstantaneo pid atacante objetivo = do
  case procesarDisparo pid atacante objetivo of
    Nothing -> return (atacante, Nothing)
    Just (atacante', objetivoDaniado, dmg) -> do
      objetivoFinal <- aplicarDanioConMuerteAleatoria dmg objetivoDaniado
      return (atacante', Just objetivoFinal)

-- Función PURA: procesa todo el disparo e impacto
procesarDisparo :: Int -> CarroCombate -> CarroCombate 
                -> Maybe (CarroCombate, CarroCombate, Int)
procesarDisparo pid atacante objetivo = do
  (proj, atacante') <- dispararA pid atacante objetivo
  let m = municionProyectil proj
  objetivoAfter <- aplicarImpactoDirecto m objetivo
  let dmg = calcularDanioRecibido objetivo objetivoAfter
  return (atacante', objetivoAfter, dmg)

-- Función PURA auxiliar para calcular daño recibido
calcularDanioRecibido :: CarroCombate -> CarroCombate -> Int
calcularDanioRecibido antes despues =
  energia antes - energia despues 

-- ============================================================
-- Nota:
-- - Valores (penetración, daños, multiplicadores) son ejemplos y se pueden afinar.
-- - La simulación de trayectoria y tiempos entre disparos no está detallada; el ejemplo
--   `ataqueInstantaneo` asume impacto inmediato para facilitar presentación.
-- - Para ejecutar todo en GHCi: cargar el archivo y usar `mostrarVisionDe mundoEjemplo`
--   o probar `ataqueInstantaneo` entre carros de `mundoEjemplo`.
-- ============================================================

data GameState = GameState
  { mundo :: Mundo
  , tiempo :: Float
  , ronda :: Int
  }

main :: IO ()
main = do
  putStrLn "Hola, Haskell Tank Game!"
  mundoInicial <- mundoAleatorio                -- Inicializa robots y región
  mundoFinal <- runTournament mundoInicial      -- Lanza el ciclo principal
  putStrLn $ "Torneo finalizado. Equipos restantes: " ++ show (equiposEnJuego mundoFinal) 
  case equipoGanador mundoFinal of
    Just e  -> putStrLn $ "¡Gana el equipo " ++ show e ++ "!"
    Nothing -> putStrLn "Empate o timeout."


updateGame :: Float -> GameState -> IO GameState
updateGame dt gs = do
  nuevoMundo <- bucleTorneo dt (mundo gs)
  return gs { mundo = nuevoMundo }

-- ============================================================
-- Bucle de torneo “driver” (punto de entrada del ciclo)
-- ============================================================

tickSeconds :: Float
tickSeconds = 1 / 60        -- tiempo que avanza el juego en cada iteración del bucle principal=60 actualizaciones por segundo (60 FPS)

maxTicks :: Int
maxTicks = 60 * 180         -- número máximo de iteraciones que ejecutará el bucle del torneo antes de detenerse automáticamente.
                                --10800 ciclos * (1/60 segundos por ciclo) = 180 segundos = 3 minutos
                                --el torneo se detendrá automáticamente tras 3 minutos de simulación para evitar bucles infinitos.
runTournament :: Mundo -> IO Mundo --Ejecuta el torneo completo desde el estado inicial hasta que termine.
runTournament = loop 0 --inicializamos un contador t en 0. Este contador representa el número de iteraciones (ticks) que han pasado.
  where
    loop :: Int -> Mundo -> IO Mundo
    loop t m                                --Antes de ejecutar una nueva iteración comprobamos dos cosas:
      | torneoTerminado m = pure m          -- termina el juego si queda 1 o 0 equipos.
      | t >= maxTicks     = pure m          -- termina el juego si se alcanzó el tiempo máximo permitido.
      | otherwise = do
          m' <- bucleTorneo tickSeconds m   --bucleTorneo recibe: tickSeconds (cuánto tiempo ha pasado desde el último ciclo) y m (el estado actual del mundo).
          loop (t + 1) m'                   --incrementamos el contador y volvemos a empezar

-- criterio de final de torneo (queda 1 equipo vivo o ninguno)
torneoTerminado :: Mundo -> Bool
torneoTerminado m = length (equiposEnJuego m) <= 1

--Equipos en juego son todos aquellos equipos que todavía tienen al menos un carro activo en el campo de batalla->al menos un tanque con energía > 0
equiposEnJuego :: Mundo -> [Int]
equiposEnJuego m = nub [ team c | c <- carros m, energia c > 0 ]
  where
    -- nub elimina elementos duplicados de una lista->se aplica porque puede haber varios tanques del mismo equipo vivos.
    nub :: (Eq a) => [a] -> [a]
    nub []     = []
    nub (x:xs) = x : nub (filter (/= x) xs)

equipoGanador :: Mundo -> Maybe Int
equipoGanador m =
    case equiposEnJuego m of
      [e] -> Just e --Si la lista tiene exactamente un elemento, significa que hay un único equipo en juego → ese es el ganador.
      _   -> Nothing --Si la lista tiene 0 elementos (todos destruidos) o más de uno (todavía hay combate), entonces no hay un ganador claro.

    -- ============================================================
    -- Bucle principal del juego (lógica del torneo)
    -- ============================================================

bucleTorneo :: Float -> Mundo -> IO Mundo
bucleTorneo dt mundoActual = do
  -- 1. Los bots deciden sus acciones
  let decisiones = mapMaybe (\car -> fmap (\x -> (car, x)) (botEstrategico mundoActual car)) (carros mundoActual)

  -- 2. Se aplican las acciones (mover, disparar, etc.)
  -- Usamos (aplicarAccionesBot dt) para pasar el tiempo a la función que procesa las acciones.
  mundoTrasAcciones <- foldM (aplicarAccionesBot dt) mundoActual decisiones

  -- 3. La física se aplica a TODOS los objetos del mundo
  let mundoConTanquesMovidos = actualizarTanques dt mundoTrasAcciones
  let mundoConProyectilesMovidos = actualizarProyectiles dt mundoConTanquesMovidos

  -- 4. Se gestionan las colisiones y sus efectos
  let eventos = checkCollisions mundoConProyectilesMovidos
  mundoFinal <- aplicarEventos eventos mundoConProyectilesMovidos

  -- 5️. Evitar que se queden quietos
  let mundoConMovimiento = reactivarTanques mundoFinal

  -- 6️. Comprobar si ha terminado el juego
  if torneoTerminado mundoConMovimiento
    then do
      return mundoConMovimiento
    else
      return mundoConMovimiento

  return mundoFinal

-- Reactivamos los tanques que no estén haciendo nada
reactivarTanques :: Mundo -> Mundo
reactivarTanques m =
  let moverSiQuieto c =
        if energia c > 0 && vectorNulo (velocidad c)
          then c { direccion = direccion c + 10 }  -- pequeño cambio direccional
          else c
  in m { carros = map moverSiQuieto (carros m) }

  -- Actualiza la posición de todos los tanques en el mundo
actualizarTanques :: Float -> Mundo -> Mundo
actualizarTanques dt mundo =
  let cs = carros mundo
      -- Mapeamos cada tanque a una nueva versión con la posición actualizada
      cs' = map (\c -> c { posicion = updatePosition dt (posicion c) (velocidad c) }) cs
  in mundo { carros = cs' }

-- Actualiza la posición de todos los proyectiles en el mundo
actualizarProyectiles :: Float -> Mundo -> Mundo
actualizarProyectiles dt mundo =
  let ps = proyectiles mundo
      -- Mapeamos cada proyectil a una nueva versión de sí mismo con la posición actualizada
      ps' = map (\p -> p { posicionProyectil = updatePosition dt (posicionProyectil p) (velocidadProyectil p) }) ps
  in mundo { proyectiles = ps' }

-- Aplicar las acciones que devuelve cada bot
aplicarAccionesBot :: Float -> Mundo -> (CarroCombate, [BotAction]) -> IO Mundo
aplicarAccionesBot dt m (carro, acts) = foldM aplicar m acts
  where
    aplicar mundo (DispararA objetivoId) =
      case buscarCarro objetivoId (carros mundo) of
        Just obj ->
          let pid = length (proyectiles mundo) + 1
          in case dispararA pid carro obj of
              Just (proj, carroSinMun) ->
                return $ agregarProyectil proj
                      $ reemplazarCarro carroSinMun (carros mundo) mundo
              Nothing -> return mundo
        Nothing -> return mundo

    -- aplica la función moverCarro al carro que tenga el mismo ID que el que realiza la acción
    aplicar mundo (Mover dir) =
      let velocidadMax = fst (velocidad carro) -- Extrae la velocidad máxima del tanque
          (dirX, dirY) = dir                   -- Dirección que ha elegido el bot
          -- Calculamos el nuevo vector de velocidad
          nuevaVel = (dirX * velocidadMax, dirY * velocidadMax)
          -- Actualizamos el tanque con su nueva velocidad
          carroActualizado = carro { velocidad = nuevaVel }
      in return $ reemplazarCarro carroActualizado (carros mundo) mundo

    aplicar mundo (Girar angulo) =
      let c' = carro { direccion = direccion carro + angulo }
      in return $ reemplazarCarro c' (carros mundo) mundo

    aplicar mundo _ = return mundo

-- Aplica los efectos de las colisiones
aplicarEventos :: [CollisionEvent] -> Mundo -> IO Mundo
aplicarEventos eventos m = foldM procesarEvento m eventos
  where
    procesarEvento mundo (RobotHit carroId pid) = do
      case (buscarCarro carroId (carros mundo), buscarProyectil pid (proyectiles mundo)) of
        (Just car, Just proj) -> do
          let mun = municionProyectil proj
          case aplicarImpactoDirecto mun car of
            Just carDanado -> do
              -- Calculamos el daño REAL infligido (vida antes - vida después)
              let danoReal = energia car - energia carDanado
              carFinal <- aplicarDanioConMuerteAleatoria danoReal carDanado
              return $ removerProyectil pid
                    $ reemplazarCarro carFinal (carros mundo) mundo
            Nothing -> return $ removerProyectil pid mundo
        _ -> return mundo

    procesarEvento mundo (RobotRobot id1 id2) = do
      let maybeC1 = buscarCarro id1 (carros mundo)
          maybeC2 = buscarCarro id2 (carros mundo)
      case (maybeC1, maybeC2) of
        (Just c1, Just c2) -> do
          let (x1, y1) = posicionCarro c1
              (x2, y2) = posicionCarro c2
              dx = x1 - x2
              dy = y1 - y2
              dist = max 1 (sqrt (dx*dx + dy*dy))
              -- vector unitario de separación
              ux = dx / dist
              uy = dy / dist
              -- nueva posición desplazada en direcciones opuestas
              c1' = c1 { posicion = (x1 + ux * 10, y1 + uy * 10)
                       , direccion = atan2 uy ux * 180 / pi }
              c2' = c2 { posicion = (x2 - ux * 10, y2 - uy * 10)
                       , direccion = atan2 (-uy) (-ux) * 180 / pi }
              carrosNuevos = reemplazarCarro c1' (carros mundo) $
                             reemplazarCarro c2' (carros mundo) mundo
          return carrosNuevos
        _ -> return mundo

    procesarEvento mundo (FronteraCarro carroId) =
      case buscarCarro carroId (carros mundo) of
        Just c ->
          let (x, y) = posicionCarro c
              (vx, vy) = velocidad c
              (limX, limY) = tamanoMundo mundo
              halfX = limX / 2 - 5
              halfY = limY / 2 - 5

              -- Clampea posición dentro de los límites
              x' = max (-halfX) (min halfX x)
              y' = max (-halfY) (min halfY y)

              -- Rebote simple: si toca un borde, invierte componente de velocidad
              vx' = if abs x >= halfX then -vx else vx
              vy' = if abs y >= halfY then -vy else vy

              -- Actualiza dirección para que mire hacia el nuevo vector
              nuevaDir = atan2 vy' vx'

              c' = c { posicion = (x', y')
                    , velocidad = (vx', vy')
                    , direccion = nuevaDir
                    }
          in return $ reemplazarCarro c' (carros mundo) mundo
        Nothing -> return mundo

    procesarEvento mundo (FronteraProyectil pid) =
      -- proyectil desaparece al salir
      return $ removerProyectil pid mundo

-- Helpers auxiliares
-- safeHead es polimórfica
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- buscarCarro: busca por id en una lista de CarroCombate
buscarCarro :: Int -> [CarroCombate] -> Maybe CarroCombate
buscarCarro cid = safeHead . filter ((== cid) . carroId)

-- buscarProyectil: busca por id en una lista de Proyectil
buscarProyectil :: Int -> [Proyectil] -> Maybe Proyectil
buscarProyectil pid = safeHead . filter ((== pid) . proyectilId)

-- reemplazarCarro: sustituye (o añade) un carro en la lista dentro del Mundo
reemplazarCarro :: CarroCombate -> [CarroCombate] -> Mundo -> Mundo
reemplazarCarro c cs m = m { carros = c : filter ((/= carroId c) . carroId) cs }

-- moverCarro: aplica un vector de desplazamiento al carro
moverCarro :: Float -> Vector -> CarroCombate -> CarroCombate
moverCarro dt direccion c =
  let (velocidadLineal, _) = velocidad c -- Usamos la velocidad definida en el tanque
      (dirX, dirY) = direccion
      (posX, posY) = posicionCarro c
      -- La nueva posición es: pos_actual + direccion * velocidad * tiempo
      nuevaPos = (posX + dirX * velocidadLineal * dt, posY + dirY * velocidadLineal * dt)
  in c { posicion = nuevaPos }