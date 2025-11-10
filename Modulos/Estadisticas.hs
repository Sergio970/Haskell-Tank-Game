module Estadisticas where

import qualified Data.Map.Strict as Map
import Data.List (sortBy, maximumBy, intercalate)
import Data.Ord (comparing)
import Text.Printf (printf)
import Control.Exception (catch, IOException)

statsFilePath :: FilePath
statsFilePath = "logs/estadisticas.txt"


-- =====================================================
-- TIPOS DE DATOS PARA ESTADÍSTICAS
-- =====================================================

data EstadisticasBot = EstadisticasBot
  { botId :: Int
  , botTeam :: Int
  , botTipo :: String
  , disparosRealizados :: Int
  , impactosAcertados :: Int
  , danioCausado :: Int
  , tiempoVivo :: Float
  , tiempoTotal :: Float
  , muertes :: Int
  } deriving (Show, Eq)

data EstadisticasTorneo = EstadisticasTorneo
  { numeroTorneo :: Int
  , duracionTorneo :: Float
  , equipoGanador :: Int
  , tanquesIniciales :: [(Int, Int)]  -- (equipo, cantidad)
  , tanquesFinales :: [(Int, Int)]    -- (equipo, cantidad)
  , estadisticasBots :: [EstadisticasBot]
  , totalDisparos :: Int
  , totalImpactos :: Int
  } deriving (Show)

data EstadisticasAgregadas = EstadisticasAgregadas
  { totalTorneos :: Int
  , victoriasEquipo1 :: Int
  , victoriasEquipo2 :: Int
  , empates :: Int
  , duracionPromedio :: Float
  , duracionMaxima :: Float
  , duracionMinima :: Float
  , precisionPromedio :: Float
  , mejorPrecision :: Float
  , mejorBot :: Maybe EstadisticasBot
  , botMasLetal :: Maybe EstadisticasBot
  } deriving (Show)

-- =====================================================
-- INICIALIZACIÓN
-- =====================================================

inicializarEstadisticasBot :: Int -> Int -> String -> EstadisticasBot
inicializarEstadisticasBot bid team tipo = EstadisticasBot
  { botId = bid
  , botTeam = team
  , botTipo = tipo
  , disparosRealizados = 0
  , impactosAcertados = 0
  , danioCausado = 0
  , tiempoVivo = 0
  , tiempoTotal = 0
  , muertes = 0
  }

-- =====================================================
-- ACTUALIZACIÓN DE ESTADÍSTICAS
-- =====================================================

registrarDisparo :: Int -> Map.Map Int EstadisticasBot -> Map.Map Int EstadisticasBot
registrarDisparo bid stats = 
  Map.adjust (\s -> s { disparosRealizados = disparosRealizados s + 1 }) bid stats

registrarImpacto :: Int -> Int -> Map.Map Int EstadisticasBot -> Map.Map Int EstadisticasBot
registrarImpacto bid danio stats = 
  Map.adjust (\s -> s { impactosAcertados = impactosAcertados s + 1
                      , danioCausado = danioCausado s + danio }) bid stats

registrarTiempoVivo :: Float               -- ^ dt
                    -> [(Int, Bool)]       -- ^ [(botId, estaVivo)]
                    -> Map.Map Int EstadisticasBot
                    -> Map.Map Int EstadisticasBot
registrarTiempoVivo dt vivos stats =
  let vivosMap = Map.fromList vivos  -- Map botId -> estaVivo
  in Map.mapWithKey
       (\bid s ->
          let estaVivo = Map.findWithDefault False bid vivosMap
          in s { tiempoTotal = tiempoTotal s + dt
               , tiempoVivo  = if estaVivo
                               then tiempoVivo s + dt
                               else tiempoVivo s
               }
       )
       stats


registrarMuerte :: Int -> Map.Map Int EstadisticasBot -> Map.Map Int EstadisticasBot
registrarMuerte bid stats = 
  Map.adjust (\s -> s { muertes = muertes s + 1 }) bid stats

-- =====================================================
-- CÁLCULOS
-- =====================================================

calcularPrecision :: EstadisticasBot -> Float
calcularPrecision bot
  | disparosRealizados bot == 0 = 0.0
  | otherwise =
      let disp = fromIntegral (disparosRealizados bot) :: Float
          -- A efectos de porcentaje, no dejamos que impactos supere a disparos
          imp  = fromIntegral (min (impactosAcertados bot) (disparosRealizados bot)) :: Float
      in imp / disp * 100


calcularSupervivencia :: EstadisticasBot -> Float
calcularSupervivencia bot
  | tiempoTotal bot == 0 = 0.0
  | otherwise = tiempoVivo bot / tiempoTotal bot * 100

-- =====================================================
-- GUARDAR ESTADÍSTICAS EN ARCHIVO
-- =====================================================

guardarEstadisticasTorneo :: EstadisticasTorneo -> IO ()
guardarEstadisticasTorneo stats = do
  let contenido = formatearTorneo stats
  catch (appendFile statsFilePath contenido)
        (\e -> putStrLn $ "Error guardando estadísticas: " ++ show (e :: IOException))

formatearTorneo :: EstadisticasTorneo -> String
formatearTorneo stats = unlines
  [ "=========================================="
  , "TORNEO #" ++ show (numeroTorneo stats)
  , "=========================================="
  , "Duración: " ++ printf "%.2f" (duracionTorneo stats) ++ " segundos"
  , "Ganador: Equipo " ++ show (equipoGanador stats)
  , ""
  , "Tanques iniciales:"
  , intercalate "\n" [ "  Equipo " ++ show eq ++ ": " ++ show cant 
                     | (eq, cant) <- tanquesIniciales stats ]
  , ""
  , "Tanques finales:"
  , intercalate "\n" [ "  Equipo " ++ show eq ++ ": " ++ show cant 
                     | (eq, cant) <- tanquesFinales stats ]
  , ""
  , "Resumen de disparos:"
  , "  Total disparos: " ++ show (totalDisparos stats)
  , "  Total impactos: " ++ show (totalImpactos stats)
  , "  Precisión global: " ++ printf "%.2f%%" (calcularPrecisionGlobal stats)
  , ""
  , "Estadísticas por bot:"
  , "-------------------------------------------"
  , formatearEncabezado
  , intercalate "\n" (map formatearBot (estadisticasBots stats))
  , ""
  ]

formatearEncabezado :: String
formatearEncabezado = 
  printf "%-6s %-10s %-8s %-8s %-8s %-8s %-12s %-10s" 
    "ID" "Tipo" "Equipo" "Disparos" "Impactos" "Daño" "Superviv(%)" "Precisión(%)"

formatearBot :: EstadisticasBot -> String
formatearBot bot = 
  printf "%-6d %-10s %-8d %-8d %-8d %-8d %-12.2f %-10.2f"
    (botId bot)
    (botTipo bot)
    (botTeam bot)
    (disparosRealizados bot)
    (impactosAcertados bot)
    (danioCausado bot)
    (calcularSupervivencia bot)
    (calcularPrecision bot)

calcularPrecisionGlobal :: EstadisticasTorneo -> Float
calcularPrecisionGlobal stats
  | totalDisparos stats == 0 = 0.0
  | otherwise =
      let disp = fromIntegral (totalDisparos stats) :: Float
          imp  = fromIntegral (min (totalImpactos stats) (totalDisparos stats)) :: Float
      in imp / disp * 100

-- =====================================================
-- ESTADÍSTICAS AGREGADAS
-- =====================================================

calcularEstadisticasAgregadas :: [EstadisticasTorneo] -> EstadisticasAgregadas
calcularEstadisticasAgregadas [] = EstadisticasAgregadas 0 0 0 0 0 0 0 0 0 Nothing Nothing
calcularEstadisticasAgregadas torneos =
  let total = length torneos
      v1 = length $ filter ((== 1) . equipoGanador) torneos
      v2 = length $ filter ((== 2) . equipoGanador) torneos
      emp = length $ filter ((== 0) . equipoGanador) torneos
      
      duraciones = map duracionTorneo torneos
      durProm = sum duraciones / fromIntegral total
      durMax = maximum duraciones
      durMin = minimum duraciones
      
      todosLosBots = concatMap estadisticasBots torneos
      precisiones = map calcularPrecision todosLosBots
      precProm = if null precisiones then 0 else sum precisiones / fromIntegral (length precisiones)
      mejorPrec = if null precisiones then 0 else maximum precisiones
      
      mejorBotPorPrec = if null todosLosBots 
                        then Nothing 
                        else Just $ maximumBy (comparing calcularPrecision) todosLosBots
      
      mejorBotPorDanio = if null todosLosBots
                         then Nothing
                         else Just $ maximumBy (comparing danioCausado) todosLosBots
      
  in EstadisticasAgregadas
      { totalTorneos = total
      , victoriasEquipo1 = v1
      , victoriasEquipo2 = v2
      , empates = emp
      , duracionPromedio = durProm
      , duracionMaxima = durMax
      , duracionMinima = durMin
      , precisionPromedio = precProm
      , mejorPrecision = mejorPrec
      , mejorBot = mejorBotPorPrec
      , botMasLetal = mejorBotPorDanio
      }

guardarEstadisticasAgregadas :: [EstadisticasTorneo] -> IO ()
guardarEstadisticasAgregadas torneos = do
  let agregadas = calcularEstadisticasAgregadas torneos
      contenido = formatearAgregadas agregadas
  catch (appendFile "estadisticas.txt" contenido)
        (\e -> putStrLn $ "Error guardando estadísticas agregadas: " ++ show (e :: IOException))

formatearAgregadas :: EstadisticasAgregadas -> String
formatearAgregadas stats = unlines
  [ ""
  , "=========================================="
  , "ESTADÍSTICAS AGREGADAS - TODOS LOS TORNEOS"
  , "=========================================="
  , "Total de torneos: " ++ show (totalTorneos stats)
  , ""
  , "Victorias:"
  , "  Equipo 1: " ++ show (victoriasEquipo1 stats) ++ " (" ++ 
    printf "%.1f%%" (porcentaje (victoriasEquipo1 stats) (totalTorneos stats)) ++ ")"
  , "  Equipo 2: " ++ show (victoriasEquipo2 stats) ++ " (" ++ 
    printf "%.1f%%" (porcentaje (victoriasEquipo2 stats) (totalTorneos stats)) ++ ")"
  , "  Empates: " ++ show (empates stats) ++ " (" ++ 
    printf "%.1f%%" (porcentaje (empates stats) (totalTorneos stats)) ++ ")"
  , ""
  , "Duración de torneos:"
  , "  Promedio: " ++ printf "%.2f" (duracionPromedio stats) ++ " segundos"
  , "  Máxima: " ++ printf "%.2f" (duracionMaxima stats) ++ " segundos"
  , "  Mínima: " ++ printf "%.2f" (duracionMinima stats) ++ " segundos"
  , ""
  , "Precisión:"
  , "  Promedio: " ++ printf "%.2f%%" (precisionPromedio stats)
  , "  Mejor: " ++ printf "%.2f%%" (mejorPrecision stats)
  , ""
  , case mejorBot stats of
      Nothing -> "No hay datos de bots"
      Just bot -> unlines
        [ "Bot con mejor precisión:"
        , "  ID: " ++ show (botId bot)
        , "  Tipo: " ++ botTipo bot
        , "  Equipo: " ++ show (botTeam bot)
        , "  Precisión: " ++ printf "%.2f%%" (calcularPrecision bot)
        , "  Impactos: " ++ show (impactosAcertados bot) ++ "/" ++ show (disparosRealizados bot)
        ]
  , ""
  , case botMasLetal stats of
      Nothing -> ""
      Just bot -> unlines
        [ "Bot más letal:"
        , "  ID: " ++ show (botId bot)
        , "  Tipo: " ++ botTipo bot
        , "  Equipo: " ++ show (botTeam bot)
        , "  Daño total: " ++ show (danioCausado bot)
        , "  Impactos: " ++ show (impactosAcertados bot)
        ]
  , ""
  ]

porcentaje :: Int -> Int -> Float
porcentaje parte total
  | total == 0 = 0
  | otherwise = fromIntegral parte / fromIntegral total * 100

-- =====================================================
-- INICIALIZAR ARCHIVO
-- =====================================================

inicializarArchivoEstadisticas :: IO ()
inicializarArchivoEstadisticas = do
  catch (writeFile "estadisticas.txt" cabecera)
        (\e -> putStrLn $ "Error inicializando estadísticas: " ++ show (e :: IOException))
  where
    cabecera = unlines
      [ "=========================================="
      , "ESTADÍSTICAS DE TORNEOS - TANK GAME"
      , "=========================================="
      , ""
      ]
