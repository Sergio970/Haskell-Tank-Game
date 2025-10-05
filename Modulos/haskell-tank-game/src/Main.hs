module Main where

import qualified Physics.Physics as Physics
import qualified Entities.Carro as Carro
import qualified Entities.Municion as Municion
import qualified Entities.Tripulacion as Tripulacion
import qualified Entities.Mundo as Mundo
import qualified Collisions.Collisions as Collisions
import qualified Rendering.Rendering as Rendering
import qualified AI.Bot as Bot

-- Funcion principal del juego.

-- Inicializa el mundo, los carros y la tripulacion(Start).
-- Inicia el bucle principal del juego(Frame).

-- ============================================================
-- =========        Funciones de ejemplo              =========
-- ============================================================

-- ============================================================
-- Ejemplo: crear carros, munición y un mundo
-- ============================================================

-- Helper para crear tripulación totalmente viva
tripulacionViva :: Tripulacion
tripulacionViva = Tripulacion Vivo Vivo Vivo Vivo Vivo

-- Crear Carros de ejemplo con ID incluido
carroLigero :: Int -> Int -> Position -> CarroCombate
carroLigero cid equipo pos = aplicarEfectosTripulacion $ CarroCombate {
    carroId = cid,
    team = equipo,
    tipoCarro = Ligero,
    posicionCarro = pos,
    direccionCarro = 0.0,
    velocidadCarro = (10.0, 0.0),
    tamanoCarro = (3.0, 3.0),
    energia = 100,
    blindaje = blindajeBase Ligero,
    alcanceVision = visionBase Ligero,
    alcanceRadio = radioBase Ligero,
    tripulacion = tripulacionViva,
    municiones = replicate 3 (Municion AP 75.0 memoriaMunicionAP) ++ replicate 2 (Municion AE 120.0 memoriaMunicionAE),
    cadencia = 1.0,
    precisionBase = 0.9,
    memoriaCarro = memoriaLigero
}

carroPesado :: Int -> Int -> Position -> CarroCombate
carroPesado cid equipo pos = aplicarEfectosTripulacion $ CarroCombate {
    carroId = cid,
    team = equipo,
    tipoCarro = Pesado,
    posicionCarro = pos,
    direccionCarro = 0.0,
    velocidadCarro = (4.0, 0.0),
    tamanoCarro = (5.0, 5.0),
    energia = 300,
    blindaje = blindajeBase Pesado,
    alcanceVision = visionBase Pesado,
    alcanceRadio = radioBase Pesado,
    tripulacion = tripulacionViva,
    municiones = replicate 4 (Municion AP 120.0 memoriaMunicionAP) ++ replicate 3 (Municion AE 150.0 memoriaMunicionAE),
    cadencia = 2.0,
    precisionBase = 0.8,
    memoriaCarro = memoriaPesado
}

cazacarros :: Int -> Int -> Position -> CarroCombate
cazacarros cid equipo pos = aplicarEfectosTripulacion $ CarroCombate {
    carroId = cid,
    team = equipo,
    tipoCarro = Cazacarros,
    posicionCarro = pos,
    direccionCarro = 0.0,
    velocidadCarro = (7.0, 0.0),
    tamanoCarro = (4.0, 4.0),
    energia = 180,
    blindaje = blindajeBase Cazacarros,
    alcanceVision = visionBase Cazacarros,
    alcanceRadio = radioBase Cazacarros,
    tripulacion = tripulacionViva,
    municiones = replicate 3 (Municion AP 110.0 memoriaMunicionAP) ++ replicate 2 (Municion AE 140.0 memoriaMunicionAE),
    cadencia = 1.5,
    precisionBase = 0.85,
    memoriaCarro = memoriaCazacarros
}

c1 = carroLigero 1 1 (10,10)
c2 = carroPesado 2 1 (40,20)
c3 = cazacarros 3 2 (200,20)

-- Mundo de ejemplo con dos equipos
mundoEjemplo :: Mundo
mundoEjemplo = Mundo {
    carros = [c1, c2, c3],
    proyectiles = [],
    tamanoMundo = (1000, 1000),
    memoria = memoriaMundo
}

-- ============================================================
-- Ejemplo de uso (simulación simplificada)
-- ============================================================

-- Simular que a ataca b: dispara, se crea proyectil y al impactar aplicamos daño
-- Para demo: hacemos impacto instantáneo (sin simular trayectoria)
ataqueInstantaneo :: Int -> CarroCombate -> CarroCombate -> IO (CarroCombate, Maybe CarroCombate)
ataqueInstantaneo pid atacante objetivo =
    case dispararA pid atacante objetivo of
        Nothing -> return (atacante, Nothing) -- sin munición
        Just (proj, atacante') -> do
            -- Calculamos daño según munición y blindaje
            let m = municionProyectil proj
                objetivoAfter = aplicarImpactoDirecto m objetivo
                dmg = energia objetivo - energia objetivoAfter
            objetivoFinal <- aplicarDanioConMuerteAleatoria dmg objetivoAfter
            return (atacante', Just objetivoFinal)

-- Función para mostrar lo que ve cada carro en un mundo
mostrarVisionDe :: Mundo -> IO ()
mostrarVisionDe mundo = mapM_ printVision (carros mundo)
  where
    printVision c = do
        let vistos = carrosVistosPor c mundo
        putStrLn $ "Carro (team " ++ show (team c) ++ ", tipo " ++ show (tipoCarro c) ++
                   ") ve " ++ show (map (tipoCarro) vistos)

-- ============================================================
-- Nota:
-- - Valores (penetración, daños, multiplicadores) son ejemplos y se pueden afinar.
-- - La simulación de trayectoria y tiempos entre disparos no está detallada; el ejemplo
--   `ataqueInstantaneo` asume impacto inmediato para facilitar presentación.
-- - Para ejecutar todo en GHCi: cargar el archivo y usar `mostrarVisionDe mundoEjemplo`
--   o probar `ataqueInstantaneo` entre carros de `mundoEjemplo`.
-- ============================================================