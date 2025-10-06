module Entities.Tripulacion where

import System.Random (randomRIO)

-- =========================
-- Tipos de tripulación
-- =========================

data EstadoTripulante = Vivo | Muerto
  deriving (Show, Eq)

data Tripulacion = Tripulacion
  { comandante    :: EstadoTripulante
  , conductor     :: EstadoTripulante
  , artillero     :: EstadoTripulante
  , operadorRadio :: EstadoTripulante
  , cargador      :: EstadoTripulante
  } deriving (Show, Eq)

-- =========================
-- Utilidades
-- =========================

todosVivos :: Tripulacion -> Bool
todosVivos t =
  comandante t    == Vivo &&
  conductor t     == Vivo &&
  artillero t     == Vivo &&
  operadorRadio t == Vivo &&
  cargador t      == Vivo

contarVivos :: Tripulacion -> Int
contarVivos t =
  length $ filter (== Vivo)
    [ comandante t, conductor t, artillero t, operadorRadio t, cargador t ]

-- ============================================================
-- Muerte aleatoria de tripulante (sin conocer Carro)
-- ============================================================

matarTripulanteAleatorioSiDanio :: Int -> Tripulacion -> IO Tripulacion
matarTripulanteAleatorioSiDanio dmg t
  | dmg <= 0  = return t
  | otherwise = matarTripulanteAleatorio t

matarTripulanteAleatorio :: Tripulacion -> IO Tripulacion
matarTripulanteAleatorio t = do
  let vivos =
        [ ("conductor",     conductor t)
        , ("artillero",     artillero t)
        , ("operadorRadio", operadorRadio t)
        , ("cargador",      cargador t)
        , ("comandante",    comandante t)
        ]
      disponibles = [ r | (r, estado) <- vivos, estado == Vivo ]
  if null disponibles
    then return t
    else do
      idx <- randomRIO (0, length disponibles - 1)
      let elegido = disponibles !! idx
      return $ case elegido of
        "conductor"     -> t { conductor     = Muerto }
        "artillero"     -> t { artillero     = Muerto }
        "operadorRadio" -> t { operadorRadio = Muerto }
        "cargador"      -> t { cargador      = Muerto }
        "comandante"    -> t { comandante    = Muerto }
        _               -> t
