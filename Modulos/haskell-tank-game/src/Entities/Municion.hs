module Entities.Municion where

import qualified Data.Map.Strict as Map

import Types.Types
  ( Memory, Position, Vector, Angle
  , MunicionTipo(..)
  )

import Types.Objeto (Objeto(..))
import Entities.Carro
  ( CarroCombate, CarroAtributos(..)
  , municiones, blindaje, posicionCarro, direccionCarro, team, energia
  , setEnergia
  )

-- ============ Tipos ============

data Municion = Municion
  { tipoMun    :: MunicionTipo
  , calibreMun :: Float      -- mm o valor abstracto
  , memoriaMun :: Memory
  } deriving (Show, Eq)

data Proyectil = Proyectil
  { proyectilId        :: Int
  , posicionProyectil  :: Position
  , direccionProyectil :: Angle
  , velocidadProyectil :: Vector
  , municionProyectil  :: Municion
  , disparadorTeam     :: Int
  , memoriaProj        :: Memory
  } deriving (Show)

-- ============================================================
-- Munición: penetración y daño según tipo y calibre
-- ============================================================

penetracionEstim :: Municion -> Float
penetracionEstim (Municion { tipoMun = AP, calibreMun = c }) = c * 30.0
penetracionEstim (Municion { tipoMun = AE, calibreMun = c }) = c * 8.0

danioEstim :: Municion -> Int
danioEstim (Municion { tipoMun = AP, calibreMun = c }) = round (c * 6.0)
danioEstim (Municion { tipoMun = AE, calibreMun = c }) = round (c * 4.0)

-- ============================================================
-- Elección de munición y disparo
-- ============================================================

buscarMunicionPreferida :: MunicionTipo -> CarroCombate -> Maybe Int
buscarMunicionPreferida mtype carro =
  let ms       = municiones carro
      indexed  = zip [0..] ms
      filtered = filter (\(_, m) -> tipoMun m == mtype) indexed
  in if null filtered
       then Nothing
       else Just (fst (maximumByCalibre filtered))
  where
    maximumByCalibre :: [(Int, Municion)] -> (Int, Municion)
    maximumByCalibre =
      foldl1 (\a@(_, m1) b@(_, m2) -> if calibreMun m1 >= calibreMun m2 then a else b)

elegirMunicionPara :: CarroCombate -> CarroCombate -> Maybe Int
elegirMunicionPara atacante objetivo =
  let ms     = municiones atacante
      iAP    = buscarMunicionPreferida AP atacante
      iAE    = buscarMunicionPreferida AE atacante
      tryAP  = case iAP of
                 Nothing -> Nothing
                 Just i  -> let m = ms !! i
                            in if penetracionEstim m > blindaje objetivo then Just i else Nothing
  in case tryAP of
       Just i  -> Just i
       Nothing -> iAE

-- Devuelve (proyectil, atacanteSinEsaMunición)
dispararA :: Int -> CarroCombate -> CarroCombate -> Maybe (Proyectil, CarroCombate)
dispararA pid atacante objetivo =
  case elegirMunicionPara atacante objetivo of
    Nothing  -> Nothing
    Just idx ->
      let -- extraemos municiones para construir el proyectil y luego quitarlas
          ms         = municiones atacante
          m          = ms !! idx
          pos        = posicionCarro atacante
          dir        = direccionCarro atacante
          velProj    = (0.0, 200.0)
          proyectil  = Proyectil pid pos dir velProj m (team atacante) (memoriaMun m)
          nuevaLista = take idx ms ++ drop (idx + 1) ms
          -- actualizamos el atacante modificando el campo 'municionesE' dentro de 'atributos'
          atacante'  =
            case atacante of
              obj@Objeto{ atributos = a@CarroAtributos{} } ->
                obj { atributos = a { municionesE = nuevaLista } }
      in Just (proyectil, atacante')

-- ============================================================
-- Aplicar impacto directo
-- ============================================================

aplicarImpactoDirecto :: Municion -> CarroCombate -> CarroCombate
aplicarImpactoDirecto mun objetivo =
  let pen     = penetracionEstim mun
      dmgBase = danioEstim mun
      dmg     = case tipoMun mun of
                  AP -> if pen > blindaje objetivo
                          then dmgBase * 2
                          else round (fromIntegral dmgBase * 0.6)
                  AE -> dmgBase
      nuevaE  = max 0 (energia objetivo - dmg)
  in setEnergia nuevaE objetivo
