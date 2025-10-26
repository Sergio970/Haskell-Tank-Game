module Rendering where

import Main
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Unidad
import Types (TipoCarro(..))
import Objeto (Objeto(..))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)

-- Ventana y parámetros de render
window :: Display
window = InWindow "Haskell Tank Game" (1000, 1000) (80, 80)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 60

-- Escala visual para convertir el tamaño del mundo a píxeles
sizeScale :: Float
sizeScale = 10

-- Color por equipo
teamColor :: Int -> Color
teamColor t =
  case t of
    1 -> red
    2 -> blue
    3 -> green
    4 -> orange
    _ -> makeColorI 200 200 200 255

-- Conversión de coords: 0,0 del mundo coincide con el centro de la ventana de Gloss
toScreen :: Mundo -> (Float, Float) -> (Float, Float)
toScreen _ (x, y) = (x, y)

-- Carga de sprites por equipo (reemplaza la definición anterior de t1Image/t2Image con where).
{-# NOINLINE t1Image #-}
{-# NOINLINE t2Image #-}
fallbackSprite :: Picture
fallbackSprite = Color (greyN 0.5) (rectangleSolid 40 20)

t1Image, t2Image :: Picture
t1Image = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/T1.png")
t2Image = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/T2.png")

-- Dimensiones en píxeles de cada sprite (ajusta a tus imágenes reales).
t1WidthPx, t1HeightPx :: Float
t1WidthPx  = 101
t1HeightPx = 56

t2WidthPx, t2HeightPx :: Float
t2WidthPx  = 101
t2HeightPx = 56

getTeamSprite :: Int -> (Picture, Float, Float)
getTeamSprite t =
  case t of
    1 -> (t1Image, t1WidthPx, t1HeightPx)
    _ -> (t2Image, t2WidthPx, t2HeightPx)

-- Dibujo de un tanque (usa sprite por equipo)
drawTank :: Mundo -> CarroCombate -> Picture
drawTank m c =
  let (sx, sy) = toScreen m (posicionCarro c)
      col      = teamColor (team c)
      ang      = direccion c                 -- grados
      angCanon = getdireccionCanon c         -- grados del cañón
      (w, h)   = tamano c                    -- unidades de mundo
      (img, imgW, imgH) = getTeamSprite (team c)

      -- Escala para ajustar el PNG al tamaño del tanque en el mundo
      sX = (w * sizeScale) / imgW
      sY = (h * sizeScale) / imgH

      bodyPic  = Translate sx sy $ Rotate ang $ Scale sX sY img
      canonPic = Color col $ Translate sx sy $ Rotate angCanon $ rectangleSolid (w * sizeScale * 1.2) (h * sizeScale / 4)
  in Pictures [bodyPic, canonPic]

-- Barra de vida sobre el tanque
drawHealthBar :: Mundo -> CarroCombate -> Picture
drawHealthBar m c =
  let (sx, sy) = toScreen m (posicionCarro c)
      (_, h)   = tamano c
      barW = 30
      barH = 4
      e    = fromIntegral (energia c) :: Float
      eMax = fromIntegral (energiaE (atributos c)) :: Float
      r    = max 0 (min 1 (e / eMax))
      greenW = r * barW
      redW   = (1 - r) * barW
      yOff = (h * sizeScale) / 2 + 10
      greenPic = Color green  $ Translate (-barW/2 + greenW/2) 0 (rectangleSolid greenW barH)
      redPic   = Color red    $ Translate ( barW/2 - redW/2)   0 (rectangleSolid redW   barH)
      frame    = Color (greyN 0.2) (rectangleWire barW barH)
  in Translate sx (sy + yOff) (Pictures [redPic, greenPic, frame])

-- Dibujo de un proyectil
drawProjectile :: Mundo -> Proyectil -> Picture
drawProjectile m p =
  let (sx, sy) = toScreen m (posicionProyectil p)
  in Color yellow $ Translate sx sy (circleSolid 3)

-- Renderizado del estado completo
renderGame :: GameState -> IO Picture
renderGame gs = do
  let m = mundo gs
      tanks = map (drawTank m) (carros m)
      bars  = map (drawHealthBar m) (carros m)
      projs = map (drawProjectile m) (proyectiles m)
  pure $ Pictures (tanks ++ bars ++ projs)

-- Ignorar eventos de usuario
handleEvent :: Event -> GameState -> IO GameState
handleEvent _ gs = pure gs

-- Avance de simulación reutilizando Main.updateGame
stepGame :: Float -> GameState -> IO GameState
stepGame dt gs = updateGame dt gs

-- Avance manual: no avanza con el tiempo, solo con espacio
handleEventPause :: Event -> GameState -> IO GameState
handleEventPause (EventKey (SpecialKey KeySpace) Down _ _) gs = do
  gs' <- updateGame tickSeconds gs
  -- En modo pausa no cerramos la ventana al terminar
  pure gs'
handleEventPause _ gs = pure gs

stepGamePause :: Float -> GameState -> IO GameState
stepGamePause _ = pure

-- Punto de entrada pausado: avanza 1 tick por pulsación de espacio
renderPause :: IO ()
renderPause = do
  mundoInicial <- mundoAleatorio
  let initial = GameState { mundo = mundoInicial, tiempo = 0, ronda = 0 }
  playIO window backgroundColor fps initial renderGame handleEventPause stepGamePause

-- Punto de entrada gráfico para ejecutar la simulación con Gloss
-- Usa GameState y mundoEjemplo de Main.
render :: IO ()
render = do
  mundoInicial <- mundoAleatorio
  let initial = GameState { mundo = mundoInicial, tiempo = 0, ronda = 0 }
  playIO window backgroundColor fps initial renderGame handleEvent stepGame