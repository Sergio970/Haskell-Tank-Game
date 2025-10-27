module Rendering where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)
import System.Random (mkStdGen, randomRs)
import Types (MunicionTipo(..))
import Unidad
import GameTypes

window :: Display
window = InWindow "Haskell Tank Game" (1000, 1000) (80, 80)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 60

sizeScale :: Float
sizeScale = 10

teamColor :: Int -> Color
teamColor t = case t of
  1 -> red
  2 -> blue
  3 -> green
  4 -> orange
  _ -> greyN 0.7

toScreen :: Mundo -> (Float,Float) -> (Float,Float)
toScreen _ (x,y) = (x,y)

{-# NOINLINE t1Image #-}
{-# NOINLINE t2Image #-}
fallbackSprite :: Picture
fallbackSprite = Color (greyN 0.5) (rectangleSolid 40 20)

t1Image, t2Image :: Picture
t1Image = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/T1.png")
t2Image = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/T2.png")

t1WidthPx, t1HeightPx :: Float
t1WidthPx = 101; t1HeightPx = 56
t2WidthPx, t2HeightPx :: Float
t2WidthPx = 101; t2HeightPx = 56

getTeamSprite :: Int -> (Picture, Float, Float)
getTeamSprite t = if t == 1
  then (t1Image, t1WidthPx, t1HeightPx)
  else (t2Image, t2WidthPx, t2HeightPx)

drawTank :: Mundo -> CarroCombate -> Picture
drawTank m c =
  let (sx, sy) = toScreen m (posicionCarro c)
      col      = teamColor (team c)
      ang      = direccionCarro c
      angCanon = getdireccionCanon c
      (w, h)   = tamanoCarro c
      (img, imgW, imgH) = getTeamSprite (team c)
      sX = (w * sizeScale) / imgW
      sY = (h * sizeScale) / imgH
      bodyPic  = Translate sx sy $ Rotate ang $ Scale sX sY img
      canonPic = Color col $ Translate sx sy $ Rotate angCanon $
                   rectangleSolid (w * sizeScale * 1.2) (h * sizeScale / 4)
  in Pictures [bodyPic, canonPic]

drawHealthBar :: Mundo -> CarroCombate -> Picture
drawHealthBar m c =
  let (sx, sy) = toScreen m (posicionCarro c)
      (_, h)   = tamanoCarro c
      barW = 30; barH = 4
      e    = fromIntegral (energia c) :: Float
      eMax = 100.0
      r    = max 0 (min 1 (e / eMax))
      greenW = r * barW; redW = (1 - r) * barW
      yOff = (h * sizeScale) / 2 + 10
      greenPic = Color green  $ Translate (-barW/2 + greenW/2) 0 (rectangleSolid greenW barH)
      redPic   = Color red    $ Translate ( barW/2 - redW/2)   0 (rectangleSolid redW   barH)
      frame    = Color (greyN 0.2) (rectangleWire barW barH)
  in Translate sx (sy + yOff) (Pictures [redPic, greenPic, frame])

drawProjectile :: Mundo -> Proyectil -> Picture
drawProjectile m p =
  let (sx, sy) = toScreen m (posicionProyectil p)
  in Color yellow $ Translate sx sy (circleSolid 3)

-- =====================================================
-- Fondo animado: estrellas que parpadean lentamente
-- =====================================================

-- Genera un patrón fijo de estrellas con brillo oscilante
starField :: [(Float, Float, Float)]
starField = take 1500 $ zip3 xs ys bs
  where
    g  = mkStdGen 42
    xs = randomRs (-500, 500) g
    ys = randomRs (-500, 500) (mkStdGen 99)
    bs = randomRs (0.3, 1.0) (mkStdGen 123)

drawBackground :: Float -> Picture
drawBackground t =
  Pictures
    [ Translate x y $
        Color (makeColor b b 1 (0.7 + 0.3 * sin (t/2 + b))) $
          circleSolid 1
    | (x, y, b) <- starField
    ]

-- =====================================================
-- Render principal
-- =====================================================

renderGame :: GameState -> IO Picture
renderGame gs = do
  let m = mundo gs
      t = tiempo gs
      fondo = drawBackground t
      vivos = filter (\c -> energia c > 0) (carros m)
      tanks = map (drawTank m) vivos
      bars  = map (drawHealthBar m) vivos
      projs = map (drawProjectile m) (proyectiles m)
  pure $ Pictures (fondo : tanks ++ bars ++ projs)

handleEvent :: Event -> GameState -> IO GameState
handleEvent _ gs = pure gs

