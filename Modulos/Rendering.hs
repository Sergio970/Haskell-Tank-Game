module Rendering where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)
import System.Random (mkStdGen, randomRs)
import Types (MunicionTipo(..), TipoCarro(..))
import Unidad
import GameTypes

-- =====================================================
-- Configuración de la ventana
-- =====================================================

window :: Display
window = InWindow "Haskell Tank Game" (1000, 1000) (80, 80)

backgroundColor :: Color
backgroundColor = makeColor 0.05 0.05 0.15 1.0  -- Azul oscuro espacial

fps :: Int
fps = 60

sizeScale :: Float
sizeScale = 10

-- =====================================================
-- Carga de Sprites (usando NOINLINE para cargar una vez)
-- =====================================================

fallbackSprite :: Picture
fallbackSprite = Color (greyN 0.5) (rectangleSolid 40 20)

fallbackCannon :: Picture
fallbackCannon = Color (greyN 0.7) (rectangleSolid 30 8)

fallbackBullet :: Picture
fallbackBullet = Color yellow (circleSolid 3)

fallbackExplosion :: Picture
fallbackExplosion = Color orange (circleSolid 15)

-- Sprites de tanques (azules)
{-# NOINLINE lightTankBlueImg #-}
lightTankBlueImg :: Picture
lightTankBlueImg = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/light_tank_blue.png")

{-# NOINLINE heavyTankBlueImg #-}
heavyTankBlueImg :: Picture
heavyTankBlueImg = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/heavy_tank_blue.png")

{-# NOINLINE hunterTankBlueImg #-}
hunterTankBlueImg :: Picture
hunterTankBlueImg = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/hunter_tank_blue.png")

-- Sprites de tanques (rojos)
{-# NOINLINE lightTankRedImg #-}
lightTankRedImg :: Picture
lightTankRedImg = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/light_tank_red.png")

{-# NOINLINE heavyTankRedImg #-}
heavyTankRedImg :: Picture
heavyTankRedImg = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/heavy_tank_red.png")

{-# NOINLINE hunterTankRedImg #-}
hunterTankRedImg :: Picture
hunterTankRedImg = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/hunter_tank_red.png")

-- Sprites de cañones
{-# NOINLINE cannonBlueImg #-}
cannonBlueImg :: Picture
cannonBlueImg = fromMaybe fallbackCannon $ unsafePerformIO (loadJuicyPNG "Assets/cannon_blue.png")

{-# NOINLINE cannonRedImg #-}
cannonRedImg :: Picture
cannonRedImg = fromMaybe fallbackCannon $ unsafePerformIO (loadJuicyPNG "Assets/cannon_red.png")

-- Sprite de bala
{-# NOINLINE bulletImg #-}
bulletImg :: Picture
bulletImg = fromMaybe fallbackBullet $ unsafePerformIO (loadJuicyPNG "Assets/bullet.png")

-- Sprites de explosiones
{-# NOINLINE explosionImpactImg #-}
explosionImpactImg :: Picture
explosionImpactImg = fromMaybe fallbackExplosion $ unsafePerformIO (loadJuicyPNG "Assets/explosion_impact.png")

{-# NOINLINE explosionDeathImg #-}
explosionDeathImg :: Picture
explosionDeathImg = fromMaybe fallbackExplosion $ unsafePerformIO (loadJuicyPNG "Assets/explosion_death.png")

-- Sprite decorativo removido (no se usa UFO)

-- =====================================================
-- Funciones de selección de sprites
-- =====================================================

-- Selecciona el sprite del cuerpo del tanque según tipo y equipo
getTankBodySprite :: TipoCarro -> Int -> Picture
getTankBodySprite Ligero 1 = lightTankRedImg
getTankBodySprite Ligero 2 = Scale (-1) 1 lightTankBlueImg  -- volteado horizontalmente
getTankBodySprite Pesado 1 = heavyTankRedImg
getTankBodySprite Pesado 2 = Scale (-1) 1 heavyTankBlueImg  -- volteado horizontalmente
getTankBodySprite Cazacarros 1 = hunterTankRedImg
getTankBodySprite Cazacarros 2 = Scale (-1) 1 hunterTankBlueImg  -- volteado horizontalmente
getTankBodySprite _ _ = fallbackSprite

-- Selecciona el sprite del cañón según equipo
getCannonSprite :: Int -> Picture
getCannonSprite 1 = cannonRedImg
getCannonSprite 2 = Scale (-1) 1 cannonBlueImg  -- volteado horizontalmente
getCannonSprite _ = fallbackCannon

-- Dimensiones estándar de los sprites (ajustar según tus PNGs reales)
tankSpriteWidth, tankSpriteHeight :: Float
tankSpriteWidth = 101
tankSpriteHeight = 56

cannonSpriteWidth, cannonSpriteHeight :: Float
cannonSpriteWidth = 120
cannonSpriteHeight = 30

bulletSpriteSize :: Float
bulletSpriteSize = 5  -- más pequeñas

explosionSpriteSize :: Float
explosionSpriteSize = 80

-- =====================================================
-- Funciones de conversión de coordenadas
-- =====================================================

toScreen :: Mundo -> (Float,Float) -> (Float,Float)
toScreen _ (x,y) = (x,y)

teamColor :: Int -> Color
teamColor t = case t of
  1 -> red
  2 -> blue
  3 -> green
  4 -> orange
  _ -> greyN 0.7

-- =====================================================
-- Renderizado de tanques
-- =====================================================

drawTank :: Mundo -> CarroCombate -> Picture
drawTank m c =
  let (sx, sy) = toScreen m (posicionCarro c)
      ang      = direccionCarro c
      angCanon = getdireccionCanon c
      (w, h)   = tamanoCarro c
      tipo     = tipoCarro c
      equipo   = team c
      
      -- Sprite del cuerpo
      bodySprite = getTankBodySprite tipo equipo
      bodyScaleX = (w * sizeScale) / tankSpriteWidth
      bodyScaleY = (h * sizeScale) / tankSpriteHeight
      bodyPic = Translate sx sy $ Rotate ang $ Scale bodyScaleX bodyScaleY bodySprite
      
      -- Sprite del cañón
      cannonSprite = getCannonSprite equipo
      cannonScaleX = (w * sizeScale * 1.2) / cannonSpriteWidth
      cannonScaleY = (h * sizeScale * 0.3) / cannonSpriteHeight
      canonPic = Translate sx sy $ Rotate angCanon $ Scale cannonScaleX cannonScaleY cannonSprite
      
  in Pictures [bodyPic, canonPic]

-- =====================================================
-- Barra de salud
-- =====================================================

drawHealthBar :: Mundo -> CarroCombate -> Picture
drawHealthBar m c =
  let (sx, sy) = toScreen m (posicionCarro c)
      (_, h)   = tamanoCarro c
      barW = 30
      barH = 4
      e    = fromIntegral (energia c) :: Float
      eMax = 100.0
      r    = max 0 (min 1 (e / eMax))
      greenW = r * barW
      redW = (1 - r) * barW
      yOff = (h * sizeScale) / 2 + 10
      
      greenPic = Color green  $ Translate (-barW/2 + greenW/2) 0 (rectangleSolid greenW barH)
      redPic   = Color red    $ Translate ( barW/2 - redW/2)   0 (rectangleSolid redW   barH)
      frame    = Color (greyN 0.2) (rectangleWire barW barH)
      
  in Translate sx (sy + yOff) (Pictures [redPic, greenPic, frame])

-- =====================================================
-- Renderizado de proyectiles
-- =====================================================

drawProjectile :: Mundo -> Proyectil -> Picture
drawProjectile m p =
  let (sx, sy) = toScreen m (posicionProyectil p)
      ang      = direccionProyectil p
      scale    = bulletSpriteSize / 10
  in Translate sx sy $ Rotate (ang + 90) $ Scale scale scale bulletImg  -- +90 para que apunte horizontalmente

-- =====================================================
-- Fondo animado con estrellas parpadeantes
-- =====================================================

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

-- Decoración de UFOs removida

-- =====================================================
-- Sistema de partículas para explosiones
-- =====================================================

drawExplosion :: Mundo -> Explosion -> Picture
drawExplosion m e =
  let (sx, sy) = toScreen m (explosionPos e)
      sprite = case explosionType e of
                 ImpactExplosion -> explosionImpactImg
                 DeathExplosion  -> explosionDeathImg
      t = explosionTime e
      pulseScale = (1.0 + 0.3 * sin (t * 10)) * (t / 1.0)
      scale = (explosionSpriteSize / 80) * pulseScale
      alpha = t / 1.0
  in Translate sx sy $ Scale scale scale $ 
       Color (makeColor 1 1 1 alpha) sprite

-- =====================================================
-- Render principal
-- =====================================================

renderGame :: GameState -> IO Picture
renderGame gs = do
  case modo gs of
    Menu -> pure drawMenu
    Jugando -> do
      let m = mundo gs
          t = tiempo gs
          
          fondo = drawBackground t
          
          vivos = filter (\c -> energia c > 0) (carros m)
          tanks = map (drawTank m) vivos
          bars  = map (drawHealthBar m) vivos
          
          projs = map (drawProjectile m) (proyectiles m)
          
          explosionPics = map (drawExplosion m) (explosions gs)
          
      pure $ Pictures ([fondo] ++ tanks ++ bars ++ projs ++ explosionPics)

-- Pantalla de menú inicial
drawMenu :: Picture
drawMenu =
  Pictures
    [ -- Fondo negro con estrellas
      drawBackground 0
      
      -- Título "SPACE WAR"
    , Translate (-225) 100 $ Scale 0.8 0.8 $ Color white $ Text "SPACE WAR"
      
      -- Subtítulo
    , Translate (-200) 0 $ Scale 0.3 0.3 $ Color (greyN 0.8) $ Text "Tank Combat Simulator"
      
      -- Instrucción
    , Translate (-180) (-100) $ Scale 0.25 0.25 $ Color yellow $ Text "Press ENTER to Start"
      
      -- Controles
    , Translate (-150) (-180) $ Scale 0.15 0.15 $ Color (greyN 0.6) $ Text "Press R to restart during game"
    ]

-- =====================================================
-- Event handler (sin cambios)
-- =====================================================

handleEvent :: Event -> GameState -> IO GameState
handleEvent _ gs = pure gs
