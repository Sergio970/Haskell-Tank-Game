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

fallbackMeteorito :: Picture
fallbackMeteorito = Pictures
  [ Color (makeColor 0.4 0.3 0.2 1.0) (circleSolid 22)
  , Color (makeColor 0.6 0.4 0.3 1.0) (circleSolid 20)
  , Color (makeColor 0.5 0.35 0.25 1.0) (circleSolid 15)
  , Color (makeColor 0.4 0.3 0.2 1.0) (circleSolid 8)
  , Translate 10 5 $ Color (makeColor 0.4 0.3 0.2 1.0) (circleSolid 5)
  ]

fallbackEstela :: Picture
fallbackEstela = Color (makeColor 1.0 0.5 0.2 0.6) (circleSolid 20)

-- Fallback de fondo (visible si el PNG no carga)
fallbackBG :: Picture
fallbackBG = Color (makeColor 0.1 0.1 0.1 1.0) (rectangleSolid 1000 1000)

-- Intento de carga múltiple (prueba varias rutas)
loadFirst :: [FilePath] -> IO (Maybe Picture)
loadFirst [] = pure Nothing
loadFirst (p:ps) = do
  mp <- loadJuicyPNG p
  case mp of
    Just pic -> pure (Just pic)
    Nothing  -> loadFirst ps

-- Fondos (se cargan una vez, probando varios nombres y rutas relativas)
{-# NOINLINE bg1Img #-}
{-# NOINLINE bg2Img #-}
bg1Img, bg2Img :: Picture
bg1Img = fromMaybe fallbackBG $ unsafePerformIO $ loadFirst
  ["Assets/Background1.png"]
bg2Img = fromMaybe fallbackBG $ unsafePerformIO $ loadFirst
  ["Assets/Background2.png"]

-- Escalado (ajusta si tus PNG no son 1000x1000)
bgWidthPx, bgHeightPx :: Float
bgWidthPx  = 1000
bgHeightPx = 1000

drawSelectedBackground :: Int -> Picture
drawSelectedBackground i =
  let pic = if i == 1 then bg1Img else bg2Img
      sx  = 1000 / bgWidthPx
      sy  = 1000 / bgHeightPx
  in Scale sx sy pic

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
lightTankRedImg = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/light_tank_red.png")

{-# NOINLINE heavyTankRedImg #-}
heavyTankRedImg = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/heavy_tank_red.png")

{-# NOINLINE hunterTankRedImg #-}
hunterTankRedImg = fromMaybe fallbackSprite $ unsafePerformIO (loadJuicyPNG "Assets/hunter_tank_red.png")

-- Sprites de cañones
{-# NOINLINE cannonBlueImg #-}
cannonBlueImg = fromMaybe fallbackCannon $ unsafePerformIO (loadJuicyPNG "Assets/cannon_blue.png")

{-# NOINLINE cannonRedImg #-}
cannonRedImg = fromMaybe fallbackCannon $ unsafePerformIO (loadJuicyPNG "Assets/cannon_red.png")

-- Sprite de bala
{-# NOINLINE bulletImg #-}
bulletImg = fromMaybe fallbackBullet $ unsafePerformIO (loadJuicyPNG "Assets/bullet.png")

-- Sprites de explosiones
{-# NOINLINE explosionImpactImg #-}
explosionImpactImg = fromMaybe fallbackExplosion $ unsafePerformIO (loadJuicyPNG "Assets/explosion_impact.png")

{-# NOINLINE explosionDeathImg #-}
explosionDeathImg = fromMaybe fallbackExplosion $ unsafePerformIO (loadJuicyPNG "Assets/explosion_death.png")

-- Sprite de meteoritos
{-# NOINLINE meteoritoImg #-}
meteoritoImg :: Picture
meteoritoImg = fromMaybe fallbackMeteorito $ unsafePerformIO $ (loadJuicyPNG "Assets/asteroid.png")

{-# NOINLINE estelaImg #-}
estelaImg :: Picture
estelaImg = fromMaybe fallbackEstela $ unsafePerformIO $ (loadJuicyPNG "Assets/stele.png")

-- Sprite de bomba
{-# NOINLINE bombImg #-}
bombImg :: Picture
bombImg = fromMaybe (Color (greyN 0.6) (rectangleSolid 30 30)) $ unsafePerformIO $ loadJuicyPNG "Assets/bomb.png"

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
      (vx, vy) = velocidadCarro c  -- Obtiene velocidad
      angCanon = getdireccionCanon c
      (w, h)   = tamanoCarro c
      tipo     = tipoCarro c
      equipo   = team c
      -- Calcula ángulo desde la velocidad
      angCuerpo = if vx == 0 && vy == 0
                  then direccionCarro c + 180  -- Si está quieto, mantiene orientación
                  else let angRad = atan2 vy vx
                       in angRad * (180 / pi) + 180

      -- Sprite del cuerpo
      bodySprite = getTankBodySprite tipo equipo
      bodyScaleX = (w * sizeScale) / tankSpriteWidth
      bodyScaleY = (h * sizeScale) / tankSpriteHeight
      bodyPic = Translate sx sy $ Rotate angCuerpo $ Scale bodyScaleX bodyScaleY bodySprite
      -- Ahora rota según la dirección de movimiento real

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
      (vx, vy) = velocidadProyectil p
      -- Calcular el ángulo de movimiento desde la velocidad
      angRad = atan2 vy vx  -- atan2 calcula el ángulo del vector (vx, vy)
      angGrados = angRad * (180 / pi)  -- convertir radianes a grados
      scale = bulletSpriteSize / 10
  in Translate sx sy $ Rotate angGrados $ Scale scale scale bulletImg

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
-- Renderizado de obstáculos
-- =====================================================

drawMeteorito :: Mundo -> Meteorito -> Picture
drawMeteorito m met =
  let (sx, sy) = toScreen m (posicionMeteorito met)
      tam = tamanoMeteorito met
      rot = rotacionMeteorito met
      scale = tam / 450
  in Translate sx sy $ Rotate rot $ Scale scale scale meteoritoImg

drawEstela :: Mundo -> Estela -> Picture
drawEstela m e =
  let (sx, sy) = toScreen m (estelaPos e)
      radio = estelaRadio e
      vidaNorm = estelaVida e / 0.5
      alpha = vidaNorm * 0.7
      scale = radio / 450
  in Translate sx sy $ 
     Color (makeColor 1.0 1.0 1.0 alpha) $ 
     Scale scale scale estelaImg

-- Nuevo: dibujar bomba
drawBomb :: Mundo -> Bomba -> Picture
drawBomb m b =
  let (sx, sy) = toScreen m (posicionBomba b)
      radio = radioBomba b
      -- reducir escala para que la bomba sea más pequeña en pantalla
      scale = (radio * sizeScale) / 400.0
      -- Número a mostrar: 3 por defecto; si activa, tiempo restante redondeado hacia arriba
      tShown :: Int
      tShown = max 0 $ ceiling (if activaBomba b then tiempoBomba b else 3.0)
      txt = Translate (sx - 6) (sy - 6)
            $ Scale 0.12 0.12
            $ Color (if activaBomba b then yellow else white)
            $ Text (show tShown)
      sprite = Translate sx sy $ Scale scale scale bombImg
  in Pictures [sprite, txt]

-- =====================================================
-- Render principal
-- =====================================================

renderGame :: GameState -> IO Picture
renderGame gs = do
  case modo gs of
    Menu -> do
      -- Solo estrellas como fondo en el menú
      pure $ Pictures
        [ drawBackground 0
        , drawMenuWith (bgIndex gs)
        ]
    Victoria eq -> do
      let m = mundo gs
          fondoSel = drawSelectedBackground (bgIndex gs)
          vivos = filter (\c -> energia c > 0) (carros m)
          tanks = map (drawTank m) vivos
          bars  = map (drawHealthBar m) vivos
          projs = map (drawProjectile m) (proyectiles m)
          bombs = map (drawBomb m) (bombas m)   -- <-- mostrar bombas
          explosionPics = map (drawExplosion m) (explosions gs)
          mensaje = Translate (-260) 0 $ Scale 0.3 0.3 $ Color yellow $ Text ("Ha ganado el equipo " ++ show eq)
      pure $ Pictures ( [fondoSel] ++ bombs ++ tanks ++ bars ++ projs ++ explosionPics ++ [mensaje] )
    Jugando -> do
      let m = mundo gs
          fondoSel = drawSelectedBackground (bgIndex gs)
          vivos = filter (\c -> energia c > 0) (carros m)
          mets = map (drawMeteorito m) (obstaculos m)
          estelasR = concat [map (drawEstela m) (estelas met) | met <- obstaculos m]
          tanks = map (drawTank m) vivos
          bars  = map (drawHealthBar m) vivos
          projs = map (drawProjectile m) (proyectiles m)
          bombs = map (drawBomb m) (bombas m)   -- <-- mostrar bombas
          explosionPics = map (drawExplosion m) (explosions gs)
      pure $ Pictures ( [fondoSel] ++ estelasR ++ mets ++ bombs ++ tanks ++ bars ++ projs ++ explosionPics )

-- Pantalla de menú inicial
drawMenuWith :: Int -> Picture
drawMenuWith idx =
  Pictures
    [ Translate (-225) 100 $ Scale 0.8 0.8 $ Color white $ Text "SPACE WAR"
    , Translate (-200) 0 $ Scale 0.3 0.3 $ Color (greyN 0.8) $ Text "Tank Combat Simulator"
    , Translate (-180) (-100) $ Scale 0.25 0.25 $ Color yellow $ Text "Presiona ENTER para iniciar"
    , Translate (-180) (-180) $ Scale 0.15 0.15 $ Color (greyN 0.7) $ Text "Pulsa F para cambiar fondo:"
    , Translate (240)  (-180) $ Scale 0.15 0.15 $ Color (greyN 0.7) $ Text (if idx == 1 then "Nebulosa" else "Star-Wars")
    ]

-- =====================================================
-- Event handler
-- =====================================================

handleEvent :: Event -> GameState -> IO GameState
handleEvent _ gs = pure gs
