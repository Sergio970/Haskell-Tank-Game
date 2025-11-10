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
import qualified Estadisticas as E

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

-- Escalado
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

drawEstela :: Mundo -> Meteorito -> Estela -> Picture
drawEstela m met e =
  let (mx, my) = posicionMeteorito met  -- Posición del meteorito
      (vx, vy) = velocidadMeteorito met  -- Velocidad (dirección)
      
      -- Normalizar la dirección de movimiento
      speed = sqrt (vx*vx + vy*vy)
      (dirX, dirY) = if speed > 0.1
                     then (-vx / speed, -vy / speed)  -- Dirección OPUESTA
                     else (0, 0)
      
      -- Offset: distancia detrás del meteorito
      offsetDist = tamanoMeteorito met * 1.5
      
      -- Posición de la estela (detrás del meteorito)
      (ex, ey) = (mx + dirX * offsetDist, my + dirY * offsetDist)
      
      -- Convertir a coordenadas de pantalla
      (sx, sy) = toScreen m (ex, ey)

      flipX = if vx > 0 then (-1) else 1  -- Si vx > 0 (derecha), voltear
      
      radio = estelaRadio e
      vidaNorm = estelaVida e / 0.5
      alpha = vidaNorm * 0.7
      scaleX = radio / 450        -- Ancho
      scaleY = radio / 450 * 2.0  -- Alto (largo de la llama)
  in Translate sx sy $ 
     Scale flipX 1.0 $  -- voltear horizontalmente
     Color (makeColor 1.0 1.0 1.0 alpha) $ 
     Scale scaleX scaleY estelaImg


-- Dibujar Bombas
drawBomb :: Mundo -> Bomba -> Picture
drawBomb m b =
  let (sx, sy) = toScreen m (posicionBomba b)
      radio = radioBomba b
      scale = (radio * sizeScale) / 400.0
      t = tiempoBomba b

      colorBomba
        | not (activaBomba b) = makeColor 0.4 0.4 0.4 1.0  -- gris
        | t > 2.0             = makeColor 1.0 1.0 0.2 1.0  -- amarillo
        | t > 1.0             = makeColor 1.0 0.6 0.0 1.0  -- naranja
        | otherwise           = makeColor 1.0 0.1 0.1 1.0  -- rojo

      -- fondo de color dinámico debajo del sprite
      colorCircle = Translate sx sy $ Color colorBomba $ circleSolid (radio * 1.5)

      sprite = Translate sx sy $ Scale scale scale bombImg

      txt = if activaBomba b
            then Translate (sx - 6) (sy + 8)
               $ Scale 0.15 0.15
               $ Color white
               $ Text (show (ceiling t))
            else Blank
  in Pictures [colorCircle, sprite, txt]






-- dibujar planetas
{-# NOINLINE planet1Img #-}
planet1Img = fromMaybe fallbackSprite $ unsafePerformIO $ loadJuicyPNG "Assets/planet01.png"
planet2Img = fromMaybe fallbackSprite $ unsafePerformIO $ loadJuicyPNG "Assets/planet02.png"
planet3Img = fromMaybe fallbackSprite $ unsafePerformIO $ loadJuicyPNG "Assets/planet03.png"
planet4Img = fromMaybe fallbackSprite $ unsafePerformIO $ loadJuicyPNG "Assets/planet04.png"
planet5Img = fromMaybe fallbackSprite $ unsafePerformIO $ loadJuicyPNG "Assets/planet05.png"

-- ... planet2Img, planet3Img, etc.

getPlanetSprite :: Int -> Picture
getPlanetSprite 1 = planet1Img
getPlanetSprite 2 = planet2Img
getPlanetSprite 3 = planet3Img
getPlanetSprite 4 = planet4Img
getPlanetSprite 5 = planet5Img
getPlanetSprite _ = planet1Img

drawObstaculoEstatico :: Mundo -> ObstaculoEstatico -> Picture
drawObstaculoEstatico m obs =
  let (sx, sy) = toScreen m (posicionObstaculoEstatico obs)
      radio = tamanoObstaculoEstatico obs
      sprite = getPlanetSprite (tipoVisual obs)
      scale = radio / 800 
  in Translate sx sy $ Scale scale scale sprite

-- =====================================================
-- Render principal
-- =====================================================
textoBold :: String -> Float -> Color -> Picture
textoBold txt escala col = Pictures
  [ Translate dx dy $ Scale escala escala $ Color col $ Text txt
  | dx <- [-0.8, -0.4, 0, 0.4, 0.8]
  , dy <- [-0.8, -0.4, 0, 0.4, 0.8]
  ]

renderGame :: GameState E.EstadisticasBot E.EstadisticasTorneo -> IO Picture
renderGame gs = do
  putStrLn ("obstaculos=" ++ show (length (obstaculos (mundo gs))))
  case modo gs of

    Menu -> do
      pure $ Pictures
        [ drawBackground 0
        , drawMenuWith (bgIndex gs)
        ]

    Victoria eq -> do
      let m = mundo gs
          fondoSel = drawSelectedBackground (bgIndex gs)

          vivos = filter (\c -> energia c > 0) (carros m)

          obsEst   = map (drawObstaculoEstatico m) (obstaculosEstaticos m)
          mets     = map (drawMeteorito m) (obstaculos m)
          estelasR = concat [map (drawEstela m met) (estelas met) | met <- obstaculos m]

          tanks = map (drawTank m) vivos
          bars  = map (drawHealthBar m) vivos
          projs = map (drawProjectile m) (proyectiles m)
          bombs = map (drawBomb m) (bombas m)
          explosionPics = map (drawExplosion m) (explosions gs)

          tiempoEspera = ceiling (tiempoEsperaVictoria gs)

          mensaje = Translate (-240) 20 $
                    textoBold ("Ha ganado el equipo " ++ show eq) 0.3 yellow

          mensaje2 = Translate (-250) (-20) $
                     textoBold ("Próximo torneo en "
                                ++ show tiempoEspera
                                ++ " segundos") 0.25 yellow

          separador = Translate 0 (-60) $
                      Color (greyN 0.5) $ Line [(-300, 0), (300, 0)]

          instruccion1 = Translate (-200) (-100) $
                         textoBold "Presiona R para reiniciar" 0.22 white

          instruccion2 = Translate (-200) (-160) $
                         textoBold "Presiona P para volver al menú" 0.22 white

          rondaInfo = Translate (-200) (-220) $
                      Scale 0.15 0.15 $ Color (greyN 0.6) $
                      Text ("Ronda: " ++ show (ronda gs))

      pure $ Pictures
        ( [fondoSel]
       ++ obsEst
       ++ estelasR
       ++ mets
       ++ bombs
       ++ tanks
       ++ bars
       ++ projs
       ++ explosionPics
       ++ [mensaje, mensaje2, separador, instruccion1, instruccion2, rondaInfo]
        )


    Jugando -> do
      let m = mundo gs
          fondoSel = drawSelectedBackground (bgIndex gs)

          vivos = filter (\c -> energia c > 0) (carros m)

          obsEst   = map (drawObstaculoEstatico m) (obstaculosEstaticos m)
          mets     = map (drawMeteorito m) (obstaculos m)
          estelasR = concat [map (drawEstela m met) (estelas met) | met <- obstaculos m]

          tanks = map (drawTank m) vivos
          bars  = map (drawHealthBar m) vivos
          projs = map (drawProjectile m) (proyectiles m)
          bombs = map (drawBomb m) (bombas m)
          explosionPics = map (drawExplosion m) (explosions gs)
          statsPanel = drawStatsPanel m (ronda gs)

          hudTorneo = Translate (-350) 270 $
                      Scale 0.15 0.15 $
                      Color black $
                      Text ("Torneo " ++ show (actualTorneo gs))

      pure $ Pictures
        ( [hudTorneo, fondoSel]
       ++ obsEst
       ++ estelasR
       ++ mets
       ++ bombs
       ++ tanks
       ++ bars
       ++ projs
       ++ explosionPics
       ++ [statsPanel]
        )


    FinTorneos -> do
      pure $ Pictures
        [ Translate 0 0 $ Scale 0.3 0.3 $ Color blue $ Text "¡Torneos Completados!"
        , Translate 0 (-50) $ Scale 0.15 0.15 $ Color black $ Text "Presiona ESC para salir"
        ]


-- Pantalla de menú inicial
drawMenuWith :: Int -> Picture
drawMenuWith idx =
  Pictures
    [ Translate (-225) 100 $ Scale 0.8 0.8 $ Color white $ Text "SPACE WAR"
    , Translate (-200) 0 $ Scale 0.3 0.3 $ Color (greyN 0.8) $ Text "Tank Combat Simulator"
    , Translate (-180) (-100) $ Scale 0.25 0.25 $ Color yellow $ Text "Presiona ENTER para iniciar"
    , Translate (-180) (-180) $ Scale 0.15 0.15 $ Color (greyN 0.7) $ Text "Pulsa F para cambiar fondo:"
    , Translate (240)  (-180) $ Scale 0.15 0.15 $ Color (greyN 0.7) $ Text (if idx == 1 then "Nebulosa" else "Star-Wars")
    , Translate (-180) (-230) $ Scale 0.12 0.12 $ Color (greyN 0.6) $ Text "R: Reiniciar partida en cualquier momento"
    , Translate (-180) (-260) $ Scale 0.12 0.12 $ Color (greyN 0.6) $ Text "P: Pausar durante el juego"
    ]

drawStatsPanel :: Mundo -> Int -> Picture
drawStatsPanel m rondaActual =
  let vivos   = filter (\c -> energia c > 0) (carros m)
      equipo1 = filter (\c -> team c == 1) vivos
      equipo2 = filter (\c -> team c == 2) vivos

      contarTipo tipo cs = length $ filter (\c -> tipoCarro c == tipo) cs
      ligeros1 = contarTipo Ligero equipo1
      pesados1 = contarTipo Pesado  equipo1
      caza1    = contarTipo Cazacarros equipo1
      ligeros2 = contarTipo Ligero equipo2
      pesados2 = contarTipo Pesado  equipo2
      caza2    = contarTipo Cazacarros equipo2

      extraTop = 120 :: Float
      baseH    = 280 :: Float
      panelH   = baseH + extraTop
      baseCX   = -390 :: Float
      baseCY   = 180  :: Float
      panelCY  = baseCY + extraTop / 2

   
      shiftLeft = -145 :: Float   
  in Translate shiftLeft 0 $ Pictures
    [ -- Fondo principal
      Translate baseCX panelCY $ Color (makeColor 0 0 0 0.85) $ rectangleSolid 220 panelH

      -- Borde
    , Translate baseCX panelCY $ Color (makeColor 0.3 0.3 0.4 1.0) $ rectangleWire 220 panelH

      -- === CABECERA ===
    , Translate (-495) 310 $ Scale 0.12 0.12 $ Color white $ Text "BATALLA"
    , Translate (-495) 290 $ Scale 0.1  0.1  $ Color yellow $ Text ("Ronda: " ++ show rondaActual)
    , Translate (-390) 275 $ Color (makeColor 0.6 0.6 0.7 1.0) $ Line [(-105, 0), (105, 0)]

      -- === EQUIPO 1 (ROJO) ===
    , Translate (-390) 255 $ Color (makeColor 0.8 0.2 0.2 0.6) $ rectangleSolid 210 25
    , Translate (-495) 250 $ Scale 0.12 0.12 $ Color white $ Text "EQUIPO 1"
    , Translate (-495) 224 $ Scale 0.20 0.20 $ Color red   $ Text (show (length equipo1))
    , Translate (-455) 227 $ Scale 0.09 0.09 $ Color white $ Text "NAVES"
    , Translate (-390) 195 $ Color (makeColor 0.3 0.1 0.1 0.5) $ rectangleSolid 210 30
    , Translate (-495) 203 $ Scale 0.08  0.08  $ Color (makeColor 1.0 0.95 0.95 1.0) $ Text "Composicion:"
    , Translate (-495) 188 $ Scale 0.075 0.075 $ Color white $
        Text ("Ligeros: " ++ show ligeros1 ++ "  Pesados: " ++ show pesados1 ++ "  Cazacarros: " ++ show caza1)

    , Translate (-390) 170 $ Color (makeColor 0.7 0.7 0.7 1.0) $ Line [(-105, 0), (105, 0)]

      -- === EQUIPO 2 (AZUL) ===
    , Translate (-390) 150 $ Color (makeColor 0.2 0.4 0.8 0.6) $ rectangleSolid 210 25
    , Translate (-495) 145 $ Scale 0.12 0.12 $ Color white $ Text "EQUIPO 2"
    , Translate (-495) 119 $ Scale 0.20 0.20 $ Color blue  $ Text (show (length equipo2))
    , Translate (-455) 122 $ Scale 0.09 0.09 $ Color white $ Text "NAVES"
    , Translate (-390)  90 $ Color (makeColor 0.1 0.2 0.4 0.5) $ rectangleSolid 210 30
    , Translate (-495)  98 $ Scale 0.08  0.08  $ Color (makeColor 0.95 0.95 1.0 1.0) $ Text "Composicion:"
    , Translate (-495)  83 $ Scale 0.075 0.075 $ Color white $
        Text ("Ligeros: " ++ show ligeros2 ++ "  Pesados: " ++ show pesados2 ++ "  Cazacarros: " ++ show caza2)
    ]


