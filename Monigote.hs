-- Monigote.hs
-- Práctica: Juego simple con Gloss
-- Movimiento horizontal (A/D o flechas), salto (W o flecha arriba),
-- física básica (gravedad), animación al caminar y volteo del sprite.

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- =========================
-- Configuración y constantes
-- =========================
windowWidth, windowHeight :: Int
windowWidth  = 800
windowHeight = 600

halfW, halfH :: Float
halfW = fromIntegral windowWidth / 2
halfH = fromIntegral windowHeight / 2

fps :: Int
fps = 60

groundY :: Float
groundY = -200   -- altura del suelo (coordenadas de Gloss: origen en centro)

gravity :: Float
gravity = -1500  -- aceleración gravitatoria (px / s^2)

jumpImpulse :: Float
jumpImpulse = 700  -- velocidad inicial al saltar (px / s)

moveSpeed :: Float
moveSpeed = 250  -- velocidad horizontal objetivo (px / s)

vxResponse :: Float
vxResponse = 12  -- cuanto "suaviza" velocidad horizontal (mayor = más rápido alcanzar target)

xLimit :: Float
xLimit = halfW - 20  -- límites laterales para el personaje

-- =========================
-- Estado del juego
-- =========================
data Facing = FaceL | FaceR deriving (Show, Eq)

data GameState = GameState
  { posX       :: Float    -- posición x
  , posY       :: Float    -- posición y
  , velX       :: Float    -- velocidad x
  , velY       :: Float    -- velocidad y
  , facing     :: Facing   -- orientación
  , onGround   :: Bool     -- si está en el suelo
  , leftDown   :: Bool     -- tecla izquierda presionada
  , rightDown  :: Bool     -- tecla derecha presionada
  , jumpDown   :: Bool     -- tecla salto presionada
  , animTimer  :: Float    -- temporizador para animación de caminar
  } deriving (Show)

initialState :: GameState
initialState = GameState
  { posX      = 0
  , posY      = groundY
  , velX      = 0
  , velY      = 0
  , facing    = FaceR
  , onGround  = True
  , leftDown  = False
  , rightDown = False
  , jumpDown  = False
  , animTimer = 0
  }

-- =========================
-- Dibujo del monigote
-- =========================
-- Dibujamos con primitivas: cabeza, cuerpo, brazos y piernas.
-- Para animar caminata usamos animTimer para mover piernas con sin().

drawGame :: GameState -> Picture
drawGame gs =
  Translate (posX gs) (posY gs + bodyYOffset) $       -- posicionar personaje en (x,y)
  applyFacing (facing gs) $                         -- voltear si mira a la izquierda
  Pictures [ shadow, cuerpo, cabeza, brazoIzq, brazoDer, piernaIzq, piernaDer ]
  where
    -- offsets y tamaños
    bodyYOffset = 40        -- dibujamos el personaje con su "suelo" en posY
    headR = 16
    bodyW = 24
    bodyH = 36
    armLen = 18
    legLen = 22

    -- animación de piernas: oscilación según animTimer
    t = animTimer gs
    step = if onGround gs then sin (t * 8) else 0  -- solo cuando en suelo se mueve
    legOffset = 6 * step
    armOffset = 4 * step

    -- piezas
    cabeza = Translate 0 (bodyH/2 + headR) $
             Color (makeColor 1 0.87 0.77 1) $ circleSolid headR

    cuerpo = Color (makeColor 0.2 0.6 0.9 1) $
             Translate 0 0 $ rectangleSolid bodyW bodyH

    brazoIzq = Translate (-bodyW/2 - 4) (bodyH/8) $
               Rotate (-10) $
               Translate 0 (armOffset) $
               rectangleSolid 6 armLen

    brazoDer = Translate (bodyW/2 + 4) (bodyH/8) $
               Rotate 10 $
               Translate 0 (-armOffset) $
               rectangleSolid 6 armLen

    piernaIzq = Translate (-6) (-bodyH/2 - legLen/2) $
                Translate 0 (legOffset) $
                rectangleSolid 8 legLen

    piernaDer = Translate (6) (-bodyH/2 - legLen/2) $
                Translate 0 (-legOffset) $
                rectangleSolid 8 legLen

    shadow = Color (makeColor 0 0 0 0.25) $
             Translate 0 (-bodyH/2 - legLen - 4) $
             Scale 1 0.3 $ circleSolid 30

    applyFacing FaceR pic = pic
    applyFacing FaceL pic = Scale (-1) 1 pic  -- reflejar horizontalmente

-- =========================
-- Manejador de eventos
-- =========================
handleEvent :: Event -> GameState -> GameState
handleEvent evt gs = case evt of

  EventKey (SpecialKey KeyLeft) Down _ _ ->
    gs { leftDown = True, facing = FaceL }

  EventKey (Char 'a') Down _ _ ->
    gs { leftDown = True, facing = FaceL }

  EventKey (SpecialKey KeyRight) Down _ _ ->
    gs { rightDown = True, facing = FaceR }

  EventKey (Char 'd') Down _ _ ->
    gs { rightDown = True, facing = FaceR }

  EventKey (SpecialKey KeyLeft) Up _ _ ->
    gs { leftDown = False }

  EventKey (Char 'a') Up _ _ ->
    gs { leftDown = False }

  EventKey (SpecialKey KeyRight) Up _ _ ->
    gs { rightDown = False }

  EventKey (Char 'd') Up _ _ ->
    gs { rightDown = False }

  -- Salto: sólo al pulsar (Down) y si está en suelo
  EventKey (SpecialKey KeyUp) Down _ _ ->
    gs { jumpDown = True }

  EventKey (SpecialKey KeyUp) Up _ _ ->
    gs { jumpDown = False }

  EventKey (Char 'w') Down _ _ ->
    gs { jumpDown = True }

  EventKey (Char 'w') Up _ _ ->
    gs { jumpDown = False }

  _ -> gs

-- =========================
-- Actualización por frame (física)
-- =========================
update :: Float -> GameState -> GameState
update dt gs =
  let
    -- decidir velocidad objetivo según teclas
    targetVX
      | leftDown gs && not (rightDown gs) = -moveSpeed
      | rightDown gs && not (leftDown gs) = moveSpeed
      | otherwise = 0

    -- suavizado de la velocidad horizontal (aprox. "inercia")
    newVx = velX gs + (targetVX - velX gs) * min 1 (vxResponse * dt)

    -- aplicar gravedad a velY
    newVy = velY gs + gravity * dt

    -- nueva posición provisional
    newX = posX gs + newVx * dt
    newY = posY gs + newVy * dt

    -- colisión con suelo y salto automático
    (finalY, finalVy, finalOnGround) =
      if newY <= groundY
        then (groundY, 
              if jumpDown gs then jumpImpulse else 0,  -- saltar si tecla presionada
              True)
        else (newY, newVy, False)

    -- limitar en X
    limitedX = max (-xLimit) (min xLimit newX)

    -- actualizar animTimer sólo cuando caminando y en suelo
    walking = finalOnGround && abs newVx > 20
    newAnim = if walking
                then animTimer gs + dt
                else 0

    -- actualizar facing según velocidad objetivo (si se está moviendo)
    newFacing = case compare targetVX 0 of
                  GT -> FaceR
                  LT -> FaceL
                  EQ -> facing gs
  in
    gs { posX = limitedX
       , posY = finalY
       , velX = newVx
       , velY = finalVy
       , onGround = finalOnGround
       , animTimer = newAnim
       , facing = newFacing
       }

-- =========================
-- Main: ventana y play
-- =========================
main :: IO ()
main = play
  (InWindow "Monigote - Practica Gloss" (windowWidth, windowHeight) (100,100))
  (makeColor 0.95 0.95 0.95 1)  -- color fondo
  fps
  initialState
  drawGame
  handleEvent
  update