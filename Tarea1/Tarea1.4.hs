-- Animate
import Graphics.Gloss

main :: IO ()
main = animate
  (InWindow "Animación con Gloss" (800, 600) (100, 100)) -- ventana
  white                                                  -- fondo
  frame                                                  -- función de animación

-- t: tiempo en segundos desde que inició
frame :: Float -> Picture
frame t = Pictures
  [ Color blue $ Translate (200 * cos t) (200 * sin t) $ circleSolid 30  -- El círculo se mueve en una trayectoria circular centrada en (0,0) con radio 200px.
  , Rotate (t * 45) $ Color red $ rectangleSolid 200 20                  -- El rectángulo rojo gira alrededor del centro, 
                                                                         -- haciendo una rotación completa cada 8 segundos (360° / 45° ≈ 8 s).
  , Translate (-300) (-200) $ Scale (1 + 0.5 * sin t) (1 + 0.5 * sin t) $
      Color green $ circleSolid 50                                       -- Círculo que crece hasta 150 % de su tamaño original y encoge
                                                                         -- hasta 50 % de su tamaño original
  ]
