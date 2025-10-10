-- Rotate
import Graphics.Gloss

main :: IO ()
main =
  display (InWindow "Rotate" (800, 600) (100, 100)) white dibujo

dibujo :: Picture
dibujo = Pictures                                              -- rectangleWire 200 100 = 200 px de ancho y 100 px de alto         
  [ Color black $ rectangleWire 200 100                        -- El rectángulo negro no rota
  , Rotate 45 $ Color red $ rectangleWire 200 100              -- El rectángulo rojo se gira 45° en sentido antihorario
  , Rotate 90 $ Color blue $ rectangleWire 200 100             -- El restángulo azul se gira 90° y queda vertical
  ]
