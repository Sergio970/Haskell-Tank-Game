-- Translate
import Graphics.Gloss

main :: IO ()
main =
  display (InWindow "Translate" (800, 600) (100, 100)) white dibujo

dibujo :: Picture
dibujo = Pictures                                                  --circleSolid 40 = círculo de 40cm de radio      
  [ Color red   $ circleSolid 40                                   -- El círculo rojo está en el centro (0,0)
  , Translate 200 100 $ Color blue $ circleSolid 40                -- El círculo azul se mueve 200 px a la derecha y 100 px hacia arriba
  , Translate (-150) (-120) $ Color green $ circleSolid 40         -- El círculo verde se mueve 150 px a la izquierda y 120 px hacia abajo
  ]
