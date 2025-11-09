--Scale + Translate
import Graphics.Gloss

main :: IO ()
main =
  display (InWindow "Scale + Translate" (900, 400) (80, 80)) white dibujo

dibujo :: Picture
dibujo = Pictures
  [ -- círculo original (izquierda)
    Translate (-300) 0 $
      Color black $ circleSolid 50

    -- círculo doble de ancho que el original (centro)
  , Translate 0 0 $
      Scale 2 1 $
        Color red $ circleSolid 50

    -- círculo mitad de ancho y doble de alto (derecha)
  , Translate 300 0 $
      Scale 0.5 2 $
        Color blue $ circleSolid 50

  ]
