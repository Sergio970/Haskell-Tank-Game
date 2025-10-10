-- Play
import Graphics.Gloss --Picture, Color, Translate
import Graphics.Gloss.Interface.Pure.Game --Play, Event, teclas/ratón...

-- Estado del juego: posición del círculo
data Mundo = Mundo { posX :: Float, posY :: Float }

main :: IO ()
main = play
  (InWindow "Interacción con Gloss" (800, 600) (100, 100)) -- ventana
  white         -- color de fondo
  60            -- frames por segundo
  (Mundo 0 0)   -- estado inicial
  dibujar       -- función de dibujo
  manejarEvento -- función de eventos (cómo reacciona el estado a un evento)
  actualizar    -- función de actualización(cómo evoluciona el estado con el paso del tiempo)

-- Dibuja el estado actual
dibujar :: Mundo -> Picture
dibujar m = Translate (posX m) (posY m) $
  Color red $ circleSolid 40

-- teclado: mover en pasos de 20 px cuando se pulsa (estado Down)
manejarEvento :: Event -> Mundo -> Mundo
manejarEvento (EventKey (Char 'a') Down _ _) m = m { posX = posX m - 20 }     -- mover izquierda
manejarEvento (EventKey (Char 'd') Down _ _) m = m { posX = posX m + 20 }     -- mover derecha
manejarEvento (EventKey (Char 'w') Down _ _) m = m { posY = posY m + 20 }     -- mover arriba
manejarEvento (EventKey (Char 's') Down _ _) m = m { posY = posY m - 20 }     -- mover abajo
-- ratón: click izquierdo → teletransporta el círculo a la posición del cursor
manejarEvento (EventKey (MouseButton LeftButton) Down _ (x, y)) _ = Mundo x y 
-- cualquier otro evento se ignora
manejarEvento _ m = m  

--devolvemos m sin cambios
actualizar :: Float -> Mundo -> Mundo
actualizar _ m = m
