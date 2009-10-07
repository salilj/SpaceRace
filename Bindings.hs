module Bindings where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import qualified Data.Map as M

keyboardAct keyState zoom k@(SpecialKey _) Up = do
  ks <- get keyState
  keyState $= M.insert k False ks
keyboardAct keyState zoom k@(SpecialKey _) Down = do
  ks <- get keyState
  keyState $= M.insert k True ks
keyboardAct keyState zoom k@(MouseButton WheelUp) Down = do
  z <- get zoom
  zoom $= z + 0.1
keyboardAct keyState zoom k@(MouseButton WheelDown) Down = do
  z <- get zoom
  zoom $= z - 0.1
keyboardAct _ _ k _ = print (show k) >> return ()

keyboardMouse keyState zoom key state modifiers position = do
  keyboardAct keyState zoom key state

