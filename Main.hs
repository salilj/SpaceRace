{-# OPTIONS -fglasgow-exts #-}
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Bindings
import Display.Core
import Types
import Data.IORef
import Vector
import MapParser

import qualified Data.Map as M

import Texture


-- Setup window, graphics settings
initWindow = do
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  createWindow "Hello World"
  windowSize $= Size 600 600
  fullScreen
  reshapeCallback $= Just reshape

  initParams

  perWindowKeyRepeat $= PerWindowKeyRepeatOff


main = do
  (progname,mapName:_) <- getArgsAndInitialize
  initWindow

  gT <- createTexture "Textures/glass.png" (False, False)

  let ?glassTex = case gT of 
                    Nothing -> error "Could not load glass texture"
                    Just _ -> gT

  initialWorld <- readMap mapName
  world <- newIORef $ initialWorld

  -- init keyboard stuff
  keyState <- newIORef M.empty  -- stores currently pressed keyboard keys
  zoom <- newIORef (1 :: GLfloat)
  keyboardMouseCallback $= Just (keyboardMouse keyState zoom)

  screenInfo <- newIORef (0 :: V, 0 :: V)

  timer <- initTimer
  idleCallback $= Just (idle world keyState timer screenInfo)

  fps <- initFPS
  displayCallback $= (display world fps zoom screenInfo)

  mainLoop
  where
    ?g = 6.673 * (10**(-2)) * 5 :: GLdouble

