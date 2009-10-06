{-# OPTIONS -fglasgow-exts #-}
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Bindings
import Display
import Types
import Data.IORef
import Vector
import MapParser

import qualified Data.Map as M

import Texture

r = Vector3 0.5 0 0
v = Vector3 0 0.577624445 0
--v = Vector3 0 0 0
--v = Vector3 0 0.75 0

{-
initialWorld texture = 
  ([Planet {planetName = "Earth", planetPos = Vector3 0 0 0, planetMass = 0.5, planetTexture = texture, planetRadius = 0.1}],
   [],
   [Ship {shipPos = r, shipVelocity = v, shipTrail = []} | i <- [1..2]])
-}
--   Ship {shipPos = Vector3 0.5 0 0, shipV = Vector3 0 0.577624445 0, shipTrail = []}])




-- Setup window, graphics settings
initWindow = do
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  createWindow "Hello World"
  windowSize $= Size 800 800
  reshapeCallback $= Just reshape

  initParams

  perWindowKeyRepeat $= PerWindowKeyRepeatOff


main = do
  (progname,mapName:_) <- getArgsAndInitialize
  initWindow

  initialWorld <- readMap mapName
  world <- newIORef $ initialWorld

  -- init keyboard stuff
  keyState <- newIORef M.empty  -- stores currently pressed keyboard keys
  zoom <- newIORef (1 :: GLfloat)
  keyboardMouseCallback $= Just (keyboardMouse keyState zoom)

  timer <- initTimer
  idleCallback $= Just (idle world keyState timer)

  fps <- initFPS
  displayCallback $= (display world fps zoom)

  mainLoop
  where
    ?g = 6.673 * (10**(-2)) * 5 :: GLdouble

