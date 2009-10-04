{-# OPTIONS -fglasgow-exts #-}
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Bindings
import Display
import Types
import Data.IORef
import Vector
import qualified Data.Map as M

import Texture

r = Vector3 0.5 0 0
v = Vector3 0 0.577624445 0
--v = Vector3 0 0 0
--v = Vector3 0 0.75 0

initialWorld texture = 
  ([Planet {planetPos = Vector3 0 0 0, planetMass = 0.5, planetTexture = texture, planetRadius = 0.1}],
   [],
   [Ship {shipPos = r, shipVelocity = v, shipTrail = []} | i <- [1..2]])
--   Ship {shipPos = Vector3 0.5 0 0, shipV = Vector3 0 0.577624445 0, shipTrail = []}])

main = do
  (progname,texName:_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  createWindow "Hello World"
  windowSize $= Size 800 800
  reshapeCallback $= Just reshape

  initParams
  texture <- createTexture texName (True, True)


  timer <- initTimer
  world <- newIORef $ initialWorld texture
  keyState <- newIORef M.empty

  zoom <- newIORef (1 :: GLfloat)
  perWindowKeyRepeat $= PerWindowKeyRepeatOff
  keyboardMouseCallback $= Just (keyboardMouse keyState zoom)
  idleCallback $= Just (idle world keyState timer)

  fps <- initFPS
  displayCallback $= (display world fps zoom)


  mainLoop
  where
    ?g = 6.673 * (10**(-2)) * 5 :: GLdouble

