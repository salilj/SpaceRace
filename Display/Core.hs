{-# OPTIONS -fglasgow-exts #-}
module Display.Core where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import System.Random
import Data.Maybe
import Control.Monad

import Display.Minimap
import Display.Utils
import Display.Collision

import Debug.Trace

import Types
import Physics
import Vector


timeStep::GLdouble
timeStep = 10**(-2)


renderShip colorFun ship = do
  color $ Color3 1 1 (1::GLfloat)
  preservingMatrix $ do
    translate $ shipPos ship
    renderObject Solid (Sphere' (0.01::GLdouble) 100 100)
  let n = length $ shipTrail ship

  mapM_ (\(v, i) -> preservingMatrix $ do
      if i < 0.3 
        then color $ colorFun (0.3::GLfloat)
        else color $ colorFun i
      renderPrimitive Points $ do
        vertex $ vector2vertex v
      ) $ zip (shipTrail ship) ([1 - (fromIntegral i/ fromIntegral n) | i <- [1..n]] ::[GLfloat])


renderPlanet p = preservingMatrix $ do
  translate $ planetPos p
  case planetTexture p of
    Nothing -> renderObject Solid (Sphere' (planetRadius p) 100 100)
    Just tex -> do
      textureBinding Texture2D $= Just tex
      renderQuadric qStyle $ Sphere (planetRadius p) 100 100
      textureBinding Texture2D $= Nothing



renderScene world = do
  clear [DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  (planets, collisions, ships) <- get world

  col' <- liftM catMaybes $ mapM renderCollision collisions
  world $= (planets, col', ships)

  color $ Color3 1 1 (1::GLfloat)
  mapM_ renderPlanet planets

  mapM_ (\(o, (r,g,b)) -> renderShip (\c -> Color3 (c*r) (c*g) (c*b)) o) $ zip ships randomColors
  where
    vel s = normalizeV $ shipVelocity s
    randomColors = map normalizeV $ group3 $ map (fromRational . toRational) $ randomRs (0, 1::Float) (mkStdGen 1)
    group3 (a:b:c:l) = (a, b, c):(group3 l)



-- may not be needed
reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral w/ fromIntegral h) 1 500


displayScene world fps zoom screenSize = do
  viewport $= (Position 0 0, s)
  clearMatrices
  perspective 45 (fromIntegral w/ fromIntegral h) 1 500
  translate $ Vector3 0 0 (-5 :: GLfloat)
  scaleScreen s

  get zoom >>= \z -> scale z z z

  renderScene world
  where
    scaleScreen s = do
      zval <- readPixelZ 0 0
      (proj :: GLmatrix GLdouble) <- liftM get matrix $ Just Projection
      mdl <- liftM get matrix $ Just $ Modelview 0
      Vertex3 x y z <- unProject (Vertex3 0 0 zval) proj mdl (Position 0 0, s)
      scale (0.845 / abs y) (0.845 / abs y) (0.845 / abs y) --the number is probably a function of the perspective transformation


display world fps zoom = do 
  clear [ColorBuffer]
  s@(Size w h) <- get windowSize

  displayScene world fps zoom s
  displayMiniMap (w `div` 5) (h `div` 5) world

  updateFPS fps
  swapBuffers
  where
    displayFPS = do
      f <- get fps
      color $ Color3 1 1 (1::GLfloat)
      rasterPos $ Vertex3 0 0 (0::GLfloat)
      renderString Helvetica10 $ "FPS : " ++ (show $ fpsFPS f)


idle world keyState timer = do
  t <- get timer
  newTime <- currentTime
  t' <- updatePhysics world keyState (t{accumulator = acc t newTime, oldTime = newTime}) timeStep

  timer $= t'
  postRedisplay Nothing
  where
    acc t newT = accumulator t + newT - oldTime t
