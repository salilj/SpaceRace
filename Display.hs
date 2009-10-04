module Display where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import System.Random
import Data.Maybe
import Control.Monad

import Debug.Trace

import Types
import Physics
import Vector


qStyle = QuadricStyle (Just Smooth) GenerateTextureCoordinates Outside FillStyle
dt::GLdouble
dt = 10**(-2)


rotateV theta v@(Vector3 x y z) = Vector3 x' y' z
  where
    x' = x * cos theta - y * sin theta
    y' = y * cos theta + x * sin theta

renderCollision c = do
  t <- currentTime
  if t - collTime c > 2
    then return Nothing
    else renderCollision' t 0 >> return (Just c)
  where
    n = 6 :: GLdouble
    deltaT = 2/n :: GLdouble
    renderCollision' t 0 = do
      color $ Color3 1 1 (0::GLfloat)
      renderPrimitive Points $ vertex $ vector2vertex $ collPos c
      renderCollision' t 1
    renderCollision' t i
      |collTime c + deltaT * i < t = do
        --color $ Color3 scaleFactor scaleFactor 0
        renderC $ collVelocity c
        renderC $ rotateV 0.5 $ collVelocity c 
        renderC $ rotateV (-0.5) $ collVelocity c 
        renderCollision' t (i+1)
      |otherwise = return ()
      where
        renderC x = 
          renderPrimitive Points $ vertex $ vector2vertex $ collPos c + (i *  0.03 * deltaT) .* x
        scaleFactor = (1-i/n) :: GLdouble

      



renderShip colorFun ship = do
  color $ Color3 1 1 (1::GLfloat)
  preservingMatrix $ do
    translate $ shipPos ship
    renderObject Solid (Sphere' (0.01::GLdouble) 100 100)
    renderPrimitive Lines $ do
      vertex $ Vertex3 (0::GLdouble) 0 0
      vertex $ vector2vertex $ normalizeV $ shipVelocity ship
  let n = length $ shipTrail ship

  mapM_ (\(v, i) -> preservingMatrix $ do
      if i < 0.3 
        then color $ colorFun (0.3::GLfloat)
        else color $ colorFun i
      renderPrimitive Points $ do
        vertex $ vector2vertex v
      ) $ zip (shipTrail ship) ([1 - (fromIntegral i/ fromIntegral n) | i <- [1..n]] ::[GLfloat])

vector2vertex (Vector3 a b c) = Vertex3 a b c

pairs (a:b:l) = (a,b):(pairs (b:l))
pairs [b] = [(b,b)]
pairs [] = []

plotIdeal ideal theta delta
  |theta > 2 * pi = return ()
  |otherwise = do
    let r = ideal theta
    let r' = ideal (theta + delta)
    renderPrimitive Lines $ do
      vertex $ Vertex3 (r * cos theta) (r * sin theta) 0
      vertex $ Vertex3 (r' * cos (theta+delta)) (r' * sin (theta+delta)) 0
    plotIdeal ideal (theta+delta) delta


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

  --plotIdeal ideal 0 0.01
  rasterPos $ Vertex3 0 (-1) (0::GLfloat)

  mapM_ (\(o, (r,g,b)) -> renderShip (\c -> Color3 (c*r) (c*g) (c*b)) o) $ zip ships randomColors
  where
    vel s = normalizeV $ shipVelocity s
    randomColors = map normalizeV $ group3 $ map (fromRational . toRational) $ randomRs (0, 1::Float) (mkStdGen 1)
    group3 (a:b:c:l) = (a, b, c):(group3 l)


renderMiniMap world = preservingMatrix $ do
  (planets, _,ships) <- get world
  rasterPos $ Vertex2 10 (10::GLfloat)

  translate $ Vector3 100 (100::GLfloat) 0
  color $ Color3 1 1 (1::GLfloat)
  mapM_ (\p -> preservingMatrix $ do
    translate $ k .* planetPos p
    case planetTexture p of
      Nothing -> renderQuadric qStyle (Disk 0 5 5 5)
      Just tex -> do
        textureBinding Texture2D $= Just tex
        renderQuadric qStyle (Disk 0 5 10 10)
        textureBinding Texture2D $= Nothing
    ) planets

  color $ Color3 1 0 (0::GLfloat)
  mapM_ (\p -> preservingMatrix $ do
    translate $ k .* shipPos p
    renderQuadric qStyle (Disk 0 2 5 5)
    ) ships
  rasterPos $ Vertex2 (-100::GLfloat) (-90)
  case ships of
    s:_ -> renderString Helvetica10 ("Velocity : " ++ show (norm $ shipVelocity $ s))
    _ -> return ()
  where
    k = 50
  

display world fps zoom = do 
  clear [ColorBuffer]
  s@(Size w h) <- get windowSize

  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral w/ fromIntegral h) 1 500
  translate $ Vector3 0 0 (-5 :: GLfloat)
  rotate 15 $ Vector3 (1::GLfloat) 0 0
  get zoom >>= \z -> scale z z z
  renderScene world

  viewport $= (Position 0 0, Size 200 200)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (fromIntegral 200) 0 (fromIntegral 200)
  renderMiniMap world

  t <- currentTime
  f <- get fps

  case (t - fpsTime f) > 1 of
    True -> do
      fps $= f{fpsFPS = (fromIntegral $ fpsFrames f)/ (t - fpsTime f), 
               fpsFrames = 0, fpsTime = t}
    False -> do
      fps $= f{fpsFrames = fpsFrames f + 1}

  displayFPS
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
  t' <- updatePhysics world keyState (t{accumulator = acc t newTime, oldTime = newTime}) dt

  timer $= t'
  postRedisplay Nothing
  where
    acc t newT = accumulator t + newT - oldTime t

