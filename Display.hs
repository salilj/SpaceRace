{-# OPTIONS -fglasgow-exts #-}
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

import Foreign (malloc, free, Ptr)
import Foreign.Storable


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
    {-
    renderPrimitive Lines $ do
      vertex $ Vertex3 (0::GLdouble) 0 0
      vertex $ vector2vertex $ normalizeV $ shipVelocity ship
      -}
  let n = length $ shipTrail ship

  mapM_ (\(v, i) -> preservingMatrix $ do
      if i < 0.3 
        then color $ colorFun (0.3::GLfloat)
        else color $ colorFun i
      renderPrimitive Points $ do
        vertex $ vector2vertex v
      ) $ zip (shipTrail ship) ([1 - (fromIntegral i/ fromIntegral n) | i <- [1..n]] ::[GLfloat])

vector2vertex (Vector3 a b c) = Vertex3 a b c


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

hudOffset = 30

readPixelZ x y = do
  buf <- malloc :: IO (Ptr GLdouble)
  readPixels (Position x  y) (Size 1 1) (PixelData DepthComponent Double buf)
  p <- peek buf
  free buf
  return p

renderMiniMap w h world = preservingMatrix $ do
 
  color $ Color3 0 0 (0 :: GLfloat)
  renderPrimitive Quads $ do
    vertex $ Vertex2 (-w) (-h)
    vertex $ Vertex2 (-w) h
    vertex $ Vertex2 w h
    vertex $ Vertex2 w (-h)
  (planets, _,ships) <- get world
  let scalf = scaleFactor planets

  color $ Color3 1 1 (1::GLdouble)
  mapM_ (\p -> preservingMatrix $ do
    translate $ k .* planetPos p
    case planetTexture p of
      Nothing -> renderQuadric qStyle (Disk 0 5 5 5)
      Just tex -> do
        textureBinding Texture2D $= Just tex
        renderQuadric qStyle (Disk 0 (k * planetRadius p) 10 10)
        textureBinding Texture2D $= Nothing
    ) planets

  color $ Color3 1 0 (0::GLfloat)
  mapM_ (\p -> preservingMatrix $ do
    let Vector3 x y z = k .* shipPos p
    translate $ Vector3 (clamp x (-w/2) (w/2)) (clamp y (-h/2) (h/2)) z
    renderQuadric qStyle (Disk 0 2 5 5)
    ) ships
  where
    clamp a low high = if a < low then low else if a > high then high else a
    scaleFactor planets = if dif == 0 then 1 else 200/dif
      where
        dif = max (maxx - minx) (maxy - miny)
        max a b = if a > b then a else b
        min a b = if a < b then a else b
        (minx,miny,maxx,maxy) = foldr (\p (minx, miny, maxx,maxy) -> 
              let (Vector3 x y _) = planetPos p 
                  minx' = min x minx
                  miny' = min y miny
                  maxx' = max x maxx
                  maxy' = max y maxy
              in (minx', miny',maxx',maxy')) (0,0,0,0) planets
    k = h/2


setupMiniMap w h world = do
  viewport $= (Position 0 0, Size w (h + hudOffset))
  matrixMode $= Modelview 0
  loadIdentity
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 w' 0 (h' + hudOffset')

  textureBinding Texture2D $= ?glassTex
  renderPrimitive Quads $ do
    texCoord $ TexCoord2 0 (1::GLdouble)
    vertex $ Vertex2 0 (0::GLdouble)
    texCoord $ TexCoord2 0 (0::GLdouble)
    vertex $ Vertex2 0 (h'+hudOffset')
    texCoord $ TexCoord2 1 (0::GLdouble)
    vertex $ Vertex2 w' (h'+hudOffset')
    texCoord $ TexCoord2 1 (1::GLdouble)
    vertex $ Vertex2 w' 0
    
  textureBinding Texture2D $= Nothing

  translate $ Vector3 (w'/2) ((h' - 6)/2 + hudOffset'::GLdouble) 0
  scissor $= Just (Position 3 hudOffset, Size (w - 6) (h - 6))

  renderMiniMap (w' - 12) (h' - 6) world

  scissor $= Nothing

  (_,_,ships) <- get world
  case ships of
    [] -> return ()
    s:_ -> do
      rasterPos $ Vertex2 (-w'/2 + 3) (-h'/2 - 8)
      renderString Helvetica10 ("Velocity : " ++ show (norm $ shipVelocity $ s))
      rasterPos $ Vertex2 (-w'/2 + 3) (-h'/2 - 19)
      renderString Helvetica10 ("Fuel : " ++ (show $ shipFuel s))
  where
    hudOffset' = fromIntegral hudOffset
    w' = fromIntegral $ w
    h' = fromIntegral $ h

display world fps zoom = do 
  clear [ColorBuffer]
  s@(Size w h) <- get windowSize

  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral w/ fromIntegral h) 1 500
  translate $ Vector3 0 0 (-5 :: GLfloat)
  scaleScreen s

  get zoom >>= \z -> scale z z z

  renderScene world

  setupMiniMap (w `div` 5) (h `div` 5) world

  t <- currentTime
  f <- get fps

  case (t - fpsTime f) > 1 of
    True -> do
      fps $= f{fpsFPS = (fromIntegral $ fpsFrames f)/ (t - fpsTime f), 
               fpsFrames = 0, fpsTime = t}
    False -> do
      fps $= f{fpsFrames = fpsFrames f + 1}

  --displayFPS
  swapBuffers
  where
    scaleScreen s = do
      zval <- readPixelZ 0 0
      (proj :: GLmatrix GLdouble) <- liftM get matrix $ Just Projection
      mdl <- liftM get matrix $ Just $ Modelview 0
      Vertex3 x y z <- unProject (Vertex3 0 0 zval) proj mdl (Position 0 0, s)
      scale (0.845 / abs y) (0.845 / abs y) (0.845 / abs y) --figure out why this works

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

  {-
  renderPrimitive Points $ do
    vertex $ Vertex3 0 0 (0::GLfloat)
    vertex $ Vertex3 0 (-0.5) (0::GLfloat)
    vertex $ Vertex3 1 (-1) (0::GLfloat)
    vertex $ Vertex3 1 0 (0::GLfloat)
    -}
