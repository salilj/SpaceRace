module Display.Utils where
import Graphics.Rendering.OpenGL

import Foreign (malloc, free, Ptr)
import Foreign.Storable
import Types

qStyle = QuadricStyle (Just Smooth) GenerateTextureCoordinates Outside FillStyle

vector2vertex (Vector3 a b c) = Vertex3 a b c

-- Figure out z in object coords of a point in screen coords. Used for unProject
readPixelZ x y = do
  buf <- malloc :: IO (Ptr GLdouble)
  readPixels (Position x  y) (Size 1 1) (PixelData DepthComponent Double buf)
  p <- peek buf
  free buf
  return p

updateFPS fps = do
  t <- currentTime
  f <- get fps

  case (t - fpsTime f) > 1 of
    True -> do
      fps $= f{fpsFPS = (fromIntegral $ fpsFrames f)/ (t - fpsTime f), 
               fpsFrames = 0, fpsTime = t}
    False -> do
      fps $= f{fpsFrames = fpsFrames f + 1}
 

clearMatrices = do
  matrixMode $= Modelview 0
  loadIdentity
  matrixMode $= Projection
  loadIdentity
