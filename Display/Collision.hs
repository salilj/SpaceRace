module Display.Collision where
import Graphics.Rendering.OpenGL
import Types
import Vector

import Display.Utils


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

