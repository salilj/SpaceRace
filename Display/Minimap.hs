{-# OPTIONS -fglasgow-exts #-}
module Display.Minimap where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Types
import Vector

import Display.Utils

hudOffset = 30
hudOffset' = fromIntegral hudOffset

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


renderGlass w h = do
  textureBinding Texture2D $= ?glassTex
  renderPrimitive Quads $ do
    texCoord $ TexCoord2 0 (1::GLdouble)
    vertex $ Vertex2 0 (0::GLdouble)
    texCoord $ TexCoord2 0 (0::GLdouble)
    vertex $ Vertex2 0 (h+hudOffset')
    texCoord $ TexCoord2 1 (0::GLdouble)
    vertex $ Vertex2 w (h+hudOffset')
    texCoord $ TexCoord2 1 (1::GLdouble)
    vertex $ Vertex2 w 0
  textureBinding Texture2D $= Nothing

displayMiniMap w h world = do
  viewport $= (Position 0 0, Size w (h + hudOffset))
  clearMatrices
  ortho2D 0 w' 0 (h' + hudOffset')

  renderGlass w' h'

  translate $ Vector3 (w'/2) ((h' - 6)/2 + hudOffset'::GLdouble) 0
  scissor $= Just (Position 3 hudOffset, Size (w - 6) (h - 6))

  renderMiniMap (w' - 12) (h' - 6) world

  scissor $= Nothing

  (_,_,ships) <- get world
  color $ Color3 0 0 (0::GLfloat)
  case ships of
    [] -> return ()
    s:_ -> do
      rasterPos $ Vertex2 (-w'/2 + 3) (-h'/2 - 8)
      renderString Helvetica10 ("Velocity : " ++ show (norm $ shipVelocity $ s))
      rasterPos $ Vertex2 (-w'/2 + 3) (-h'/2 - 19)
      renderString Helvetica10 ("Fuel : " ++ (show $ shipFuel s))
  where
    w' = fromIntegral $ w
    h' = fromIntegral $ h

