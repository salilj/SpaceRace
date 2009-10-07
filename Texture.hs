{-
  Taken from:
Copyright 2005 David Morra and Eric Etheridge

Email:  bitwise@umbrellahead.com

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
   3. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



---------------

The above copyright notice only applies to the Sprites and Main modules contained in this archive.  The ReadImage module has its own copyright.
-}

module Texture where
import Graphics.UI.GLUT
import ReadImage (readImage)
import Monad (when)
import Foreign (mallocBytes, copyArray)
import Data.Array.Storable
import Codec.Image.PNG


{-
This function sets the OpenGL environment up to handle translucent sprites.
This function makes use of some parts of HOpenGL which are changing, and so attempting to compile it with the latest libraries may not work.  We have marked up the parts which may become obsolete in the near future.
This function should be called after OpenGL has set up its window, but before drawing occurs.
-}

initParams :: IO ()  -- this function takes two Int's, the screen length and the screen height.  Note that these numbers do not need to corrospond to the window size in any way.
initParams = do
  blend $= Enabled
  blendEquation $= FuncAdd  -- this line is necessary to turn blending on.
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)  -- this defines how colors will be blended when they are drawn to the framebuffer.  This means that transparent colors will let the background show through and opaque colors will be drawn over it.
  textureFunction $= Replace  -- when textures are applied to our polygons, we would like for their color and alpha information to replace.
  texture Texture2D $= Enabled  -- enable 2D textures.
  clearColor $= Color4 0.0 0.0 0.0 0.0  -- we want the framebuffer's clear color to be black (we're really just doing this for the alpha information).
  color (Color4 0.0 0.0 0.0 (1.0 :: GLfloat))  -- we define the color that all of the quads will be before textures are applied.  We want it to have an alpha value of 1.0.

  shadeModel $= Smooth

  hint PerspectiveCorrection $= Nicest
  hint LineSmooth $= Nicest
  hint PolygonSmooth $= Nicest
  hint GenerateMipmap $= Nicest
  hint TextureCompression $= Nicest
  --ortho 0.0 (fromIntegral x) 0.0 (fromIntegral y) (-1.0) (1.0)  -- this is somewhat of a hack.  Think of it as defining how many pixels large the screen is.

readPNG filename = do
  f <- loadPNGFile filename
  case f of
    Left e -> error $ "Could not load: " ++ filename ++ " error: " ++ e
    Right img -> do
      let (w,h) = dimensions img
      let imgdata = imageData img
      (_,(x,y)) <- getBounds imgdata
      buf <- mallocBytes $ (x+1)*(y+1)
      withStorableArray (imageData img) (\ptr -> copyArray buf ptr $ (x+1)*(y+1))

      return (Size (fromIntegral w) (fromIntegral h), PixelData RGBA UnsignedByte buf)


{-
This function provides a way to load a raw image file into memory.  If the load fails, this function will return Nothing.
The readImage function called in this routine is part of a module called ReadImage which was written by Sven Panne.  The function has been slightly modified to read alpha data.
-}

createTexture :: FilePath -> (Bool, Bool) -> IO (Maybe TextureObject)  -- the user must pass a FilePath to the desired image and a Bool tuple which determines whether the texture is repeated in the x and y directions.
createTexture filename (repeatX, repeatY) = do
	[texName] <- genObjectNames 1  -- generate our texture.
	textureBinding Texture2D $= Just texName  -- make our new texture the current texture.
	when repeatX (textureWrapMode Texture2D S $= (Repeated, Repeat))  -- define wrapping along the x axis.
	when repeatY (textureWrapMode Texture2D T $= (Repeated, Repeat))  -- define wrapping along the y axis.
	textureFilter Texture2D $= ((Linear', Nothing), Linear')  -- ?  This is necessary, but I don't know what it does.

	((Size x y), pixels) <- case take 3 $ reverse filename of 
                                  "gnp" -> readPNG filename
                                  _ -> readImage filename  -- read our image into a PixelData structure.
	texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D x y) 0 pixels  -- associate our image with our new texture.  Since we are dealing with sprites, we do not wish to create mipmaps.
	return (Just texName)  -- return our (Maybe TextureObject) for later use.
