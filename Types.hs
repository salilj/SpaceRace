module Types where
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
import System.Posix.Clock

type V = Vector3 GLdouble

forward = SpecialKey KeyUp
backward = SpecialKey KeyDown
left = SpecialKey KeyLeft
right = SpecialKey KeyRight


data Planet = Planet {planetName :: String, planetPos :: V, planetTexture :: Maybe TextureObject, planetRadius :: GLdouble, planetMass :: GLdouble}
  deriving Show

data Ship = Ship {shipPos :: V, shipVelocity :: V, shipTrail :: [V]}
  deriving (Show, Read, Eq)

data Collision = Collision {collPos :: V, collVelocity :: V, collTime :: GLdouble} deriving Show
newCol p v t = Collision {collPos = p, collVelocity = v, collTime = t}

type World = ([Planet], [Collision], [Ship])

data Timer = Timer {oldTime :: GLdouble, time :: GLdouble, accumulator :: GLdouble}
initTimer = do
  ot <- currentTime
  newIORef $ Timer {oldTime = ot, time = 0, accumulator = 0}


data FPS = FPS {fpsFrames :: Int, fpsFPS :: GLdouble, fpsTime :: GLdouble}
initFPS = do
  t <- currentTime
  newIORef $ FPS {fpsFrames = 0, fpsFPS = 0, fpsTime = t}


currentTime :: IO GLdouble
currentTime = do
  t <- getTime Monotonic
  return $ (fromIntegral $ sec t) + ((fromIntegral $ nsec t) * (10 ** (-9)))
