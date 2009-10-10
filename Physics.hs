{-# OPTIONS -fglasgow-exts #-}
module Physics where
import Vector
import Data.IORef
import Graphics.Rendering.OpenGL
{-# OPTIONS -fglasgow-exts #-}
import Debug.Trace
import Integrators
import qualified Data.Map as M
import Types
import Data.List

traceS a = trace (show a) a


zero = Vector3 0 0 (0::GLdouble)

-- Calculate acceleration by adding gravity due to all planets + acceleration due to thrusters, if applicable
acceleration planets ks v s = 
  foldr (+) (Vector3 0 0 0) $ map acc planets
  where
    acc planet = 
      if ns == 0
        then Vector3 0 0 0
        else 
          baseAcc + (forwardThrust + backwardThrust) .* v_hat + (leftThrust + rightThrust) .* v_hat * (Vector3 0 0 1)
      where 
        forwardThrust = if find forward then 0.5 else 0
        backwardThrust = if find backward then -0.5 else 0
        leftThrust = if find left then -0.5 else 0
        rightThrust = if find right then 0.5 else 0

        baseAcc = (planetMass planet * ?g / ns) .* (normalizeV r)
        r = planetPos planet - s
        ns = normSq r
        v_hat = normalizeV v


        find k = M.findWithDefault False k ks
 

detectCollisions planets ships t = detectCollisions' ships t ([], [])
  where
    detectCollisions' [] t x = x
    detectCollisions' (s:ships) t (collisions,ships') = 
      case col of
        Nothing -> (collisions, s:ships')
        Just p -> ((traceS $ newCol (colLoc p s) (- shipVelocity s) (oldTime t)) : collisions, ships')
      where
        col = find (\p -> norm (planetPos p - shipPos s) < planetRadius p) planets
    colLoc planet ship = 
      if norm (dir k1_loc) < norm (dir k2_loc)
        then k1_loc
        else k2_loc
      where
        b = -(v_hat .*. p')
        det = sqrt ((v_hat .*. p') ** 2 + abs (planetRadius planet ** 2 - normSq p'))
        k1 = b + det
        k2 = b - det
        k1_loc = shipPos ship + k1 .* v_hat
        k2_loc = shipPos ship + k2 .* v_hat

        v_hat = normalizeV $ shipVelocity ship
        p' = shipPos ship - planetPos planet
        
        dir r =  planetPos planet - r - shipVelocity ship


updatePhysics world@(planets, collisions, ships) keyState t dt
  |accumulator t < dt = (world, t)
  |otherwise = 
    updatePhysics world' keyState (t{time = time t + dt, accumulator = accumulator t - dt}) dt
  where
    world' = (planets, collisions ++ collisions', map updateShipPos ships')
    updateShipPos = (updatePhysics' (rk4 $ acceleration planets keyState) (time t) dt)

    (collisions', ships') = detectCollisions planets ships t
    updatePhysics' integrator t dt ship@(Ship {shipPos = s, shipTrail = trail, shipVelocity = v}) =
      if (floor $ (2*t)) > (floor $ 2*(t - dt))
        then ship{shipPos = s', shipVelocity = v', shipTrail = s':trail}
        else ship{shipPos = s', shipVelocity = v', shipTrail = trail}
      where
        (v',s') = integrator v s dt

