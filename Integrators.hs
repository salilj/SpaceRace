module Integrators where
import Graphics.Rendering.OpenGL
import Vector
import Types

type Integrator = (V -> V -> V) -> V -> V -> GLdouble -> (V, V)

-- Most of these are useless. rk4 is good of course, but reorderedEuler is also OK. preScaledEuler is so bad that the implementation is probably incorrect

euler :: Integrator
euler acceleration v s dt = 
  (v + (dt .* a), s + (dt .* v))
  where
    a = acceleration v s

secondEuler :: Integrator
secondEuler acceleration v s dt = 
 (v + (dt .* a), s + (dt .* v) + ((0.5 * dt * dt) .* a))
  where
    a = acceleration v s

preScaledEuler :: Integrator
preScaledEuler acceleration v s dt = 
  (v', s + v')
  where 
    v' = v + (dt * dt) .* a
    a = acceleration v s

reorderedEuler :: Integrator
reorderedEuler acceleration v s dt = 
  (v', s + dt .* v')
  where 
    a = acceleration v s
    v' = v + dt .* a

rk4 :: Integrator
rk4 acceleration v s dt =
  (v,s) + ((dt/6) .* (k1 + (2 .* (k2 + k3)) + k4))
  where
    f (dv, ds) dt = (acceleration v' s', v')
      where
        (v',s') = (v + (dt .* dv), s + (dt .* ds))
    k1 = f (Vector3 0 0 0, Vector3 0 0 0) 0
    k2 = f k1 (dt/2)
    k3 = f k2 (dt/2)
    k4 = f k3 dt :: (Vector3 GLdouble, Vector3 GLdouble)
