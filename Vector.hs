{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}
module Vector where
import Graphics.Rendering.OpenGL

class (Num v, Floating s) => Vector v s | v -> s where
  (.*) :: s -> v -> v
  (.*.) :: v -> v -> s
  normSq :: v -> s
  norm :: v -> s
  norm = sqrt . abs . normSq
  normalizeV :: v -> v

instance (Num a) => Num (Vector3 a) where
  (Vector3 a b c) + (Vector3 x y z) = Vector3 (a+x) (b+y) (c+z)
  (Vector3 a b c) - (Vector3 x y z) = Vector3 (a-x) (b-y) (c-z)
  negate (Vector3 x y z) = Vector3 (-x) (-y) (-z)

  (Vector3 a b c) * (Vector3 x y z) = Vector3 (b*z - c*y) (-a*z + c*x) (a*y - b*x)
 

instance (Floating s, Num (Vector3 s)) => Vector (Vector3 s) s where
  k .* (Vector3 x y z) = Vector3 (k*x) (k*y) (k*z)

  (Vector3 a b c) .*. (Vector3 x y z) = x*a + y*b + z*c
  
  normSq (Vector3 x y z) = x**2 + y**2 + z**2
  
  normalizeV v@(Vector3 x y z) = 
    if n == 0
      then v
      else Vector3 (x/n) (y/n) (z/n)
    where
      n = norm v

instance (Num a, Num b) => Num (a, b) where
  (a,b) + (x,y) = (a+x,b+y)
  (a,b) - (x,y) = (a-x,b-y)
  negate (a,b) = (-a,-b)

instance (Num a) => Num (a, a, a) where
  (a,b,c) + (x,y,z) = (a+x,b+y, c + z)
  (a,b,c) - (x,y,z) = (a-x,b-y,c-z)
  negate (a,b,c) = (-a,-b,-c)
  (a, b, c) * (x, y, z) = (b*z - c*y, -a*z + c*x, a*y - b*x)

instance (Floating s) => Vector (s,s,s) s where
  k .* (x, y, z) = (k*x ,k*y, k*z)

  (a, b, c) .*. (x, y, z) = x*a + y*b + z*c
  
  normSq (x,y,z) = x**2 + y**2 + z**2
  
  normalizeV v = 
    if n == 0
      then v
      else (1/n) .* v
    where
      n = norm v

instance (Floating s, Vector v s) => Vector (v, v) s where
  k .* (x, y) = (k .* x, k .* y)
  
  normSq (x,y) = normSq x + normSq y
  normalizeV (x,y) = (normalizeV x, normalizeV y)
