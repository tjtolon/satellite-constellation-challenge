module Linear (V3, distanceToOrigo, vLength, minus) where

type V3 = (Double, Double, Double)

minus :: V3 -> V3 -> V3
minus (x0, y0, z0) (x1, y1, z1) = (x0 - x1, y0 - y1, z0 - z1)

plus :: V3 -> V3 -> V3
plus (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

scale :: Double -> V3 -> V3
scale v (x,y,z) = (v*x, v*y, v*z)

dot :: V3 -> V3 -> Double
dot (x0, y0, z0) (x1, y1, z1) = x0 * x1 + y0 * y1 + z0 * z1

normalize :: V3 -> V3
normalize v@(x, y, z) = (x / s, y / s, z / s)
  where s = vLength v

vectorProjection :: V3 -> V3 -> V3
vectorProjection a b =
  let b' = normalize b
  in a `dot` b' `scale` b'

scalarProjection :: V3 -> V3 -> Double
scalarProjection a b = dot a b / vLength b

vLength :: V3 -> Double
vLength (x, y, z) = sqrt (x * x + y * y  + z * z)

neg :: V3 -> V3
neg (x, y, z) = (-x, -y, -z)

-- | calculate the distance between origo and a line segment
--   Line segment is described by two points in a cartesian coordinate
distanceToOrigo :: V3 -> V3 -> Double
distanceToOrigo p p' =
  let pq = neg p
      segment = p' `minus` p
      proj = vectorProjection pq segment
      ql = scalarProjection pq segment / vLength segment
  in if ql < 0 || ql > 1 then min (vLength p) (vLength p')
     else vLength (p `plus`  proj)
