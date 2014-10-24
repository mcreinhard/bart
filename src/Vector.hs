module Vector where

type Vec2 a = (a, a)

(^+^) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(u1,u2) ^+^ (v1,v2) = (u1+v1,u2+v2)

(^*^) :: Num a => a -> Vec2 a -> Vec2 a
c ^*^ (v1,v2) = (c*v1,c*v2) 

negative :: Num a => Vec2 a -> Vec2 a
negative = (fromIntegral (-1) ^*^)

(^-^) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
u ^-^ v = u ^+^ negative v

magnitude :: Floating a => Vec2 a -> a
magnitude (v1,v2) = sqrt (v1*v1 + v2*v2)

normalize :: Floating a => Vec2 a -> Vec2 a
normalize v = (1 / magnitude v) ^*^ v

normal :: Num a => Vec2 a -> Vec2 a
normal (v1,v2) = (-v2,v1)

angle :: (Eq a, Ord a, Floating a) => Vec2 a -> a
angle (x,y)
    | x >  0 && y >= 0 = atan (y/x)
    | x == 0 && y >  0 = pi/2
    | x <  0          = atan (y/x) + pi
    | x == 0 && y <  0 = 3*pi/2
    | x >  0 && y <  0 = atan (y/x) + 2*pi
    | otherwise       = 0

rotate :: (Eq a, Ord a, Floating a) => a -> Vec2 a -> Vec2 a
rotate theta v = magnitude v ^*^ (cos absoluteAngle, sin absoluteAngle)
    where absoluteAngle = theta + angle v

zeroVec2 :: Num a => Vec2 a
zeroVec2 = (0,0)

