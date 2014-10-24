module Path where

import Vector
import Control.Monad

data LeftRight = L | R
    deriving (Eq, Show, Read)

epsilon :: Floating a => a
epsilon = 0.0001

type PathVec = Vec2 Double
type Angle = Double

angleToVec :: Angle -> PathVec
angleToVec theta = rotate theta (1,0)

data Segment = Line { length    :: Double    }
             | Arc  { direction :: LeftRight
                    , radius    :: Double
                    , angle     :: Angle    }

type Path = [Segment]

data Anchored a = Anchored { startPoint     :: PathVec
                           , startAngle     :: Angle
                           , unanchor       :: a       }

instance Monad Anchored where
    return                   = Anchored (0,0) 0
    Anchored p theta x >>= f = Anchored (p ^+^ r) (theta + phi) y
        where Anchored q phi y = f x
              r = rotate theta q

parameterization :: Segment -> Double -> PathVec
parameterization (Line s       ) t = (t*s) ^*^ angleToVec 0
parameterization (Arc dir rad ang) t = 
  center ^+^ radiusVector where
     center       = (case dir of L -> -rad; R -> rad) ^*^ (0,1)
     radiusVector = rotate (case dir of L -> -t*ang; R -> t*ang) (negative center)

endAnchor :: Segment -> Anchored ()
endAnchor seg = Anchored endPoint endAngle ()
    where endPoint = parameterization seg 1
          endAngle = case seg of Line _        -> 0
                                 Arc dir _ ang -> case dir of L -> -ang; R -> ang

d :: Anchored Segment -> String
d aSeg = unwords $    ["M"]    ++ map show [x1,y1] 
                   ++ [letter] ++ map show components
    where seg                   = unanchor aSeg
          (x1,y1)               = startPoint aSeg
          letter                = case seg of Line {} -> "L"
                                              Arc  {} -> "A"
          Anchored (x2,y2) _ () = aSeg >>= endAnchor
          components            = case seg of 
              Line _          -> [x2,y2]
              Arc dir rad ang -> [rad,rad,0,largeArcFlag,sweepFlag,x2,y2]
                  where largeArcFlag = if ang <= pi then 0 else 1
                        sweepFlag    = case dir of L -> 0; R -> 1

arcLength :: Segment -> Double
arcLength (Line s     ) = s
arcLength (Arc _ rad a) = rad*a

pathTail :: Path -> Anchored Path
pathTail path = liftM (const $ tail path) $ endAnchor (head path)

pathD :: Anchored Path -> String
pathD (Anchored _ _ []) = ""
pathD aPath = d (liftM head aPath) ++ pathD (aPath >>= pathTail)


