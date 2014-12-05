import Haste.Concurrent
import Haste.Foreign
import Haste.Prim

import Fetch
import Path
import Raphael

testPath :: Anchored Path
testPath = Anchored (10,10) 2 [Line 5, Arc L 3 pi]

innerDimensions :: IO (Int,Int)
innerDimensions = do
    width <- ffi $ toJSStr "window.innerWidth"
    height <- ffi $ toJSStr "window.innerHeight"
    return (width,height)

main :: IO ()
main = do
    (width,height) <- innerDimensions
    paper <- raphael (0,0) (width,height)
    _ <- setViewBox (-2,-2) (64,42) paper
    _ <- path (pathD testPath) paper
    putStrLn $ pathD testPath
    concurrent $ do
        names <- stationNames
        liftIO $ print names
        sched <- stationDistancesForRoute 1
        liftIO $ print sched
        sched2 <- stationDistancesForRoute 2
        liftIO $ print sched2

