import Path
import Raphael
import Fetch
import Haste.Prim
import Haste.Foreign
import Haste.Concurrent

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
    concurrent $ stationNames >>= liftIO . mapM_ print

