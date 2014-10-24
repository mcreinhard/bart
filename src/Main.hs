import Vector
import Path
import Raphael
import Haste.Prim
import Haste.Foreign

testPath :: Anchored Path
testPath = Anchored (10,10) 2 [Line 5, Arc L 3 pi]

main :: IO ()
main = do
    width <- ffi $ toJSStr "window.innerWidth" :: IO Int
    height <- ffi $ toJSStr "window.innerHeight" :: IO Int
    paper <- raphael (0,0) (width,height)
    _ <- setViewBox (-2,-2) (64,42) paper
    _ <- path (pathD testPath) paper
    putStrLn $ pathD testPath
