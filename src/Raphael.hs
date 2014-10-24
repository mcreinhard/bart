module Raphael where

import Haste.Foreign
import Haste.Prim
import Data.List

type Paper = JSAny
type Element = JSAny

raphael :: (Int,Int) -> (Int,Int) -> IO Paper
raphael (x,y) (width,height) = ffi (toJSStr $ "Raphael(" ++ argString ++ ")")
    where argString = intercalate "," . map show $ [x,y,width,height]
    
setViewBox :: (Int,Int) -> (Int,Int) -> Paper -> IO Paper
setViewBox (x,y) (width,height) = 
    ffi (toJSStr $ "(function(paper){paper.setViewBox(" ++ argString ++ ")})")
        where argString = intercalate "," . map show $ [x,y,width,height]

path :: String -> Paper -> IO Element
path = ffi . toJSStr 
    $ "(function(pathString,paper){return paper.path(pathString);})"
