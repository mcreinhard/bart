{-# LANGUAGE Arrows #-}
module Fetch (stationNames) where

import Haste.Concurrent
import Control.Monad
import Control.Applicative
import Data.Maybe
import Text.XML.HXT.Core

baseUrl :: String
baseUrl = "http://api.bart.gov/api"

key :: String
key = "MW9S-E7SL-26DU-VV8V"

bartApiFetch :: String -> String -> [(Key,Val)] -> CIO String
bartApiFetch section cmd options = 
    fromMaybe (error "Failed API request") <$> ajaxRequest url allOptions
        where allOptions = ("cmd",cmd):("key",key):options
              url = baseUrl ++ "/" ++ section ++ ".aspx"

atTag :: String -> LA XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

text :: LA XmlTree String
text = getChildren >>> getText

stationNames :: CIO [(String,String)]
stationNames = liftM (runLA selectNames) (bartApiFetch "stn" "stns" [])
    where selectNames = xreadDoc >>> atTag "station" 
                                 >>> proc x -> do
                                       abbr <- text <<< atTag "abbr" -< x
                                       name <- text <<< atTag "name" -< x
                                       returnA -< (abbr,name)

