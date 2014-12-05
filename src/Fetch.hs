{-# LANGUAGE OverloadedStrings #-}
module Fetch (bartApiFetch, stationNames, schedule, timeToMinutes) where

import XMLArrow
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.Maybe
import Data.Text (splitOn, pack, unpack)
import Haste.Concurrent
import Text.XML.Light.Types

baseUrl :: String
baseUrl = "http://api.bart.gov/api"

key :: String
key = "MW9S-E7SL-26DU-VV8V"

bartApiFetch :: String -> String -> [(Key,Val)] -> CIO String
bartApiFetch section cmd options = 
    fromMaybe (error "Failed API request") <$> ajaxRequest url allOptions
        where allOptions = ("cmd",cmd):("key",key):options
              url = baseUrl ++ "/" ++ section ++ ".aspx"

stationNames :: CIO [(String,String)]
stationNames = liftM (runKleisli selectNames) (bartApiFetch "stn" "stns" []) 
    where selectNames = parseXML
                        >>> atTag "station" 
                        >>> (atTag "abbr" >>> text) &&& (atTag "name" >>> text)

schedule :: CIO [Content]
schedule = liftM (runKleisli selectLongTrain) (bartApiFetch "sched" "stnsched" [("orig","embr")])
    where selectLongTrain = parseXML -- TODO: implement this with XMLArrow
                            {->>> atTag "train"
                            >>> (getChildren >. all (not . null . runLA (hasAttr "origTime")))-}

timeToMinutes :: String -> Int
timeToMinutes timeString = 60*hoursFromMidnight + read minutes
    where [hours,minutes,ampm] = map unpack . splitOn " :" $ pack timeString
          hoursFromMidnight = (read hours `mod` 12)
                              + (if ampm == "PM" then 12 else 0)

