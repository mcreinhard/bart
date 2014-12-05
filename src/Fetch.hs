{-# LANGUAGE OverloadedStrings, Arrows #-}
module Fetch (bartApiFetch, stationNames, stationDistancesForRoute) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe

import Haste.Concurrent

import XMLArrow

baseUrl :: String
baseUrl = "http://api.bart.gov/api"

key :: String
key = "MW9S-E7SL-26DU-VV8V"

bartApiFetch :: String -> String -> [(Key,Val)] -> CIO String
bartApiFetch section cmd options = 
    fromMaybe (error "Failed API request") <$> ajaxRequest url allOptions
        where allOptions = ("cmd",cmd):("key",key):options
              url = baseUrl ++ "/" ++ section ++ ".aspx"

timeToMinutes :: String -> Int
timeToMinutes timeString = 60*hoursFromMidnight + read minutes
    where hours = takeWhile (/= ':') timeString
          minutes = takeWhile (/= ' ') . tail . dropWhile (/= ':') $ timeString
          ampm = tail . dropWhile (/= ' ') $ timeString
          hoursFromMidnight = (read hours `mod` 12)
                              + (if ampm == "PM" then 12 else 0)

-- XML parsing
stationNamesFromXML :: String -> [(String,String)]
stationNamesFromXML = runKleisli $ parseXML
    >>> atTag "station" 
    >>> (atTag "abbr" >>> text) &&& (atTag "name" >>> text)

stationDistancesFromXML :: String -> [(String,Int)]
stationDistancesFromXML = runKleisli $ proc xml -> do
        parsed <- parseXML -< xml
        longTrain <- findOne (filterBy hasAllOrigTimes <<< atTag "train") -< parsed
        firstStop <- findOne getChildren -< longTrain
        stops <- getChildren -< longTrain
        startTime <- timeToMinutes ^<< getAttr "origTime" -< firstStop
        times <- timeToMinutes ^<< getAttr "origTime" -< stops
        names <- getAttr "station" -< stops
        timesFromStart <- arr (uncurry (-)) -< (times,startTime)
        returnA -< (names,timesFromStart)
    where hasAllOrigTimes = all (hasAttr "origTime") . runKleisli getChildren 

-- IO code
stationNames :: CIO [(String,String)]
stationNames = liftM stationNamesFromXML $ bartApiFetch "stn" "stns" [] 

stationDistancesForRoute :: Int -> CIO [(String,Int)]
stationDistancesForRoute n = liftM stationDistancesFromXML
    $ bartApiFetch "sched" "routesched" [("route",show n)]

