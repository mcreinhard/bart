{-# LANGUAGE OverloadedStrings #-}
module Fetch (bartApiFetch, stationNames, schedule) where

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

stationNames :: CIO [(String,String)]
stationNames = liftM (runKleisli selectNames) (bartApiFetch "stn" "stns" []) 
    where selectNames = parseXML
              >>> atTag "station" 
              >>> (atTag "abbr" >>> text) &&& (atTag "name" >>> text)

schedule :: CIO [Int]
schedule = liftM (runKleisli selectLongTrain) (bartApiFetch "sched" "routesched" [("route","1")])
    where selectLongTrain = parseXML
              >>> findOne (atTag "train" >>> filterBy hasAllOrigTimes)
              >>> getChildren
              >>> getAttr "origTime"
              >>^ timeToMinutes
          hasAllOrigTimes train = let stops = runKleisli getChildren train in
              all (hasAttr "origTime") stops

timeToMinutes :: String -> Int
timeToMinutes timeString = 60*hoursFromMidnight + read minutes
    where hours = takeWhile (/= ':') timeString
          minutes = takeWhile (/= ' ') . tail . dropWhile (/= ':') $ timeString
          ampm = tail . dropWhile (/= ' ') $ timeString
          hoursFromMidnight = (read hours `mod` 12)
                              + (if ampm == "PM" then 12 else 0)

