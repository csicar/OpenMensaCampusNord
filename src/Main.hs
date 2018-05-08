#!/bin/runhaskell
module Main where 

import System.Process
import Prelude hiding (lines)
import Data.Text (null, Text, empty, strip, pack, splitOn)
import Data.List (transpose)
import Data.Time.Calendar
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock (utctDay)
import Control.Monad    
import System.Directory
import System.Environment
import qualified Data.Text as T

-- size of one food field
mealInfoWidth = 114
mealInfoHeith = 50

-- size of one price field
priceWidth = 114
priceHeight = 10

-- offset of the upper left corner of the first food item
firstItemX = 110
firstItemY = 170

-- distance between to meals
deltaX = 147
deltaY = 75

-- offset of the upper left corner of the first price
firstPriceX = firstItemX
firstPriceY = 228

data Meal = Meal 
    { description :: String
    , price :: String
    } deriving Show

data Line = Line 
    { lineName :: String
    , meal :: Meal
    } deriving Show

data CanteenDay = CanteenDay 
    { dayDate :: Day
    , lines :: [Line]
    } deriving Show

data Canteen = Canteen  
    { startDate :: Day
    , endDate :: Day
    , days :: [CanteenDay]
    } deriving Show

canteenDays = ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"]
canteenLines = ["Grill Wok", "Tipp des Tages", "Vegetarisch Vegan", "Essen 1", "Pizza Pasta"]

parseDate :: Integer -> String -> Day
parseDate year s = fromGregorian (fromInteger year) month monthDay
    where
        parts = map T.unpack $ T.splitOn (T.pack ".") (T.pack s)
        monthDay = read $ parts !! 0
        month =  read $ parts !! 1

main = do
    print "delete file if exist"
    fileExists <- doesFileExist fileName
    when fileExists (removeFile fileName)
    print "download"
    callProcess "wget" ["http://www.aserv.kit.edu/downloads/Speiseplan_deutsch.pdf", "--prefer-family=IPv4"]
    print "convert to text"
    canteen <- extractCanteen
    writeFile "data.xml" (writeCanteen canteen)
    pushUrl <- getArgs >>= return . head
    callProcess "scp" ["data.xml", pushUrl]
    where
        fileName = "Speiseplan_deutsch.pdf"

readConvertedFile :: IO String
readConvertedFile = do
    file <- readFile "Speiseplan_deutsch.txt"
    return $! file

cleanTextUp :: String -> String
cleanTextUp = T.unpack . T.intercalate (T.pack " ") . T.lines . T.pack 

extractBox :: Int -> Int -> Int -> Int -> IO String
extractBox x y width height = do
    process <- callProcess "pdftotext" [
        "Speiseplan_deutsch.pdf", "-l", "1", "-enc", "UTF-8", "-nopgbrk",
        "-x", show x, 
        "-y", show y, 
        "-W", show width, 
        "-H", show height]
    rawText <- readConvertedFile
    return $ cleanTextUp rawText

extractMealDescr :: Int -> Int -> IO String
extractMealDescr dayIndex lineIndex = do
    extractBox 
        (firstItemX + dayIndex * deltaX)
        (firstItemY + lineIndex * deltaY)
        mealInfoWidth
        mealInfoHeith
    

extractLine :: Int -> IO [Line]
extractLine dayIndex = do
    sequence (zipWith f canteenLines [0..])
    where
        f :: String -> Int -> IO Line
        f lineName lineIndex = do
            meal <- extractMeal dayIndex lineIndex
            return Line {lineName=lineName, meal=meal}
            
extractDays :: Day -> Day -> IO [CanteenDay]
extractDays startDay endDay = do
    sequence (zipWith f canteenDays [0..])
    where
        f :: String -> Int -> IO CanteenDay
        f dayName dayIndex = do
            lines <- extractLine dayIndex
            return CanteenDay {dayDate=addDays (toInteger dayIndex) startDay, lines=lines}

extractPrice :: Int -> Int -> IO String
extractPrice dayIndex lineIndex = do
    extractBox
        (firstPriceX + dayIndex * deltaX)
        (firstPriceY + lineIndex * deltaY)
        priceWidth
        priceHeight

extractMeal :: Int -> Int -> IO Meal
extractMeal dayIndex lineIndex = do
    descr <- extractMealDescr dayIndex lineIndex
    price <- extractPrice dayIndex lineIndex
    return Meal {description=descr, price=price}

extractDates :: IO (String, String)
extractDates = do
    startDate <- extractBox startDateX y width height
    endDate <- extractBox endDateX y width height
    return (startDate, endDate)
    where
        y = 83
        height = 12
        width = 30
        startDateX = 371
        endDateX = 431

extractCanteen :: IO Canteen
extractCanteen = do
    (rawStartDate, rawEndDate) <- extractDates
    (year, _, _) <- getCurrentTime >>= return . toGregorian . utctDay
    let startDate = parseDate year rawStartDate
    let endDate = parseDate year rawEndDate
    days <- extractDays startDate endDate
    return Canteen {startDate= startDate, endDate= endDate, days= days}

writeCanteen :: Canteen -> String 
writeCanteen canteen = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n\
    \<openmensa version=\"2.1\" xmlns=\"http://openmensa.org/open-mensa-v2\"\n\
           \xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n\
           \xsi:schemaLocation=\"http://openmensa.org/open-mensa-v2 http://openmensa.org/open-mensa-v2.xsd\">\n\
    \<version>1</version>\n\
    \<canteen>\
        \" ++ (concat $ map writeDay (days canteen)) ++ "\
    \</canteen>\
    \</openmensa>"

writeDay :: CanteenDay -> String
writeDay day = 
    "<day date=\""++ (show $ dayDate day) ++"\">"
    ++ (concat $ map writeLine (lines day))
    ++ "</day>"

writeLine :: Line -> String
writeLine line = 
    "<category name=\""++ (lineName line) ++"\">"
    ++ writeMeal (meal line) ++
    "</category>"

writeMeal :: Meal -> String
writeMeal meal =
    "<meal><name>" ++ (description meal) ++ "</name></meal>"