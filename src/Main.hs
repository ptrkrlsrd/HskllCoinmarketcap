{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Concurrent ( threadDelay )
import System.Environment
import Data.Aeson
import Data.Text hiding (take, head, length)
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics


data Ticker = Ticker { 
  name :: String,
  symbol :: String,
  rank :: String,
  price_usd :: String,
  market_cap_usd :: String,
  available_supply :: String,
  total_supply :: String,
  percent_change_1h :: String,
  percent_change_24h :: String,
  percent_change_7d :: String,
  last_updated :: String
} deriving (Show,Generic)

instance FromJSON Ticker
instance ToJSON Ticker


newLine :: String
newLine = "\n"

jsonURL :: String -> String
jsonURL tickerName = "https://api.coinmarketcap.com/v1/ticker/" ++ tickerName

getJSON :: String -> IO B.ByteString
getJSON tickerName = simpleHttp (jsonURL tickerName)

printInfo :: Ticker -> String
printInfo x = 
  (name x) ++ "(" ++ (symbol x) ++ ") is at " ++ (price_usd x)  ++ newLine ++
  "Last Updated:      " ++ (last_updated x) ++ newLine ++
  "Total Supply:      " ++ (total_supply x) ++ newLine ++
  "Available Supply:  " ++ (available_supply x) ++ newLine ++
  "Market Cap:        " ++ (market_cap_usd x) ++ newLine ++
  "Rank: " ++ (rank x) ++ ". " ++ newLine ++ newLine ++
  "Percentage change" ++ newLine ++
  "\t1h:  "   ++ (percent_change_1h x) ++ "%" ++ newLine ++
  "\t24h: " ++ (percent_change_24h x) ++ "%" ++ newLine ++
  "\t7d:  " ++ (percent_change_7d x) ++ newLine

printSummary :: Ticker -> String
printSummary x = 
  (name x) ++ "(" ++ (symbol x) ++ ") is at " ++ (price_usd x)  ++ newLine ++
  "Available Supply:  " ++ (available_supply x) ++ newLine ++
  "Rank: " ++ (rank x) ++ ". " ++ newLine ++ newLine ++
  "Percentage change 1h: " ++ (percent_change_1h x) ++ "%" ++ newLine


runForeverWithDelay :: Double -> IO () -> IO ()
runForeverWithDelay period action = forever $ do
          action
          threadDelay (round $ period * 1000 * 1000)

fetchDataAndPrint :: String -> (Ticker -> String) -> IO ()
fetchDataAndPrint tickerName f = do
  d <- (eitherDecode <$> (getJSON tickerName))
  case d of
          Left err -> putStrLn err
          Right ps -> putStrLn (f (head ps))

main :: IO ()
main = do
  args <- getArgs
  case args of 
          []    -> do putStrLn "Please enter a TickerName (Ethereum, Bitcoin, Ripple, etc), and optionally a delay in seconds as the second argument"
          (x:[]) -> do (fetchDataAndPrint x printInfo)
          (x:y:[]) -> do runForeverWithDelay (read y :: Double) $ (fetchDataAndPrint x printSummary)
