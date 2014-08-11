{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time
import Data.Time.Format
import Data.Tree.NTree.TypeDefs
import System.Locale
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Web.Scotty

data Rate = Rate UTCTime Float
data Currency = EUR | USD
data Entity = Entity Currency [Rate]

instance ToJSON Currency where
    toJSON (t) = case t of USD -> "usd"
                           EUR -> "eur"

instance ToJSON Rate where
    toJSON (Rate date value) =
        object ["date" .= date, "value" .= value]

instance ToJSON Entity where
    toJSON (Entity currency rates) =
        object ["currency" .= currency, "rates" .= rates]


-- отримання курсів за останні 90 діб для обраної валюти
getRates :: Currency -> IO [Rate]
getRates currency = do
    -- отримання та парсинг сторінки з курсами нацбанку
    url <- compileUrl currency
    numbers <- runX $ (fromUrl url) >>> css "table#results0 td.cell_c" 
    return $ toRates (map clean numbers)
    where
        -- екcтрактимо внутрішній текст тега
        clean d' = d where NTree _ ((NTree (XText d) _):_) = d'

        -- перетворюємо одномірний масив на массив курсів валют
        toRates xs = [ rate (take 4 xs) ] ++ rest
            where
                rate (a:_:_:d:[]) = Rate (parse a) (read d :: Float)
                    where parse t = readTime defaultTimeLocale "%d.%m.%Y" t :: UTCTime
                rest = if length dropped >= 4 then toRates dropped else []
                    where dropped = drop 4 xs 


-- форматування адреси для забору інформації для обраної валюти за останні 90 діб
compileUrl :: Currency -> IO String
compileUrl currency = do
    currentDay <- fmap utctDay getCurrentTime
    let formatDay = formatTime defaultTimeLocale "%d.%m.%Y"
        today = formatDay currentDay
        past = formatDay $ addDays (-90) currentDay
        code = case currency of EUR -> 196
                                USD -> 169
    return $ filter (/='"') "http://www.bank.gov.ua/control/uk/curmetal/currency/search?" ++
      "formType=searchPeriodForm&time_step=daily&outer=table&" ++
      "periodStartTime=" ++ past ++ "&periodEndTime=" ++ today ++ "&" ++
      "currency=" ++ show code


main :: IO ()
main = do
    -- початковий збір даних
    exchangeRef <- fetchAndCombineExchangeRates >>= newIORef

    -- оновлення інформації раз на добу
    _ <- forkIO $ forever $ do
        threadDelay (1000000 * 24 * 60 * 60)
        fetchAndCombineExchangeRates >>= writeIORef exchangeRef

    scotty 3001 $
        get "/" $ do
            setHeader "Access-Control-Allow-Origin" "*"
            setHeader "Content-Type" "application/json; charset=utf-8"
            liftIO (readIORef exchangeRef) >>= raw . encode

    where fetchAndCombineExchangeRates = mapM fetch [USD, EUR]
          fetch c = do
            r <- getRates c
            return $ Entity c r
