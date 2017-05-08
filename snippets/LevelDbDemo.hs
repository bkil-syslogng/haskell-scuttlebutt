{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           GHC.Generics
import           Ssb
import           Ssb.Message
import           Data.Maybe
import           System.Environment           (getArgs)
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (release)
import           Data.ByteString.Char8        hiding (take)
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Default
import           Data.Monoid
import           Database.LevelDB
import qualified Database.LevelDB.Streaming   as S
import           Prelude                      hiding (putStrLn, putStr)
import           Data.Aeson

streamDb folder =
  runResourceT $ do
    db <- open folder
               Database.LevelDB.defaultOptions{ createIfMissing = True
                                              , cacheSize= 2048
                                              }
    --get db def key >>= liftIO . print
    withIterator db def entries
  where
    entries iter = liftIO $ (S.toList (S.entrySlice iter S.AllKeys S.Asc) :: IO [(S.Key, S.Value)])

data Content = Content {root :: String} deriving (Generic, Eq, Show)
instance FromJSON Content

main :: IO ()
main = do
  maybeFolder <- listToMaybe <$> getArgs
  maybe (putStrLn "usage: leveldb-reader <leveldb folder>")
        (\folder -> do
          entries <- streamDb folder
          mapM_ (\(key, value) -> BS.putStrLn $ BS.concat [key, " | ", (prettyMessage value)]) entries
          )
        maybeFolder

    where prettyMessage :: ByteString -> ByteString
          prettyMessage msg = case ((decode $ LBS.fromStrict msg) :: Maybe (Ssb.Message Content)) of
                                Nothing -> BS.concat ["Invalid message: ", msg]
                                (Just decodedMessage) -> BS.concat ["Valid message: ", pack $ show decodedMessage]

