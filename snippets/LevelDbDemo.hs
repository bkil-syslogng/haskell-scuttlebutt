{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Maybe
import           System.Environment           (getArgs)
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (release)
import           Data.ByteString.Char8        hiding (take)
import qualified Data.ByteString.Char8        as BS
import           Data.Default
import           Data.Monoid
import           Database.LevelDB
import qualified Database.LevelDB.Streaming   as S
import           Prelude                      hiding (putStrLn, putStr)

streamDb folder =
  runResourceT $ do
    db <- open folder
               defaultOptions{ createIfMissing = True
                             , cacheSize= 2048
                             }
    --get db def key >>= liftIO . print
    withIterator db def entries
  where
    entries iter = liftIO $ (S.toList (S.entrySlice iter S.AllKeys S.Asc) :: IO [(S.Key, S.Value)])

main :: IO ()
main = do
  maybeFolder <- listToMaybe <$> getArgs
  maybe (putStrLn "usage: leveldb-reader <leveldb folder>")
        (\folder -> do
          entries <- streamDb folder
          mapM_ (\(key, value) -> BS.putStrLn $ BS.concat [key, " | ", value]) entries
          )
        maybeFolder

