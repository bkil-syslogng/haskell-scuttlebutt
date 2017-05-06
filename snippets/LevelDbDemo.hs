{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Maybe
import           System.Environment           (getArgs)
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (release)
import           Data.ByteString.Char8        hiding (take)
import           Data.Default
import           Data.Monoid
import           Database.LevelDB
import qualified Database.LevelDB.Streaming   as S
import           Prelude                      hiding (putStrLn)

streamDb folder =
  runResourceT $ do
    db <- open folder
               defaultOptions{ createIfMissing = True
                             , cacheSize= 2048
                             }
    --get db def key >>= liftIO . print
    withIterator db def $ \iter -> dumpEntries iter
    return ()
  where
    dumpEntries iter = liftIO $
            S.toList (S.entrySlice iter S.AllKeys S.Asc)
        >>= print

main :: IO ()
main = do
  maybeFolder <- listToMaybe <$> getArgs
  maybe (putStrLn "usage: leveldb-reader <leveldb folder>")
        streamDb
        maybeFolder

