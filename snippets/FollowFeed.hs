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
import           Control.Monad.IO.Class

type Msg = Ssb.Message Object


getMessage :: MonadResource m => DB -> ByteString -> m (Maybe Msg)
getMessage db messageId = do
  let messageKey = BS.concat ["\160\160\0", "p", messageId, "\0\0"]
  message <- get db Database.LevelDB.defaultReadOptions messageKey
  let lbsmessage = (LBS.fromStrict <$> message) :: Maybe LBS.ByteString
  return $ lbsmessage >>= decode


getFeed :: MonadResource m => DB -> ByteString -> m ([Msg])
getFeed db messageId = do
  maybeMessage <- getMessage db messageId
  case maybeMessage of
    Just msg -> (msg:) <$> (getFeed db $ pack $ fromJust $ previous msg)
    Nothing  -> return []


prettyPrint :: [Msg] -> IO ()
prettyPrint = mapM_ $ print . content


main :: IO ()
main =
  runResourceT $ do
    db <- open "/home/hptr/.ssb/db"
          Database.LevelDB.defaultOptions{ createIfMissing = True
                                         , cacheSize= 2048
                                         }
    args <- liftIO getArgs
    messages <- getFeed db $ pack $ Prelude.head args
    liftIO $ prettyPrint messages
