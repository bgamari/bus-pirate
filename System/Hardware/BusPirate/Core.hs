{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module System.Hardware.BusPirate.Core where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import System.IO
import Data.Word
import Control.Concurrent (threadDelay)

import System.Hardware.Serialport as SP

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

newtype BusPirateM a = BPM (EitherT String (ReaderT Handle IO) a)
                     deriving (Functor, Applicative, Monad, MonadIO)

withDevice :: (Handle -> BusPirateM a) -> BusPirateM a
withDevice action = BPM (lift ask) >>= action

settings = defaultSerialSettings { commSpeed = CS115200 }

drainInput :: Handle -> IO ()
drainInput h = do
    threadDelay 10
    a <- BS.hGetSome h 100
    when (not $ BS.null a) $ drainInput h

runBusPirate :: FilePath -> BusPirateM a -> IO (Either String a)
runBusPirate path (BPM action) = runEitherT $ do
    dev <- liftIO $ SP.hOpenSerial path settings
    let go 0 = return $ Left "Failed to enter binary mode"
        go n = do
          liftIO $ hFlush dev
          liftIO $ BS.hPut dev "\x00"
          a <- liftIO $ BS.hGetSome dev 5
          if a == "BBIO1"
            then return $ Right ()
            else go (n-1)
    go 20
    liftIO $ drainInput dev
    EitherT $ runReaderT (runEitherT action) dev

put :: ByteString -> BusPirateM ()
put bs = withDevice $ \dev->BPM $ liftIO $ BS.hPut dev bs

putByte :: Word8 -> BusPirateM ()
putByte b = withDevice $ \dev->BPM $ liftIO $ BS.hPut dev (BS.singleton b)

putWord16 :: Word16 -> BusPirateM ()
putWord16 b = do
    putByte $ fromIntegral $ b `div` 0x100
    putByte $ fromIntegral $ b
    
get :: Int -> BusPirateM ByteString
get n = withDevice $ \dev->BPM $ liftIO $ BS.hGet dev n

getByte :: BusPirateM Word8
getByte = do
    r <- get 1
    if BS.null r
      then fail $ "Failed to read byte"
      else return $ BS.head r

commandExpect :: Word8 -> ByteString -> BusPirateM ()
commandExpect cmd reply = do
    put $ BS.pack [fromIntegral cmd]
    r <- get (BS.length reply)
    if r /= reply
      then fail $ "Expected reply '"++show reply++"', found '"++show r++"'"
      else return ()
    
command :: Word8 -> BusPirateM ()
command cmd = commandExpect cmd "\x01"
