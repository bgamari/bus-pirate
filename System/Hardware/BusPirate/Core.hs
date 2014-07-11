{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module System.Hardware.BusPirate.Core where

import Control.Applicative
import Control.Monad (when, replicateM_)
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

-- | Attempt to enter binary mode
initialize :: Handle -> EitherT String IO ()
initialize dev = do
    liftIO $ hFlush dev
    liftIO $ BS.hPut dev "\x00"
    a <- liftIO $ BS.hGetSome dev 5
    when (a /= "BBIO1")
      $ left "Invalid response during initialization"

-- | Run the given action until success up to n times
attempt :: Monad m => Int -> EitherT e m a -> EitherT e m a
attempt n action = go n
  where
    go 0 = action
    go n = do res <- lift $ runEitherT action
              case res of
                Right a -> return a
                Left _  -> go (n-1)

-- | Open a Bus Pirate device and run the given action
runBusPirate :: FilePath -> BusPirateM a -> IO (Either String a)
runBusPirate path (BPM action) = do
    dev <- liftIO $ SP.hOpenSerial path settings
    res <- runEitherT $ do
      attempt 20 (initialize dev)
      liftIO $ drainInput dev
      EitherT $ runReaderT (runEitherT action) dev
    replicateM_ 20 $ BS.hPut dev "\x00"
    BS.hPut dev "\x0f"
    hClose dev
    return res

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

data PeripheralConfig
    = PConfig { perPower      :: Bool
              , perPullups    :: Bool
              , perAux        :: Bool
              , perChipSelect :: Bool
              }
    deriving (Show)

setPeripherals :: PeripheralConfig -> BusPirateM ()
setPeripherals config = do
    command $ 0x40
            + bit 3 (perPower config)
            + bit 2 (perPullups config)
            + bit 1 (perAux config)
            + bit 0 (perChipSelect config)
  where
    bit n True = 2^n
    bit _ _    = 0
