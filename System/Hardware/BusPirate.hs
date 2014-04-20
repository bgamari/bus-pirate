{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
                
module System.Hardware.BusPirate where

import System.Hardware.Serialport as SP
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Control.Error
import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import System.IO
import Control.Applicative
import Data.Word
import Data.Char

newtype BusPirateM a = BPM (EitherT String (ReaderT Handle IO) a)
                     deriving (Functor, Applicative, Monad, MonadIO)

settings = defaultSerialSettings { commSpeed = CS115200 }

runBusPirate :: FilePath -> BusPirateM a -> IO (Either String a)
runBusPirate path (BPM action) = runEitherT $ do
    dev <- liftIO $ SP.hOpenSerial path settings
    let go 0 = return $ Left "Failed to enter binary mode"
        go n = do
          liftIO $ BS.hPut dev "\x00"
          a <- liftIO $ BS.hGetNonBlocking dev 5
          if a == "BBIO1"
            then return $ Right ()
            else go (n-1)
    go 20
    EitherT $ runReaderT (runEitherT action) dev

withDevice :: (Handle -> BusPirateM a) -> BusPirateM a
withDevice action = BPM (lift ask) >>= action

put :: ByteString -> BusPirateM ()
put bs = withDevice $ \dev->BPM $ liftIO $ BS.hPut dev bs
    
get :: Int -> BusPirateM ByteString
get n = withDevice $ \dev->BPM $ liftIO $ BS.hGet dev n

getByte :: BusPirateM Word8
getByte = do
    get 1
    return 1

commandExpect :: Word8 -> ByteString -> BusPirateM ()
commandExpect cmd reply = do
    put $ BS.pack [chr $ fromIntegral cmd]
    r <- get (BS.length reply)
    if r == reply
      then fail $ "Expected reply '"++BS.unpack reply++"', found '"++BS.unpack r++"'"
      else return ()
    
command :: Word8 -> BusPirateM ()
command cmd = commandExpect cmd "\x01"

--spiMode :: SpiM a BusPirateM a
--spiMode = commandExpect 0x01 "SPI1"

i2cMode :: I2cM a -> BusPirateM a
i2cMode (I2cM m) = commandExpect 0x2 "I2C1" >>  m

--uartMode :: UartM a -> BusPirateM a
--uartMode = commandExpect 0x3 "ART1"         
         
newtype I2cM a = I2cM (BusPirateM a)
               deriving (Functor, Applicative, Monad, MonadIO)
        
startBit :: I2cM ()
startBit = I2cM $ command 0x2

stopBit :: I2cM ()
stopBit = I2cM $ command 0x3

readByte :: I2cM Word8
readByte = I2cM $ put "\x04" >> getByte
         
ackBit :: I2cM ()
ackBit = I2cM $ command 0x6

nackBit :: I2cM ()
nackBit = I2cM $ command 0x7

{-        
bulkWrite :: ByteString -> I2cM ()
bulkWrite d 
  | BS.null d = return ()
  | BS.length d > 16 = I2cM $ BPM $ left "Too many bytes"
  | otherwise = I2cM $ do 
    command $ fromIntegral $ 0x10 + BS.length d - 1
    put d
    replicateM (BS.length d) $ get 1
-}

data I2cConfig = I2cConfig { i2cPower      :: Bool
                           , i2cPullups    :: Bool
                           , i2cAux        :: Bool
                           , i2cChipSelect :: Bool
                           }
               deriving (Show)
                           
setConfig :: I2cConfig -> I2cM ()
setConfig config = I2cM $ 
    command $ 0x40
            + bit 3 (i2cPower config)
            + bit 2 (i2cPullups config)
            + bit 1 (i2cAux config)
            + bit 0 (i2cChipSelect config)
  where
    bit n True = 2^n
    bit _ _    = 0

data I2cSpeed = I2c_5kHz
              | I2c_50kHz
              | I2c_100kHz
              | I2c_400kHz
              deriving (Show, Eq, Ord, Enum)
              
setSpeed :: I2cSpeed -> I2cM ()
setSpeed speed = I2cM $ command $ fromIntegral $ 0x60 + fromEnum speed
