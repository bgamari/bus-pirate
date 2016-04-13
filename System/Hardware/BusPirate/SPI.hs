{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module System.Hardware.BusPirate.SPI
  ( -- * Types
    SpiM
  , spiMode
    -- * Bus operations
  , bulkTransfer
  , setCS
    -- * Configuration
  , PeripheralConfig(..)
  , setConfig
  , SpiSpeed(..)
  , setSpeed
  ) where

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Word

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import System.Hardware.BusPirate.Core

newtype SpiM a = SpiM (BusPirateM a)
               deriving (Functor, Applicative, Monad, MonadIO)

err :: String -> SpiM a
err = SpiM . BPM . throwE

-- | Enter I2C mode and run given action
spiMode :: SpiM a -> BusPirateM a
spiMode (SpiM m) = commandExpect 0x1 "SPI1" >> m

-- | Set chip select
setCS :: Bool -> SpiM ()
setCS True = SpiM $ command 0x21
setCS False = SpiM $ command 0x20

-- | Perform a read/write transaction of up to 16 bytes
bulkTransfer :: ByteString -> SpiM ByteString
bulkTransfer d
  | BS.null d = return BS.empty
  | BS.length d > 16 = err "Too many bytes"
  | otherwise = SpiM $ do
    command $ fromIntegral $ 0x10 + BS.length d - 1
    put d
    reply <- replicateM (BS.length d) $ getByte
    return $ BS.pack reply

-- | Set Bus Pirate peripheral configuration bits
setConfig :: PeripheralConfig -> SpiM ()
setConfig config = SpiM $ setPeripherals config

data SpiSpeed
    = Spi30KHz
    | Spi125KHz
    | Spi250KHz
    | Spi1MHz
    | Spi2MHz
    | Spi2_6MHz
    | Spi4MHz
    | Spi8MHz
    deriving (Show, Ord, Eq, Enum, Bounded)

-- | Set bus speed
setSpeed :: SpiSpeed -> SpiM ()
setSpeed speed = SpiM $ command $ 0x6 + fromIntegral (fromEnum speed)

-- | Write-then-read
--
-- This is very likely broken as the command structure itself appears
-- to be horribly broken, requiring a conditional read of a status byte.
writeRead :: Bool -> ByteString -> Word16 -> SpiM ByteString
writeRead toggleCS write readLen
  | readLen >= 4096 = err "Read length too long"
  | BS.length write >= 4096 = err "Write length too long"
  | otherwise = SpiM $ do
    putByte $ if toggleCS then 0x4 else 0x5
    putWord16 $ fromIntegral $ BS.length write
    putWord16 readLen
    status <- getByte
    case status of
      0 -> BPM $ throwE "Data too long"
      1 -> do put write
              get (fromIntegral readLen)
