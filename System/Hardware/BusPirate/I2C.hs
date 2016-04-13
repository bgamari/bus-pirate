{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module System.Hardware.BusPirate.I2C
  ( -- * Types
    I2cM
  , i2cMode
    -- * Bus operations
  , startBit
  , stopBit
  , readByte
  , ackBit
  , nackBit
  , bulkWrite
    -- * Configuration
  , PeripheralConfig(..)
  , setConfig
  , I2cSpeed(..)
  , setSpeed
    -- * Device addresses
  , I2CAddress
  , from7Bit
  , from8Bit
  , readAddr
  , writeAddr
    -- * Register interface
  , writeReg
  , readReg
  , readReg'
  ) where

import Control.Applicative
import Control.Monad (replicateM, when, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Bits
import Data.Word
import Data.List (intercalate)

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import System.Hardware.BusPirate.Core

newtype I2cM a = I2cM (BusPirateM a)
               deriving (Functor, Applicative, Monad, MonadIO)

err :: String -> I2cM a
err = I2cM . BPM . throwE

-- | Enter I2C mode and run given action
i2cMode :: I2cM a -> BusPirateM a
i2cMode (I2cM m) = commandExpect 0x2 "I2C1" >> m

-- | Send a start bit
startBit :: I2cM ()
startBit = I2cM $ command 0x2

-- | Send a stop bit
stopBit :: I2cM ()
stopBit = I2cM $ command 0x3

-- | Read a byte
readByte :: I2cM Word8
readByte = I2cM $ putByte 0x4 >> getByte

-- | Send an ACK
ackBit :: I2cM ()
ackBit = I2cM $ command 0x6

-- | Send a NACK
nackBit :: I2cM ()
nackBit = I2cM $ command 0x7

data AckNack = Ack | Nack
             deriving (Show, Eq, Ord, Enum, Bounded)

-- | Write some bytes
bulkWrite :: ByteString -> I2cM ()
bulkWrite d
  | BS.null d = return ()
  | BS.length d > 16 = err "Too many bytes"
  | otherwise = I2cM $ do
    command $ fromIntegral $ 0x10 + BS.length d - 1
    put d
    acks <- replicateM (BS.length d) $ toEnum . fromIntegral <$> getByte
    case map fst $ filter (\(n,a)->a /= Ack) $ zip [0..] acks of
      []    -> return ()
      nacks -> let nacks' = intercalate ", " $ map show nacks
                   bytes = if length nacks > 1 then "bytes" else "byte"
               in fail $ "Nack after "++bytes++" "++nacks'++" during bulkWrite of "++show d


-- | Set Bus Pirate peripheral configuration bits
setConfig :: PeripheralConfig -> I2cM ()
setConfig config = I2cM $ setPeripherals config

-- | I2C bus speed
data I2cSpeed = I2c_5kHz
              | I2c_50kHz
              | I2c_100kHz
              | I2c_400kHz
              deriving (Show, Eq, Ord, Enum, Bounded)

-- | Set I2C bus speed
setSpeed :: I2cSpeed -> I2cM ()
setSpeed speed = I2cM $ command $ fromIntegral $ 0x60 + fromEnum speed

-- | Send Start bit, write some bytes, then read some bytes (ACKing
-- each until the last), then send a stop bit
--
-- This is very likely broken as the command structure itself appears
-- to be horribly broken, requiring a conditional read of a status byte.
writeRead :: ByteString -> Int -> I2cM ByteString
writeRead send recv
  | BS.length send > 0xffff = err "Too large send request"
  | recv > 0xffff           = err "Too large recieve request"
  | otherwise               = I2cM $ do
    putByte 0x8
    putWord16 $ fromIntegral $ BS.length send
    putWord16 $ fromIntegral $ recv
    put send
    status <- getByte
    case status of
      0x00 -> fail "writeRead: Failed"
      0x01 -> get recv

-- | An I2C address (shifted 7-bit)
newtype I2CAddress = I2cAddr Word8

-- | An I2C address from a unshifted 7-bit address
from7Bit :: Word8 -> I2CAddress
from7Bit = I2cAddr . (`shiftL` 1)

-- | An I2C address from a shifted 8-bit address (masking out the read/write bit)
from8Bit :: Word8 -> I2CAddress
from8Bit = I2cAddr . (`clearBit` 0)

readAddr :: I2CAddress -> Word8
readAddr (I2cAddr n) = n + 1

writeAddr :: I2CAddress -> Word8
writeAddr (I2cAddr n) = n

type Register = Word8

-- | Perform a read of the given length starting at the given register
readReg' :: I2CAddress -> Word8 -> Int -> I2cM BS.ByteString
readReg' addr reg length = do
    startBit
    bulkWrite $ BS.pack [writeAddr addr, reg]
    writeRead (BS.singleton $ readAddr addr) length

-- | Read the given register
readReg :: I2CAddress -> Word8 -> I2cM Word8
readReg addr reg = BS.head <$> readReg' addr reg 1

-- | Perform a write to the given register
writeReg :: I2CAddress -> Word8 -> Word8 -> I2cM ()
writeReg addr reg value = do
    void $ writeRead (BS.pack [writeAddr addr, reg, value]) 0
