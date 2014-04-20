{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module System.Hardware.BusPirate.I2C where

import Control.Applicative
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Word

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import System.Hardware.BusPirate.Core

newtype I2cM a = I2cM (BusPirateM a)
               deriving (Functor, Applicative, Monad, MonadIO)
        
startBit :: I2cM ()
startBit = I2cM $ command 0x2

stopBit :: I2cM ()
stopBit = I2cM $ command 0x3

readByte :: I2cM Word8
readByte = I2cM $ putByte 0x4 >> getByte

data AckNack = Ack | Nack
             deriving (Show, Eq, Ord, Enum, Bounded)
         
ackBit :: I2cM ()
ackBit = I2cM $ command 0x6

nackBit :: I2cM ()
nackBit = I2cM $ command 0x7

bulkWrite :: ByteString -> I2cM ()
bulkWrite d 
  | BS.null d = return ()
  | BS.length d > 16 = I2cM $ BPM $ left "Too many bytes"
  | otherwise = I2cM $ do 
    command $ fromIntegral $ 0x10 + BS.length d - 1
    put d
    acks <- replicateM (BS.length d) $ toEnum . fromIntegral <$> getByte
    when (any (/= Ack) acks)
      $ fail "Nack during bulkWrite"

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
              deriving (Show, Eq, Ord, Enum, Bounded)
              
setSpeed :: I2cSpeed -> I2cM ()
setSpeed speed = I2cM $ command $ fromIntegral $ 0x60 + fromEnum speed

writeRead :: ByteString -> Int -> I2cM ByteString
writeRead send recv 
  | BS.length send > 0xffff = error "Too large send request"
  | recv > 0xffff           = error "Too large recieve request"
  | otherwise               = I2cM $ do
    putByte 0x8
    putWord16 $ fromIntegral $ BS.length send
    putWord16 $ fromIntegral $ recv
    put send
    status <- getByte
    case status of
      0x00 -> fail "writeRead: Failed"
      0x01 -> get recv
