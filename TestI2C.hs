-- | Example demonstrating usage with ADXL345 accelerometer

import System.Hardware.BusPirate
import qualified Data.ByteString as BS
import Data.Word
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Applicative

newtype I2CAddress = I2cAddr Word8

addr = I2cAddr 0xa6

readAddr :: I2CAddress -> Word8
readAddr (I2cAddr n) = n + 1

writeAddr :: I2CAddress -> Word8
writeAddr (I2cAddr n) = n

readReg :: I2CAddress -> Word8 -> I2cM Word8
readReg addr reg = BS.head <$> readReg' addr reg 1

readReg' :: I2CAddress -> Word8 -> Int -> I2cM BS.ByteString
readReg' addr reg length = do
    startBit
    bulkWrite $ BS.pack [writeAddr addr, reg]
    writeRead (BS.singleton $ readAddr addr) length

writeReg :: I2CAddress -> Word8 -> Word8 -> I2cM ()
writeReg addr reg value = do
    startBit
    bulkWrite $ BS.pack [writeAddr addr, reg, value]
    stopBit

readAccel :: I2CAddress -> I2cM (Int, Int, Int)
readAccel addr = do
    (,,) <$> readPair 0x32 <*> readPair 0x34 <*> readPair 0x36
  where
    readPair base = do
      lsb <- readReg addr base
      msb <- readReg addr (base+1)
      return $ (fromIntegral msb) * 2^8 + fromIntegral lsb

main = do
    runBusPirate "/dev/ttyUSB0" $ i2cMode $ do
    setConfig $ I2cConfig { i2cPower = True
                          , i2cPullups = True
                          , i2cAux = False
                          , i2cChipSelect = False
                          }
    readReg addr 0x00 >>= liftIO . print
    writeReg addr 0x2d 0x08
    readReg addr 0x2d >>= liftIO . print
    forever $ do
      a <- readAccel addr
      liftIO $ print a
