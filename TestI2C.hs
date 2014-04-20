-- | Example demonstrating usage with ADXL345 accelerometer

import System.Hardware.BusPirate
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
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
    d <- readReg' addr 0x32 (3*2)
    return $ flip runGet (LBS.fromStrict d) $ do
      (,,) <$> get <*> get <*> get
  where
    get = fromIntegral <$> getWord16be

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
