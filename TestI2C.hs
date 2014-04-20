import System.Hardware.BusPirate
import qualified Data.ByteString as BS
import Data.Word
import Control.Monad.IO.Class

newtype I2CAddress = I2cAddr Word8

addr = I2cAddr 0xa6

readAddr :: I2CAddress -> Word8
readAddr (I2cAddr n) = n + 1

writeAddr :: I2CAddress -> Word8
writeAddr (I2cAddr n) = n + 1

readReg :: I2CAddress -> Word8 -> I2cM Word8
readReg addr reg = do
    startBit
    bulkWrite $ BS.pack [writeAddr addr, reg]
    startBit
    bulkWrite $ BS.pack [writeAddr addr]
    v <- readByte
    stopBit
    return v

writeReg :: I2CAddress -> Word8 -> Word8 -> I2cM ()
writeReg addr reg value = do
    startBit         
    bulkWrite $ BS.pack [writeAddr addr, reg, value]
    stopBit
    
main = do
    runBusPirate "/dev/ttyUSB0" $ i2cMode $ do
    setConfig $ I2cConfig { i2cPower = True
                          , i2cPullups = True
                          , i2cAux = False
                          , i2cChipSelect = False
                          }
    readReg addr 0x00 >>= liftIO . print
