-- | Example demonstrating usage with ADXL345 accelerometer

import System.Hardware.BusPirate
import System.Hardware.BusPirate.I2C
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Data.Int
import Data.List (intercalate)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Applicative

addr :: I2CAddress
addr = from8Bit 0xa6

readAccel :: I2CAddress -> I2cM (Int16, Int16, Int16)
readAccel addr = do
    d <- readReg' addr 0x32 (3*2)
    return $ flip runGet (LBS.fromStrict d) $ do
      (,,) <$> get <*> get <*> get
  where
    get = fromIntegral <$> getWord16be

main = do
    runI2c "/dev/ttyUSB0" $ do
    setSpeed I2c_100kHz
    setConfig $ PConfig { perPower = True
                        , perPullups = False
                        , perAux = False
                        , perChipSelect = False
                        }
    readReg addr 0x00 >>= liftIO . print
    writeReg addr 0x2d 0x08
    readReg addr 0x2d >>= liftIO . print
    forever $ do
      (x,y,z) <- readAccel addr
      liftIO $ putStrLn $ intercalate "\t" $ map show [x,y,z]
