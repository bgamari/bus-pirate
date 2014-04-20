{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
                
module System.Hardware.BusPirate
  ( -- * General
    module System.Hardware.BusPirate.Core
    -- * I2C mode
  , i2cMode
  , module System.Hardware.BusPirate.I2C
  ) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Applicative

import System.Hardware.BusPirate.Core
import System.Hardware.BusPirate.I2C

--spiMode :: SpiM a BusPirateM a
--spiMode = commandExpect 0x01 "SPI1"

i2cMode :: I2cM a -> BusPirateM a
i2cMode (I2cM m) = commandExpect 0x2 "I2C1" >>  m

--uartMode :: UartM a -> BusPirateM a
--uartMode = commandExpect 0x3 "ART1"         
         
