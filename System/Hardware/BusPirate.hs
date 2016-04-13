{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module System.Hardware.BusPirate
  ( -- * General
    BusPirateM
  , runBusPirate
    -- * I2C mode
  , runI2c
    -- * SPI mode
  , runSpi
  ) where

import System.Hardware.BusPirate.Core
import System.Hardware.BusPirate.I2C
import System.Hardware.BusPirate.SPI

-- | Convenient way to run an I2C action
runI2c :: FilePath -> I2cM a -> IO (Either String a)
runI2c path action = runBusPirate path $ i2cMode action

-- | Convenient way to run an I2C action
runSpi :: FilePath -> SpiM a -> IO (Either String a)
runSpi path action = runBusPirate path $ spiMode action
