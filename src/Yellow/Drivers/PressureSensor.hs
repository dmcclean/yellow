module Yellow.Drivers.PressureSensor
(
	PressureSensor,
	initializePressureSensor,
	readPressureAndTemperature	
)
where

import qualified Prelude as P
import Data.ByteString as B
import Data.Bits
import Data.Word (Word8)
import Data.Int (Int64)
import System.RaspberryPi.GPIO
import Control.Concurrent
import Control.Monad.Except
import Numeric.Units.Dimensional.DK.Prelude
import Numeric.Units.Dimensional.DK.NonSI

import Yellow.Drivers.Arduino

data PressureSensor = PressureSensor {
                        address :: Address,
                        pressureSensitivity :: Int64,
                        pressureOffset :: Int64,
                        tempCoPressureSensitivity :: Int64,
                        tempCoPressureOffset :: Int64,
                        refTemperature :: Int64,
                        tempCoTemperature :: Int64
                      }
  deriving (Show)

initializePressureSensor :: Address -> I2C PressureSensor
initializePressureSensor adr = do
                                 resetPressureSensor adr
                                 lift $ threadDelay 10000 -- 10 ms
                                 c1 <- readProm adr 0xa2
                                 c2 <- readProm adr 0xa4
                                 c3 <- readProm adr 0xa6
                                 c4 <- readProm adr 0xa8
                                 c5 <- readProm adr 0xaa
                                 c6 <- readProm adr 0xac
                                 return $ PressureSensor adr c1 c2 c3 c4 c5 c6

readPressureAndTemperature :: PressureSensor -> I2C (Pressure Double, ThermodynamicTemperature Double)
readPressureAndTemperature s = do
                                 let adr = address s
                                 commandD1Conversion adr
                                 lift $ threadDelay 10000 -- 10 ms delay
                                 rawPres <- fmap fromIntegral $ readPressureSensorADC adr
                                 commandD2Conversion adr
                                 lift $ threadDelay 10000 -- 10 ms delay
                                 rawTemp <- fmap fromIntegral $ readPressureSensorADC adr
                                 let dt = rawTemp P.- (refTemperature s `shift` 8)
                                 let temp = 2000 P.+ (dt P.* tempCoTemperature s) `shiftR` 23
                                 let off = ((pressureOffset s) `shift` 16) P.+ ((tempCoPressureOffset s P.* dt) `shiftR` 7)
                                 let sens = ((pressureSensitivity s) `shift` 15) P.+ ((tempCoPressureSensitivity s P.* dt) `shiftR` 8)
                                 let p = (((rawPres P.* sens) `shiftR` 21) P.- off) `shiftR` 13
                                 let tempC = fromIntegral temp P.* 0.01 :: Double
                                 let pmbar = fromIntegral p P.* 0.1 :: Double
                                 return (pmbar *~ milli bar, tempC *~ degreeCelsius)

readProm :: Address -> Word8 -> I2C Int64
readProm adr reg = do
                     writeI2CE adr (B.pack [reg])
                     res <- readI2CE adr 2
                     lift $ P.putStrLn . show . B.unpack $ res
                     let [h, l] = fmap fromIntegral $ B.unpack res
                     return $ (h `shift` 8) .|. l

commandD1Conversion :: Address -> I2C ()
commandD1Conversion adr = writeI2CE adr (B.pack [0x44]) -- OSR 1024

commandD2Conversion :: Address -> I2C ()
commandD2Conversion adr = writeI2CE adr (B.pack [0x54]) -- OSR 1024

readPressureSensorADC :: Address -> I2C Int
readPressureSensorADC adr = do
                              writeI2CE adr (B.pack [0x00])
                              res <- readI2CE adr 3
                              let [h, m, l] = fmap fromIntegral $ B.unpack res
                              return $ (h `shift` 16) .|. (m `shift` 8) .|. l

resetPressureSensor :: Address -> I2C ()
resetPressureSensor adr = writeI2CE adr (B.pack [0x1e])
