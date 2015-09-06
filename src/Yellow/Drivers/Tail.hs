module Yellow.Drivers.Tail
(
  writeServoPositions,
  writeMotorSpeed,
  activateDropWeight,
  setGpsPower,
  setWhiteLedPower,
  setIrLedPower
)
where

import Data.ByteString as B
import System.RaspberryPi.GPIO
import Data.Word (Word8)
import Data.Int (Int8)

import Yellow.Drivers.Arduino

tailController :: Address
tailController = 0x0c

writeServoPositions :: (Int8, Int8, Int8) -> I2C ()
writeServoPositions (dorsal, port, starboard) = sendCommand tailController $ B.pack [0x42, fromIntegral dorsal, fromIntegral port, fromIntegral starboard]

writeMotorSpeed :: Int8 -> I2C ()
writeMotorSpeed speed = sendCommand tailController $ B.pack [0x41, fromIntegral speed]

activateDropWeight :: I2C ()
activateDropWeight = sendCommand tailController $ B.pack [0x63, 0xa5]

setGpsPower :: Bool -> I2C ()
setGpsPower on = do
                   let val = if on then 0x01 else 0x00
                   sendCommand tailController $ B.pack [0x81, val]

setWhiteLedPower :: Bool -> I2C ()
setWhiteLedPower on = do
                        let val = if on then 0x01 else 0x00
                        sendCommand tailController $ B.pack [0x61, val, 0x00]

setIrLedPower :: Bool -> I2C ()
setIrLedPower on = do
                     let val = if on then 0x01 else 0x00
                     sendCommand tailController $ B.pack [0x62, val, 0x00]
