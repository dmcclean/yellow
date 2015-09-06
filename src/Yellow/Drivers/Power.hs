module Yellow.Drivers.Power
(
  setTailPower,
  getPowerRegister
)
where

import Data.ByteString as B
import Data.Bits
import System.RaspberryPi.GPIO
import Data.Word (Word8)

import Yellow.Drivers.Arduino

powerController :: Address
powerController = 0x0b

-- should read current register and then do the modification
setTailPower :: Bool -> I2C ()
setTailPower True = sendCommand powerController  (B.pack [0x21, 0x01 .|. 0x02 .|. 0x04])
setTailPower False = sendCommand powerController (B.pack [0x21, 0x01 .|. 0x02])

getPowerRegister :: I2C Word8
getPowerRegister = do
                     res <- performRead powerController 0x21 1
                     let [x] = B.unpack res
                     return x

