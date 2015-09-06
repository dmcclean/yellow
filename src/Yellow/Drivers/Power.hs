module Yellow.Drivers.Power
(
  turnSystemOff,
  setNosePower,
  setTailPower,
  setPayloadPower
)
where

import Data.ByteString as B
import Data.Bits
import System.RaspberryPi.GPIO
import Data.Word (Word8)

import Yellow.Drivers.Arduino

powerController :: Address
powerController = 0x0b

turnSystemOff :: I2C ()
turnSystemOff = setPower 0 False

setNosePower :: Bool -> I2C ()
setNosePower = setPower 1

setTailPower :: Bool -> I2C ()
setTailPower = setPower 2

setPayloadPower :: Bool -> I2C ()
setPayloadPower = setPower 3

setPower :: Int -> Bool -> I2C ()
setPower b on = do
                  reg <- getPowerRegister
                  let new = setOrClearBit on reg b
                  sendCommand powerController $ B.pack [0x21, new]

setOrClearBit :: (Bits a) => Bool -> a -> Int -> a
setOrClearBit True = setBit
setOrClearBit False = clearBit

getPowerRegister :: I2C Word8
getPowerRegister = do
                     res <- performRead powerController 0x21 1
                     let [x] = B.unpack res
                     return x

