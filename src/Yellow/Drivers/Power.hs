module Yellow.Drivers.Power
(
  setTailPower
)
where

powerController :: Address
powerController = 0x0b

-- should read current register and then do the modification
setTailPower :: Bool -> I2C ()
setTailPower True = sendCommand powerController  (B.pack [0x21, 0x01 .|. 0x02 .|. 0x04])
setTailPower False = sendCommand powerController (B.pack [0x21, 0x01 .|. 0x02])
