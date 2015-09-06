{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NegativeLiterals #-}

module Main
where

import qualified Prelude as P
import Numeric.Units.Dimensional.DK.Prelude
import Numeric.Units.Dimensional.DK.NonSI
import Data.ByteString as B
import System.RaspberryPi.GPIO
import Control.Concurrent
import Data.Bits
import Data.Word (Word8)
import Data.Int (Int8, Int64)
import Control.Monad.Except

main = withGPIO . withI2C $ g

noseController :: Address
noseController = 0x0a

powerController :: Address
powerController = 0x0b

tailController :: Address
tailController = 0x0c

pressureSensor :: Address
pressureSensor = 0x77

imu :: Address
imu = 0x28

g :: IO ()
g = do
      runExceptT $ setTailPower True
      threadDelay 100000
      P.putStrLn "Initializing pressure sensor."
      Right sens <- runExceptT $ initializePressureSensor pressureSensor
      P.putStrLn . show $ sens
      runExceptT $ showPres sens
      runExceptT $ writeMotorSpeed 0
      jog 0 1

jog :: Int8 -> Int8 -> IO ()
jog 127    1    = jog 126 (-1)
jog (-128) (-1) = jog (-127) 1
jog pos inc = do
                P.putStr "Jogging to position "
                P.putStrLn . show $ pos
                runExceptT $ writeServoPositions (pos, pos, pos)
                threadDelay 10000 -- 10 ms
                jog (pos P.+ inc) inc

showPres :: PressureSensor -> I2C ()
showPres sens = do
                  pt <- readPressureAndTemperature sens
                  lift $ P.putStrLn . show $ pt

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

writeServoPositions :: (Int8, Int8, Int8) -> I2C ()
writeServoPositions (dorsal, port, starboard) = do
                                                  let msg = addCrc . B.pack $ [0x42, fromIntegral dorsal, fromIntegral port, fromIntegral starboard]
                                                  writeI2CE tailController msg

writeMotorSpeed :: Int8 -> I2C ()
writeMotorSpeed speed = do
                          let msg = addCrc . B.pack $ [0x41, fromIntegral speed]
                          writeI2CE tailController msg

activateDropWeight :: I2C ()
activateDropWeight = do
                       let msg = addCrc . B.pack $ [0x63, 0xa5]
                       writeI2CE tailController msg

setGpsPower :: Bool -> I2C ()
setGpsPower on = do
                   let val = if on then 0x01 else 0x00
                   let msg = addCrc . B.pack $ [0x81, val]
                   writeI2CE tailController msg

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

setTailPower :: Bool -> I2C ()
setTailPower True = sendCommand powerController  (B.pack [0x21, 0x01 .|. 0x02 .|. 0x04])
setTailPower False = sendCommand powerController (B.pack [0x21, 0x01 .|. 0x02])

readTemperature :: IO (Maybe (ThermodynamicTemperature Double))
readTemperature = do
                    bytes <- performRead 0x0a 0x86 4
                    return $ fmap parseTemperature bytes

readDepth :: IO (Maybe (Length Double))
readDepth = do
              bytes <- performRead 0x0a 0x85 4
              return $ fmap parseDepth bytes

parseSignedInt24 :: ByteString -> Int
parseSignedInt24 bs | isNegative = (255 `shift` 24) .|. hml
                    | otherwise  = hml
  where
    [l, m, h] = B.unpack bs
    isNegative = testBit h 7
    [l', m', h'] = fmap fromIntegral [l, m, h]
    hml = (h' `shift` 16) .|. (m' `shift` 8) .|. l'

parseTemperature :: ByteString -> ThermodynamicTemperature Double
parseTemperature = (*~ degreeCelsius) . (P.+ 273.15) . fromIntegral . parseSignedInt24

parseDepth :: ByteString -> Length Double
parseDepth = (*~ meter) . fromIntegral . parseSignedInt24

sendCommand :: Address -> ByteString -> I2C ()
sendCommand adr msg = do
                        let msg' = addCrc msg
                        writeI2CE adr msg'

performRead :: Address -> Word8 -> Int -> IO (Maybe ByteString)
performRead adr hdr n = do
                          let msg = addCrc . B.pack $ [hdr]
                          writeI2C adr msg
                          P.putStrLn . show . unpack $ msg
                          threadDelay 100000
                          res <- readI2C adr n
                          P.putStrLn . show . unpack $ res
                          if validateCrc res
                            then return . Just $ (B.take (B.length res P.- 1) res)
                            else return Nothing

validateCrc :: ByteString -> Bool
validateCrc bs | B.length bs < 2 = False
               | otherwise = B.last bs == crc8 (B.take (B.length bs P.- 1) bs)

addCrc :: ByteString -> ByteString
addCrc bs = snoc bs (crc8 bs)

crc8 :: ByteString -> Word8
crc8 = B.foldl crc8Byte 0

crc8Byte :: Word8 -> Word8 -> Word8
crc8Byte a b = go a b 8
  where
    go :: Word8 -> Word8 -> Int -> Word8
    go !x _ 0 = x
    go !x !y !bit = go x' y' (bit P.- 1)
      where
        x' | testBit (x `xor` y) 7 = (x `unsafeShiftL` 1) `xor` 0x07
           | otherwise             = x `unsafeShiftL` 1
        y' = y `unsafeShiftL` 1
