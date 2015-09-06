{-# LANGUAGE BangPatterns #-}

module Main
where

import qualified Prelude as P
import Numeric.Units.Dimensional.DK.Prelude
import Data.ByteString as B
import System.RaspberryPi.GPIO
import Control.Concurrent
import Data.Bits
import Data.Word (Word8)

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

g = do
      setTailPower True
      threadDelay 100000
      P.putStrLn "Initializing pressure sensor."
      sens <- initializePressureSensor pressureSensor
      P.putStrLn . show $ sens

h = do
      P.putStrLn "Activate tail?"
      P.getLine
      setTailPower True
      P.putStrLn "Deactivate tail?"
      P.getLine
      setTailPower False
      h

data PressureSensor = PressureSensor {
                        address :: Address,
                        pressureSensitivity :: Int,
                        pressureOffset :: Int,
                        tempCoPressureSensitivity :: Int,
                        tempCoPressureOffset :: Int,
                        refTemperature :: Int,
                        tempCoTemperature :: Int
                      }
  deriving (Show)

initializePressureSensor :: Address -> IO (Maybe PressureSensor)
initializePressureSensor adr = do
                                 resetPressureSensor adr
                                 threadDelay 10000 -- 10 ms
                                 c1 <- readProm adr 0xa0
                                 c2 <- readProm adr 0xa2
                                 c3 <- readProm adr 0xa4
                                 c4 <- readProm adr 0xa6
                                 c5 <- readProm adr 0xa8
                                 c6 <- readProm adr 0xaa
                                 return . Just $ PressureSensor adr c1 c2 c3 c4 c5 c6

readProm :: Address -> Word8 -> IO Int
readProm adr reg = do
                     writeI2C adr (B.pack [reg])
                     res <- readI2C adr 2
                     P.putStrLn . show . B.unpack $ res
                     let [h, l] = fmap fromIntegral $ B.unpack res
                     return $ (h `shift` 8) .|. l

commandD1Conversion :: Address -> IO (Maybe ())
commandD1Conversion adr = do
                            writeI2C adr (B.pack [0x44]) -- OSR 1024
                            return $ Just ()

commandD2Conversion :: Address -> IO (Maybe ())
commandD2Conversion adr = do
                            writeI2C adr (B.pack [0x54]) -- OSR 1024
                            return $ Just ()


readPressureSensorADC :: Address -> IO (Maybe Int)
readPressureSensorADC adr = do
                             res <- writeReadI2C adr (B.pack [0x00]) 3
                             let [h, m, l] = fmap fromIntegral $ B.unpack res
                             return . Just $ (h `shift` 16) .|. (m `shift` 8) .|. l

resetPressureSensor :: Address -> IO (Maybe ())
resetPressureSensor adr = do
                            writeI2C adr (B.pack [0x1e])
                            return $ Just ()

setTailPower :: Bool -> IO (Maybe ())
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

sendCommand :: Address -> ByteString -> IO (Maybe ())
sendCommand adr msg = do
                        let msg' = addCrc msg
                        writeI2C adr msg'
                        return $ Just ()

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
