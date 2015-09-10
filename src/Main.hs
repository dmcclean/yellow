{-# LANGUAGE BangPatterns #-} {-# LANGUAGE NegativeLiterals #-}

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

import Yellow.Drivers.Power
import Yellow.Drivers.Tail
import Yellow.Drivers.PressureSensor
import Yellow.Drivers.Arduino

main = withGPIO . withI2C $ f -- g

noseController :: Address
noseController = 0x0a

pressureSensor :: Address
pressureSensor = 0x77

imu :: Address
imu = 0x28

f :: IO ()
f = do
      d <- runExceptT $ readDepth
      P.putStrLn . show $ d
      t <- runExceptT $ readTemperature
      P.putStrLn . show $ t
      a <- runExceptT $ readAltimeter
      P.putStrLn . show $ a
      threadDelay 1000000
      f

g :: IO ()
g = do
      runExceptT $ setTailPower True
      runExceptT $ setPayloadPower False
      P.putStrLn "Initializing pressure sensor."
      Right sens <- runExceptT $ initializePressureSensor pressureSensor
      P.putStrLn . show $ sens
      runExceptT $ setGpsPower True
      runExceptT $ showPres sens
      runExceptT $ writeMotorSpeed 0
      jog 0 1

jog :: Int8 -> Int8 -> IO ()
jog 127    1    = do
                    runExceptT $ setWhiteLedPower True
                    jog 126 (-1)
jog (-128) (-1) = do
                    -- runExceptT $ setWhiteLedPower False
                    jog (-127) 1
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

readTemperature :: I2C (ThermodynamicTemperature Double)
readTemperature = do
                    bytes <- performRead 0x0a 0x87 3
                    return $ parseTemperature bytes

readDepth :: I2C (Length Double)
readDepth = do
              bytes <- performRead 0x0a 0x86 3
              return $ parseDepth bytes

readAltimeter :: I2C (Length Double)
readAltimeter = do
                  bytes <- performRead 0x0a 0x89 2
                  return $ parseAltimeter bytes

parseSignedInt24 :: ByteString -> Int
parseSignedInt24 bs | isNegative = (255 `shift` 24) .|. hml
                    | otherwise  = hml
  where
    [l, m, h] = B.unpack bs
    isNegative = testBit h 7
    [l', m', h'] = fmap fromIntegral [l, m, h]
    hml = (h' `shift` 16) .|. (m' `shift` 8) .|. l'

parseUnsignedInt16 :: ByteString -> Int
parseUnsignedInt16 bs = hl
  where
    [l, h] = B.unpack bs
    [l', h'] = fmap fromIntegral [l, h]
    hl = (h' `shift` 8) .|. l'

parseAltimeter :: ByteString -> Length Double
parseAltimeter = (*~ milli meter) . fromIntegral . parseUnsignedInt16

parseTemperature :: ByteString -> ThermodynamicTemperature Double
parseTemperature = (*~ centi degreeCelsius) . fromIntegral . parseSignedInt24

parseDepth :: ByteString -> Length Double
parseDepth = (*~ milli meter) . fromIntegral . parseSignedInt24
