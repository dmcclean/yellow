{-# LANGUAGE BangPatterns #-}

module Yellow.Drivers.Arduino
(
  sendCommand,
  performRead
)
where

import Prelude as P
import Data.ByteString as B
import System.RaspberryPi.GPIO
import Data.Bits
import Data.Word (Word8)
import Control.Concurrent
import Control.Monad.Except

sendCommand :: Address -> ByteString -> I2C ()
sendCommand adr msg = do
                        let msg' = addCrc msg
                        writeI2CE adr msg'

performRead :: Address -> Word8 -> Int -> I2C ByteString
performRead adr hdr n = do
                          let msg = addCrc . B.pack $ [hdr]
                          writeI2CE adr msg
                          -- lift $ P.putStrLn . show . unpack $ msg
                          lift $ threadDelay 10000
                          res <- readI2CE adr (n + 1)
                          -- lift $ P.putStrLn . show . unpack $ res
                          if validateCrc res
                            then return $ (B.take (B.length res - 1) res)
                            else throwError ChecksumInvalid

validateCrc :: ByteString -> Bool
validateCrc bs | B.length bs < 2 = False
               | otherwise = B.last bs == crc8 (B.take (B.length bs - 1) bs)

addCrc :: ByteString -> ByteString
addCrc bs = snoc bs (crc8 bs)

crc8 :: ByteString -> Word8
crc8 = B.foldl crc8Byte 0

crc8Byte :: Word8 -> Word8 -> Word8
crc8Byte a b = go a b 8
  where
    go :: Word8 -> Word8 -> Int -> Word8
    go !x _ 0 = x
    go !x !y !bit = go x' y' (bit - 1)
      where
        x' | testBit (x `xor` y) 7 = (x `unsafeShiftL` 1) `xor` 0x07
           | otherwise             = x `unsafeShiftL` 1
        y' = y `unsafeShiftL` 1
