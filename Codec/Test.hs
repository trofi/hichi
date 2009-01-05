import Control.Monad
import Data.Word

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import Codec.RC4

-- taken from openssl test suite
keys, inputs, outputs :: [[Word8]]
keys = [ [0x01,0x23,0x45,0x67,0x89,0xab,0xcd,0xef]
       , [0x01,0x23,0x45,0x67,0x89,0xab,0xcd,0xef]
       , [0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
       , [0xef,0x01,0x23,0x45]
       , [0x01,0x23,0x45,0x67,0x89,0xab,0xcd,0xef]
       , [0xef,0x01,0x23,0x45]
       , []]

inputs = [ [0x01,0x23,0x45,0x67,0x89,0xab,0xcd,0xef,0xff]
         , [0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff]
         , [0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff]
         , [0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0xff]
         , [0x12,0x34,0x56,0x78,0x9A,0xBC,0xDE,0xF0,
            0x12,0x34,0x56,0x78,0x9A,0xBC,0xDE,0xF0,
            0x12,0x34,0x56,0x78,0x9A,0xBC,0xDE,0xF0,
            0x12,0x34,0x56,0x78,0xff]
         , [0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff]
         , [0]]

outputs = [ [0x75,0xb7,0x87,0x80,0x99,0xe0,0xc5,0x96,0x00]
          , [0x74,0x94,0xc2,0xe7,0x10,0x4b,0x08,0x79,0x00]
          , [0xde,0x18,0x89,0x41,0xa3,0x37,0x5d,0x3a,0x00]
          , [0xd6,0xa1,0x41,0xa7,0xec,0x3c,0x38,0xdf,
             0xbd,0x61,0x5a,0x11,0x62,0xe1,0xc7,0xba,
             0x36,0xb6,0x78,0x58,0x00]
          , [0x66,0xa0,0x94,0x9f,0x8a,0xf7,0xd6,0x89,
             0x1f,0x7f,0x83,0x2b,0xa8,0x33,0xc0,0x0c,
             0x89,0x2e,0xbe,0x30,0x14,0x3c,0xe2,0x87,
             0x40,0x01,0x1e,0xcf,0x00]
          , [0xd6,0xa1,0x41,0xa7,0xec,0x3c,0x38,0xdf,0xbd,0x61,0x00]
          , [0]]

main :: IO ()
main = do print $ encode (C.pack "hello (key)") (C.pack "world (data)")
          forM_ [0 .. (length keys) - 1] $
            \i -> do let inp = B.pack $ init $ inputs  !! i
                         oup = B.pack $ init $ outputs !! i
                         k   = B.pack $ keys    !! i
                         enc = encode k inp
                         ok  = (enc == oup)
                     putStrLn $ show i ++ ": " ++ (if ok then "PASS" else "FAIL")
                     when (not ok) $
                       do putStrLn $ "    GOT: " ++ show enc
                          putStrLn $ "    EXP: " ++ show oup
                          when ((encode k enc) /= inp) $
                            putStrLn $ "   SELF: " ++ (show $ encode k enc)
