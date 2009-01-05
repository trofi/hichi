module Codec.RC4
    (
      encode
    , decode
    )
where

import qualified Data.ByteString.Lazy as B

import Data.Word (Word8)
import Data.Bits

import Control.Monad.State
import Control.Monad.ST
import Control.Monad

import Data.Array.ST

encode :: B.ByteString -> B.ByteString -> B.ByteString
encode key in_data =
    runST $ do_encode key in_data

decode :: B.ByteString -> B.ByteString -> B.ByteString
-- symmetric
decode = encode

--------------------------

do_encode :: B.ByteString -> B.ByteString -> ST s B.ByteString
do_encode key in_data =
    do ar <- (newArray_ (minBound, maxBound) :: ST s (STUArray s Word8 Word8))

       let key_len  = B.length key
           data_len = B.length in_data

       ---
       -- Preparing RC4 context

       forM_ ([minBound .. maxBound] :: [Word8]) $
         \idx -> writeArray ar idx idx

       let -- init_ctx :: StateT (Word8) (ST s)) ()
           init_ctx = forM_ ([minBound .. maxBound] :: [Word8]) $
                \idx -> do a <- lift $ readArray ar idx
                           j' <- get
                           let j, k :: Word8
                               k = B.index key ((fromIntegral idx) `mod` (fromIntegral key_len))
                               j = j' + a + k
                           put j
                           -- swap ar[idx] ar[j]
                           lift $ do a' <- readArray ar j
                                     writeArray ar idx a'
                                     writeArray ar j a
       when (key_len > 0) $
           evalStateT init_ctx 0

       ---
       -- Use RC4 context

       -- TODO: user bytestring's laziness and build result from chunks

       let -- run_ctx :: StateT (Word8, Word8) (ST s) B.ByteString
           run_ctx = do encrypted <- forM ([0 .. (fromIntegral data_len) - 1] :: [Int]) $
                          \idx -> do (x', y') <- get
                                     let x :: Word8
                                         x = x' + 1
                                     a <- lift $ readArray ar x
                                     let y :: Word8
                                         y = y' + a
                                     b <- lift $ readArray ar y
                                     put (x, y)

                                     -- swap ar[x] ar[y]
                                     lift $ do writeArray ar x b
                                               writeArray ar y a
                                     let d = B.index in_data (fromIntegral idx)
                                     d' <- lift $ readArray ar (a + b)
                                     return $ d `xor` d'
                        return $ B.pack encrypted

       evalStateT run_ctx (0, 0)
