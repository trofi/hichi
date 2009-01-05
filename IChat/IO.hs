module IChat.IO
    (
      RawMsg(..)
    , readRM
    , writeRM
    )
where

import IChat.Aux

import qualified Data.ByteString.Lazy.Char8 as C

import System.IO (Handle, hGetChar, hPutChar, hPutStr)
import Data.Char (isNumber, digitToInt)

maximum_raw_packet_size :: Int
maximum_raw_packet_size = 8000 -- bytes

data RawMsg =
    RawMsg C.ByteString -- size data
instance Show RawMsg where
    show (RawMsg raw_msg) = "RawMsg " ++ show_bs raw_msg

readRM :: Handle -> IO RawMsg
readRM h = readRM' h 0

readRM' :: Handle -> Int -> IO RawMsg
readRM' _h msg_len | msg_len > maximum_raw_packet_size = error $ "TODO: make me mkIOError: too large packet to read:" ++ show msg_len
readRM'  h msg_len =
    do ch <- hGetChar h
       -- TODO: handle special case: '00000000..{- many zeroes -}...0000000000....'
       let process_byte | isNumber ch = readRM' h (10 * msg_len + digitToInt ch)
                        | ch == '\0'  = do msg_data <- C.hGet h msg_len
                                           return $ RawMsg msg_data
                        | True        = error "TODO: make me mkIOError: not a number in raw header"
       process_byte

writeRM :: Handle -> RawMsg -> IO ()
writeRM h (RawMsg msg_data) =
    do hPutStr  h $ show $ C.length msg_data
       hPutChar h '\0'
       C.hPut   h msg_data
