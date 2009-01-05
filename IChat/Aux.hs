module IChat.Aux
    (
      show_bs

    , raw_split
    , raw_join

    , param_split
    , param_join

    , cmd_sep
    , raw_sep
    , cmd_sep_bs
    , raw_sep_bs
    , null_bs
    )
where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Word

--

show_bs :: C.ByteString -> String
show_bs = show . C.unpack

null_bs :: B.ByteString
null_bs = B.pack []

--

raw_sep :: Word8
raw_sep = 0x00

raw_sep_bs :: B.ByteString
raw_sep_bs = B.pack [raw_sep]

raw_split :: B.ByteString -> [C.ByteString]
raw_split = B.split raw_sep

raw_join :: [B.ByteString] -> B.ByteString
raw_join = B.intercalate (B.pack [raw_sep])

--

cmd_sep :: Word8
cmd_sep = 0x13

cmd_sep_bs :: B.ByteString
cmd_sep_bs = B.pack [cmd_sep]

param_split :: B.ByteString -> [C.ByteString]
param_split = B.split cmd_sep

param_join :: [B.ByteString] -> B.ByteString
param_join = B.intercalate (B.pack [cmd_sep])
