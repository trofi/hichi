module IChat.S2C
    (
      IChatS2C(..)
    , raw_s2c
    )
where

import IChat.IO
import IChat.Aux
import IChat.Message

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Codec.RC4 as RC4

import Error

passphrase :: B.ByteString
passphrase = C.pack "tahci"

data IChatS2C =
    -- IChatS2C_Junk B.ByteString -- raw data
    IChatS2C B.ByteString IChatMessage -- command message
instance Show IChatS2C where
    show (IChatS2C cmd msg) =
        "IChatS2C { cmd = " ++ show_bs cmd
        ++ ", msg = " ++ show msg
        ++ " }"

raw_s2c :: RawMsg -> IChatS2C
raw_s2c (RawMsg raw_msg) =
    let (cmd, params') = B.break (== raw_sep) raw_msg
        params = B.tail params'
        u_cmd = C.unpack cmd
    in case (B.null params) of
        True  -> err $ "no data after command"
        False -> case u_cmd of
                     "FORWARD" -> IChatS2C cmd $ raw_icm $ RC4.decode passphrase params
                     _         -> err $ "unknown command: " ++ u_cmd
    where err s = throwEx $ BadIChatS2C $ "raw_s2c: " ++ s ++ " (raw message: " ++ show_bs raw_msg ++ ")"
