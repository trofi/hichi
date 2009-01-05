module IChat.Message
    (
      IChatMessage(..)

    , icm_raw
    , raw_icm
    )
where

import IChat.Command
import IChat.Aux

import qualified Data.ByteString.Lazy.Char8 as C

import Error

data IChatMessage =
    -- IChatMessage_Junk C.ByteString -- raw data
    IChatMessage C.ByteString Int C.ByteString IChatCommand -- tag counter sender command+params
instance Show IChatMessage where
    show (IChatMessage tag counter sender cmd) =
        "IChatMessage { tag = " ++ show_bs tag
        ++ ", counter = " ++ show counter
        ++ ", sender = " ++ show_bs sender
        ++ ", cmd = " ++ show cmd
        ++ " }"

-- [~] tag [~~] [ASCII-counter] [~~] [sender] [~~] [CMD + cmd-params]
icm_raw :: IChatMessage -> C.ByteString
icm_raw (IChatMessage tag counter sender cmd) =
    param_join [null_bs,
                tag,
                null_bs,
                (C.pack . show) counter,
                null_bs,
                sender,
                null_bs,
                icc_raw cmd]

raw_icm :: C.ByteString -> IChatMessage
raw_icm raw_msg =
    let params = param_split raw_msg
        params_count = length params
    in case (params_count < 8) of
            True  -> err $ "not enough params: got " ++ show params_count ++ ", at least 8 expected"
            False -> let tag    = params !! 1
                         c      = read (C.unpack $ params !! 3) :: Int
                         sender = params !! 5
                         rest   = drop 7 params
                     in IChatMessage tag c sender $ raw_icc rest
    where err s = throwEx $ BadIChatMessage $ "raw_s2c: " ++ s ++ " (raw message: " ++ show_bs raw_msg ++ ")"
