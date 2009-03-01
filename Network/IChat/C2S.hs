module Network.IChat.C2S
    (
      IChatC2S(..)
    , c2s_raw
    )
where

import Network.IChat.IO
import Network.IChat.Aux
import Network.IChat.Message

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Codec.RC4 as RC4

passphrase :: C.ByteString
passphrase = C.pack "tahci"

data IChatC2S =
    -- IChatC2S_Junk C.ByteString -- raw data
    IChatC2S C.ByteString C.ByteString C.ByteString IChatMessage -- sender command receiver message
instance Show IChatC2S where
    show (IChatC2S sender cmd receiver msg) =
        "IChatC2S { sender = " ++ show_bs sender
        ++ ", cmd = " ++ show_bs cmd
        ++ ", receiver = " ++ show_bs receiver
        ++ ", msg = " ++ show msg
        ++ " }"

c2s_raw :: IChatC2S -> RawMsg
c2s_raw (IChatC2S sender cmd receiver msg) = RawMsg raw_data
    where raw_data = raw_join [sender, cmd, receiver, RC4.encode passphrase (icm_raw msg)]
