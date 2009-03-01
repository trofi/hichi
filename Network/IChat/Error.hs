{-# OPTIONS -fglasgow-exts #-}
module Network.IChat.Error
    (
      IChatError(..)
    , throwEx
    , catchEx
    , handleEx
    )
where

import Data.Typeable
import qualified Control.Exception as E

data IChatError =
    UnknownError String
    | Unimplemented String -- should be temporary type :]
    | BadIChatS2C String
    | BadIChatMessage String
    deriving (Typeable)

instance E.Exception IChatError

throwEx :: IChatError -> a
throwEx = E.throw

catchEx :: IO a -> (IChatError -> IO a) -> IO a
catchEx = E.catch

handleEx :: (IChatError -> IO a) -> IO a -> IO a
handleEx = flip catchEx

--

instance Show IChatError where
    show = iChatShowError

iChatShowError :: IChatError -> String
iChatShowError (UnknownError err)  = "Unknown error: " ++ err
iChatShowError (Unimplemented err) = "Unimplemented: " ++ err
iChatShowError (BadIChatS2C err) = "Bad IChatS2C: " ++ err
iChatShowError (BadIChatMessage err) = "Bad IChatMessage (wrong decryption?): " ++ err
