module IChatMain
    (
      main'
    , ServerAddr
    )
where

import System.IO (hSetBuffering, BufferMode(..), hClose)
import Network (connectTo, PortID(..))

import qualified Data.ByteString.Lazy.Char8 as C

import Control.Concurrent.MVar
import Control.Monad.State

import State

import IChat

type ServerAddr = (String, Int)

data MyBotState =
    MyBotState {
        mbs_msg_cnt :: Int
    } deriving (Show)

user_state :: MyBotState
user_state = MyBotState { mbs_msg_cnt = 0
                        }


-- | msg dispatcher function, makes bot intelligent :]
bot_react :: IChatS2C -> BotApp MyBotState ()
bot_react (IChatS2C _s_cmd m@(IChatMessage _tag _counter sender cmd)) =

    do let log_m msg = let (p1, p2) = break (== ' ') msg
                       in  (liftIO . putStrLn) $ p1 ++ "<" ++ show (C.unpack sender) ++ ">" ++ p2

           -- do smth with our state
           u_get    = bs_user_state `fmap` get
           u_put us = get >>= \s -> put s{bs_user_state = us}
           u_up  f  = do us <- f `fmap` u_get
                         u_put us
                         return us

       u_up $ \(MyBotState cnt) -> MyBotState (cnt + 1)

       -- log interesting messages first
       case cmd of
           Refresh _line _login _nick _away_msg _receiver _version _status
               -> return ()

           Board _chunk _text
               -> log_m $ (take 70 $ show cmd) ++ "..."

           IChatCommand_Junk _raw_data
               -> do log_m $ show cmd
                     liftIO $ appendFile "unknown_commands.log" $ show m ++ "\n"

           _   -> log_m $ show cmd

       bot_state <- u_get

       my_sig    <- bs_my_signature `fmap` get
       bcast_sig <- bs_bcast_signature `fmap` get
       -- do something nasty :]
       case cmd of
           Connect line _login _nick _away_msg _receiver _version _status
                -> do when (my_sig /= sender) $
                        when (line == main_line) $
                          with_receiver sender $
                              connect line

           Refresh line _login _nick _away_msg receiver _version _status
                -> when (C.unpack receiver == "*") $
                       -- do not send non-broadcast refresh into "*"
                       when (C.unpack line /= "*"
                             && C.unpack bcast_sig == "*") $
                           with_receiver sender $
                               refresh line
           StatusRequest
                -> when (my_sig /= sender) $
                       with_receiver sender $
                           status

           Create line _receiver
                -> do when (my_sig /= sender) $
                          with_receiver sender $
                              connect line

           Text line text receiver_nick
                -> do let u_text = C.unpack text
                          to_all = C.pack ""
                      when (my_sig /= sender) $
                          do when (receiver_nick == to_all) $ -- bcast
                                 when (u_text `elem` [":)", ":]", "[:", "(:"]) $
                                     say line text to_all
                             case u_text of
                                 ('s':'a':'y':' ':xs) -> say line (C.pack xs) to_all
                                 "state"              -> with_receiver sender $
                                                             say line (C.pack $ show bot_state) to_all
                                 _                    -> return ()

                      when (line /= main_line) $
                          with_receiver sender $
                              with_away_msg (C.pack $ "your \"" ++ (C.unpack text) ++ "\" received") $
                                  received line
           _   -> return ()
--
--

main' :: String -> ServerAddr -> IO ()
main' nick (host, port) =
    do print $ "Hello! I am " ++ nick ++ " at " ++ host ++ ":" ++ show port
       h <- connectTo host (PortNumber (fromIntegral port))
       -- TODO: add `bracket' to prevent handle leak

       hSetBuffering h NoBuffering -- hFlush might be better

       sq <- newMVar []
       -- TODO: hide this bot state
       let bot_state = BotState { bs_counter         = 0
                                , bs_nick            = C.pack nick
                                , bs_login           = C.pack nick
                                , bs_my_signature    = C.pack $ "127.0.0.1/haskell/" ++ nick
                                , bs_bcast_signature = C.pack "*"
                                , bs_handle          = h
                                , bs_status          = 0
                                , bs_version         = C.pack "0.0.1"
                                , bs_away_msg        = C.pack "Hello All!"
                                , bs_sendq           = sq
                                , bs_user_state      = user_state
                                }

       evalStateT (bot_main bot_react) bot_state

       hClose h
