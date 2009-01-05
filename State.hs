module State
    (
      bot_main
    , BotState(..)
    , BotApp

    -- bot api:
    -- low_level
    , send_msg -- :: IChatC2S -> BotApp s ()
    , recv_msg -- :: BotApp s IChatS2C

    -- not-so-low level
    , get_counter -- :: BotApp s Int
    , send_cmd    -- :: IChatCommand -> BotApp s ()

    -- high level (action generators)
    , with_receiver    -- :: C.ByteString -> BotApp s a -> BotApp s a -- can be used as action generator via retval :]
    , with_away_msg    -- :: C.ByteString -> BotApp s a -> BotApp s a -- can be used as action generator via retval :]

    , mk_connect       -- :: C.ByteString -> BotApp s IChatCommand
    , mk_refresh       -- :: C.ByteString -> BotApp s IChatCommand
    , mk_disconnect    -- :: C.ByteString -> BotApp s IChatCommand
    , mk_status        -- :: BotApp s IChatCommand
    , mk_received      -- :: C.ByteString -> BotApp s IChatCommand
    , mk_say           -- :: C.ByteString -> C.ByteString -> C.ByteString -> BotApp s IChatCommand
    , mk_me_say        -- :: C.ByteString -> C.ByteString -> BotApp s IChatCommand

    -- high level (action performers)
    -- (TODO: should evolve to full set of known IChat commands soon)
    , connect     -- :: C.ByteString -> BotApp s ()
    , refresh     -- :: C.ByteString -> BotApp s ()
    , disconnect  -- :: C.ByteString -> BotApp s ()
    , received    -- :: C.ByteString -> BotApp s ()
    , status      -- :: BotApp s ()
    , say         -- :: C.ByteString -> C.ByteString -> C.ByteString -> BotApp s ()
    , me_say      -- :: C.ByteString -> C.ByteString -> BotApp s ()
    )
where

--
-- TODO:
--  * add support for sending `refresh' on timeout

import System.IO (Handle)
import qualified Data.ByteString.Lazy.Char8 as C

import Control.Concurrent(threadDelay, forkIO)
import Control.Concurrent.MVar

import Control.Monad.State

import IChat
import Error

data BotState s =
    BotState { bs_counter         :: Int
             , bs_nick            :: C.ByteString
             , bs_login           :: C.ByteString
             , bs_my_signature    :: C.ByteString
             , bs_bcast_signature :: C.ByteString
             , bs_handle          :: Handle
             , bs_status          :: Int
             , bs_version         :: C.ByteString
             , bs_away_msg        :: C.ByteString
             , bs_sendq           :: MVar [IChatC2S] -- syncs access to bs_handle
             , bs_user_state      :: s
             }

type BotApp s = StateT (BotState s) IO

s_cmd :: C.ByteString
s_cmd  = C.pack "FORWARD"

default_tag :: C.ByteString
default_tag = C.pack "iChit"

---------------------------------------------------------

-- | low level send
send_msg :: IChatC2S -> BotApp s ()
send_msg msg = do -- h <- bs_handle `fmap` get
                  -- liftIO $ writeRM h (c2s_raw msg)
                  -- Now it's handled other way: msg pushed into queue
                  sq <- bs_sendq `fmap` get
                  liftIO $ modifyMVar_ sq (\l -> return $ l ++ [msg])

-- | low level recv
recv_msg :: BotApp s IChatS2C
recv_msg     = do h  <- bs_handle `fmap` get
                  rm <- liftIO $ readRM h
                  return $ raw_s2c rm

---------------------------------------------------------

-- | function to retutn and increment ASCII counter
get_counter :: BotApp s Int
get_counter  = do c <- bs_counter `fmap` get
                  get >>= \s -> put s { bs_counter = c + 1 }
                  return c

send_cmd :: IChatCommand -> BotApp s ()
send_cmd cmd =
    do s <- get
       c <- get_counter
       let ichat_msg  = IChatMessage default_tag c (bs_my_signature s) cmd
           ichat_c2s  = IChatC2S (bs_my_signature s) s_cmd (bs_bcast_signature s) ichat_msg
       send_msg ichat_c2s

--
-- | high level mk_message functions (use them to generate commands conviniently)

---------------------------------------------------------

mk_connect :: C.ByteString -> BotApp s IChatCommand
mk_connect line =
    do s <- get
       return $ Connect line
                        (bs_login s)
                        (bs_nick s)
                        (bs_away_msg s)
                        (bs_bcast_signature s)
                        (bs_version s)
                        (bs_status s)

mk_refresh :: C.ByteString -> BotApp s IChatCommand
mk_refresh line =
    do s <- get
       return $ Refresh line
                        (bs_login s)
                        (bs_nick s)
                        (bs_away_msg s)
                        (bs_bcast_signature s)
                        (bs_version s)
                        (bs_status s)

mk_disconnect :: C.ByteString -> BotApp s IChatCommand
mk_disconnect line = return $ Disconnect line

mk_status :: BotApp s IChatCommand
mk_status =
    get >>=
       \s -> return $ Status (bs_status s) (bs_away_msg s)

mk_received :: C.ByteString -> BotApp s IChatCommand
mk_received line =
    get >>=
        \s -> return $ Received line (bs_away_msg s)

mk_say :: C.ByteString -> C.ByteString -> C.ByteString -> BotApp s IChatCommand
mk_say line text to_nick = return $ Text line text to_nick

mk_me_say :: C.ByteString -> C.ByteString -> BotApp s IChatCommand
mk_me_say line text =
    get >>=
        \s -> return $ Me line text (bs_bcast_signature s)

---------------------------------------------------------
--
-- | high level action functions (use them to send commands conviniently)

-- TODO: check wheter we are in that line already?
connect :: C.ByteString -> BotApp s ()
connect line = mk_connect line >>= send_cmd

refresh :: C.ByteString -> BotApp s ()
refresh line = mk_refresh line >>= send_cmd

disconnect :: C.ByteString -> BotApp s ()
disconnect line = mk_disconnect line >>= send_cmd

received :: C.ByteString -> BotApp s ()
received line = mk_received line >>= send_cmd

status :: BotApp s ()
status = mk_status >>= send_cmd

say :: C.ByteString -> C.ByteString -> C.ByteString -> BotApp s ()
say line text to_nick = mk_say line text to_nick >>= send_cmd

me_say :: C.ByteString -> C.ByteString -> BotApp s ()
me_say line text = mk_me_say line text >>= send_cmd

-- | overrides away message for given action
with_away_msg :: C.ByteString -> BotApp s a -> BotApp s a
with_away_msg away_msg action =
    do old_away_msg <- bs_away_msg `fmap` get
       get >>= \s -> put s{bs_away_msg = away_msg}
       r <- action
       get >>= \s -> put s{bs_away_msg = old_away_msg}
       return r

-- | overrides receiver for given action
with_receiver :: C.ByteString -> BotApp s a -> BotApp s a
with_receiver receiver action =
    do old_receiver <- bs_bcast_signature `fmap` get
       get >>= \s -> put s{bs_bcast_signature = receiver}
       r <- action
       get >>= \s -> put s{bs_bcast_signature = old_receiver}
       return r

--
---------------------------------------------------------

-- TODO: do we need sync access to `h' in reader/writer thread?
-- | send_task loops forever
send_task :: Handle -> MVar [IChatC2S] -> IO ()
send_task h sq = do pending_messages <- swapMVar sq []
                    mapM_ (writeRM h . c2s_raw) pending_messages
                    -- useful for debug
                    -- mapM_ (\m -> putStrLn $ " -> " ++ show m) pending_messages
                    threadDelay (200 * 1000)
                    send_task h sq

-- | recv_task loops forever
recv_task :: (IChatS2C -> BotApp s ()) -> BotApp s ()
recv_task message_handler =
    do botHandleEx (\e -> liftIO $ print e) $
           recv_msg >>= message_handler
       recv_task message_handler

-- | main multithreaded loop
bot_main :: (IChatS2C -> BotApp s ()) -> BotApp s ()
bot_main message_handler =
    do s <- get

       -- start async sendq thread
       _sq_tid <- liftIO $ forkIO $ send_task (bs_handle s) (bs_sendq s)

       -- ouble disconnect to imitate normal ichat clients
       disconnect main_line
       disconnect main_line
       connect    main_line

       -- sync recv thread
       recv_task message_handler

--
--

-- | handleEx wrapper for BotApp monad
botHandleEx :: (IChatError -> BotApp s a) -> BotApp s a -> BotApp s a
botHandleEx handler action = do -- | Some dark magic here to keep state consistent
                                s <- get
                                (r, s') <- liftIO $ handleEx (\e -> runStateT (handler e) s) (runStateT action s)
                                put s'
                                return r
