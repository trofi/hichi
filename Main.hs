module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import Hichi.IChatMain

data Config =
    Config { my_nick :: String
           , server_addr :: ServerAddr
           } deriving Show

defaultConfig :: Config
defaultConfig =
    Config { my_nick = "hichi"
           , server_addr = ("127.0.0.1", 6666)
           }

main :: IO ()
main = do args <- getArgs
          Config nick serv_addr <- parseArgs args
          main' nick serv_addr

parseArgs :: [String] -> IO Config
parseArgs args =
    case args of
        []  -> return $ defaultConfig
        [x] -> return $ defaultConfig { my_nick = x }
        _   -> do print "usage: hichi <bot-nick>"
                  exitWith (ExitFailure 1)
