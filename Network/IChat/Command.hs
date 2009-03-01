module Network.IChat.Command
    (
      IChatCommand(..)

    , icc_raw
    , raw_icc

    , main_line
    , private_line
    )
where

import Network.IChat.Aux

import qualified Data.ByteString.Lazy.Char8 as C

main_line :: C.ByteString
main_line = C.pack "iTCniaM"

private_line :: C.ByteString
private_line = C.pack "gsMTCI"

data IChatCommand =
    Disconnect C.ByteString -- line
    | Connect C.ByteString C.ByteString C.ByteString C.ByteString C.ByteString C.ByteString Int -- line login nick away-mess receiver version status
    | Refresh C.ByteString C.ByteString C.ByteString C.ByteString C.ByteString C.ByteString Int -- as Connect
    | StatusRequest
    | RefreshBoard
    | Board Int C.ByteString -- chunk text
    | Status Int C.ByteString -- status away_text
    | Alert C.ByteString -- text
    | CreateLine C.ByteString C.ByteString C.ByteString -- line pass sender
    | Create C.ByteString C.ByteString -- line receiver
    | Received C.ByteString C.ByteString -- line text
    | Text C.ByteString C.ByteString C.ByteString -- line text receiver_nick
    | Me C.ByteString C.ByteString C.ByteString   -- line text receiver
    | Rename C.ByteString   -- new_nick

    | IChatCommand_Junk C.ByteString -- raw_data

instance Show IChatCommand where
    show (Disconnect line) =
        "[DISCONNECT] { line = " ++ show_bs line ++ " }"
    show (Connect line login nick away_msg receiver version status) =
        "[CONNECT] { line = " ++ show_bs line
        ++ ", login = " ++ show_bs login
        ++ ", nick = " ++ show_bs nick
        ++ ", away_msg = " ++ show_bs away_msg
        ++ ", receiver = " ++ show_bs receiver
        ++ ", version = " ++ show_bs version
        ++ ", status = " ++ show status
        ++ " }"
    show (Refresh line login nick away_msg receiver version status) =
        "[REFRESH] { line = " ++ show_bs line
        ++ ", login = " ++ show_bs login
        ++ ", nick = " ++ show_bs nick
        ++ ", away_msg = " ++ show_bs away_msg
        ++ ", receiver = " ++ show_bs receiver
        ++ ", version = " ++ show_bs version
        ++ ", status = " ++ show status
        ++ " }"
    show (StatusRequest) =
        "[STATUS_REQ]"
    show (RefreshBoard) =
        "[REFRESH_BOARD]"
    show (Board chunk text) =
        "[BOARD] { chunk = " ++ show chunk
        ++ ", text = " ++ show_bs text
        ++ " }"
    show (Received line text) =
        "[RECEIVED] { line = " ++ show_bs line
        ++ ", text = " ++ show_bs text
        ++ " }"
    show (Text line text receiver_nick) =
        "[TEXT] { line = " ++ show_bs line
        ++ ", text = " ++ show_bs text
        ++ ", receiver_nick = " ++ show_bs receiver_nick
        ++ " }"
    show (Me line text receiver) =
        "[ME] { line = " ++ show_bs line
        ++ ", text = " ++ show_bs text
        ++ ", receiver = " ++ show_bs receiver
        ++ " }"
    show (Status status away_text) =
        "[STATUS] { status = " ++ show status
        ++ ", away_text = " ++ show_bs away_text
        ++ " }"
    show (Alert text) =
        "[ALERT] { text = " ++ show_bs text
        ++ " }"
    show (Rename new_nick) =
        "[RENAME] { new_nick = " ++ show_bs new_nick
        ++ " }"
    show (CreateLine line password sender) =
        "[CREATE_LINE] { line = " ++ show_bs line
        ++ ", password = " ++ show_bs password
        ++ ", sender = " ++ show_bs sender
        ++ " }"

    show (Create line receiver) =
        "[CREATE] { line = " ++ show_bs line
        ++ ", receiver = " ++ show_bs receiver
        ++ " }"

    show (IChatCommand_Junk raw_data) =
        "[JUNK] { raw_msg = " ++
        (show $ zip [0 :: Int .. ] $ map C.unpack $ param_split raw_data)
        ++ " }"

icc_raw :: IChatCommand -> C.ByteString
icc_raw cmd =
    case cmd of
        -- [~~] [line] [~]
        Disconnect line
            -> param_join [C.pack "DISCONNECT", null_bs, line, null_bs]
        Alert text
            -> param_join [C.pack "ALERT", null_bs, text, null_bs]
        -- [~~] [line] [~~] [login] [~~] [nick] [~~] [~~] [away_msg] [~~] [receiver] [~~] [ver] [~~] [status] [~]
        Connect line login nick away_msg receiver version status
            -> param_join [C.pack "CONNECT", null_bs, line, null_bs, login, null_bs, nick
                          , null_bs, null_bs, null_bs, away_msg, null_bs, receiver
                          , null_bs, version, null_bs, (C.pack . show) status, null_bs]
        Refresh line login nick away_msg receiver version status
            -> param_join [C.pack "REFRESH", null_bs, line, null_bs, login, null_bs, nick
                          , null_bs, null_bs, null_bs, away_msg, null_bs, receiver
                          , null_bs, version, null_bs, (C.pack . show) status, null_bs]
        StatusRequest
            -> param_join [C.pack "STATUS_REQ", null_bs]

        -- uncertain
        RefreshBoard
            -> param_join [C.pack "REFRESH_BOARD", null_bs]

        Board chunk text
            -> param_join [C.pack "BOARD", null_bs, (C.pack . show) chunk, null_bs, text, null_bs]

        Status status away_text
            -> param_join [C.pack "STATUS", null_bs, (C.pack . show) status, null_bs, away_text, null_bs]

        CreateLine line password sender
            -> param_join [C.pack "CREATE_LINE", null_bs, line, null_bs, password, null_bs, sender, null_bs]

        Create line receiver
            -> param_join [C.pack "CREATE", null_bs, line, null_bs, null_bs, null_bs, receiver, null_bs]

        Received line text
            -> param_join [C.pack "RECEIVED", null_bs, line, null_bs, text, null_bs]

        Text line text receiver_nick
            -> param_join [C.pack "TEXT", null_bs, line, null_bs, text, null_bs, receiver_nick, null_bs]

        Me line text receiver
            -> param_join [C.pack "ME", null_bs, line, null_bs, text, null_bs, receiver, null_bs]

        Rename new_nick
            -> param_join [C.pack "RENAME", null_bs, new_nick, null_bs]

        IChatCommand_Junk raw_data -> raw_data
        -- _                       -> throwEx $ Unimplemented "icc_raw"

raw_icc :: [C.ByteString] -> IChatCommand
raw_icc [] = error $ "not enought params"
raw_icc params@(cmd:args) =
    let argc = length args
    in case (C.unpack cmd) of

         -- (no params. uncertain)
        "REFRESH_BOARD" | argc == 1 -> RefreshBoard

         -- [~]
        "STATUS_REQ"    | argc == 1 -> StatusRequest

        -- [~~] [line] [~]
        "DISCONNECT"    | argc == 3 -> Disconnect $ args !! 1

        -- [~~] [text] [~]
        "ALERT"         | argc == 3 -> Alert $ args !! 1

        -- [~~] [new_nick] [~]
        "RENAME"        | argc == 3 -> Rename $ args !! 1

        -- [~~] [line] [~~] [login] [~~] [nick] [~~] [~~] [away_msg] [~~] [receiver] [~~] [ver] [~~] [status] [~]
        "REFRESH"       | argc == 17  -> Refresh (args !! 1)
                                                 (args !! 3)
                                                 (args !! 5)
                                                 (args !! 9)
                                                 (args !! 11)
                                                 (args !! 13)
                                                 (read (C.unpack $ args !! 15) :: Int)
        -- as REFRESH
        "CONNECT"       | argc == 17  -> Connect (args !! 1)
                                                 (args !! 3)
                                                 (args !! 5)
                                                 (args !! 9)
                                                 (args !! 11)
                                                 (args !! 13)
                                                 (read (C.unpack $ args !! 15) :: Int)

        -- [~~] [chunk] [~~] [text] [~]
        "BOARD"         | argc == 5  -> Board (read (C.unpack $ args !! 1) :: Int)
                                              (args !! 3)
        -- [~~] [line] [~~] [away_text] [~]
        "RECEIVED"      | argc == 5  -> Received (args !! 1)
                                                 (args !! 3)

        -- [~~] [status] [~~] [away_text] [~]
        "STATUS"        | argc == 5  -> Status (read (C.unpack $ args !! 1) :: Int)
                                               (args !! 3)

        -- [~~] [line] [~~] [password] [~~] [sender] [~]
        "CREATE_LINE"   | argc == 7  -> CreateLine (args !! 1)
                                                   (args !! 3)
                                                   (args !! 5)

        -- [~~] [line] [~~] [~~] [sender] [~]
        "CREATE"        | argc == 7  -> Create (args !! 1)
                                               (args !! 5)

        -- [~~] [line] [~~] [text] [~~] [receiver_nick] [~]
        "TEXT"          | argc == 7  -> Text       (args !! 1)
                                                   (args !! 3)
                                                   (args !! 5)

        -- [~~] [line] [~~] [text] [~~] [receiver] [~]
        "ME"            | argc == 7  -> Me         (args !! 1)
                                                   (args !! 3)
                                                   (args !! 5)

        _ -> IChatCommand_Junk (param_join params)
