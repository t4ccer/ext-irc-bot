{-# LANGUAGE OverloadedStrings #-}

module ExtIrcBot where

import           ChatEvents
import           IrcCommands
import           Parsers
import           TCPClient

type Handler = ChatEvent -> ChatAction

data BotSettings = BotSettings
  { host     :: String
  , port     :: String
  , name     :: String
  , channels :: [String]
  , handler  :: Handler
  }

runBot :: BotSettings -> IO ()
runBot s = runTCPClient (host s) (port s) $ \sock -> do
  sendCommand sock $ NICK (name s)
  sendCommand sock $ USER (name s) (name s) (name s) (name s)
  mapM_ (sendCommand sock . JOIN (name s)) (channels s)
  mainLoop sock (handler s)

mainLoop :: Socket -> Handler -> IO ()
mainLoop s h = do
  msg <- recvUntill s "\n"
  let x = runCommandParser msg
  case x of
    Left _ -> do
      return ()
    Right v -> do
      case v of
        PING m -> sendCommand s $ PONG m
        _ -> do
          let event  = commandToEvent v
          let action = h event
          let cmd    = actionToCommand action
          print event
          print action
          putStrLn ""
          sendCommand s cmd
  mainLoop s h
