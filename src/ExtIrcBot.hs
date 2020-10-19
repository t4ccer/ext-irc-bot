{-# LANGUAGE OverloadedStrings #-}

module ExtIrcBot where

import           ChatEvents
import           Control.Concurrent (forkIO)
import           Control.Monad      (void)
import           IrcCommands
import           Parsers
import           TCPClient

type Handler = ChatEvent -> IO ChatAction

data BotSettings = BotSettings
  { host           :: String   -- ^ IRC server hostname
  , port           :: Int      -- ^ IRC server port. 6667 by default
  , name           :: String   -- ^ Bot name
  , channels       :: [String] -- ^ Channels that bot'll join after start
  , handler        :: Handler  -- ^ Chat event handler function
  , async_handlers :: Bool     -- ^ If True each handler will be called with forkIO
  }

runBot :: BotSettings -> IO ()
runBot s = runTCPClient (host s) (show $ port s) $ \sock -> do
  sendCommand sock $ NICK (name s)
  sendCommand sock $ USER (name s) (name s) (name s) (name s)
  mapM_ (sendCommand sock . JOIN (name s)) (channels s)
  mainLoop sock s

mainLoop :: Socket -> BotSettings -> IO ()
mainLoop s set = do
  msg <- recvUntill s "\n"
  let x = runCommandParser msg
  case x of
    Left _ -> do
      return ()
    Right v -> if async_handlers set == True
      then void $ forkIO $ handleCommand s v $ handler set
      else handleCommand s v $ handler set
  mainLoop s set

handleCommand :: Socket -> IrcCommand -> Handler -> IO ()
handleCommand s cmd h = case cmd of
    PING m -> sendCommand s $ PONG m
    _ -> do
      let event  = commandToEvent cmd
      action <- h event
      let cmd    = actionToCommand action
      print event
      print action
      putStrLn ""
      sendCommand s cmd

