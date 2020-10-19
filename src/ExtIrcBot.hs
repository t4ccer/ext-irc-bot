{-# LANGUAGE OverloadedStrings #-}

module ExtIrcBot where

import           Commands
import           Parsers
import           TCPClient

type Handler = (Socket -> Command -> IO ())

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
      print v
      h s v
      putStrLn ""
  mainLoop s h
