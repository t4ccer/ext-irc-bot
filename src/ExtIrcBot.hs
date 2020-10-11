{-# LANGUAGE OverloadedStrings #-}

module ExtIrcBot where

import           Commands
import           Parsers
import           TCPClient

type Handler = (Socket -> Command -> IO ())

runBot :: String -> String -> String -> String -> Handler -> IO ()
runBot host port name chan handler = runTCPClient host port $ \s -> do
  sendCommand s $ NICK name
  sendCommand s $ USER name name name name
  sendCommand s $ JOIN name chan
  mainLoop s handler


mainLoop :: Socket -> Handler -> IO ()
mainLoop s handler = do
  msg <- recvUntill s "\n"
  let x = runCommandParser msg
  case x of
    Left _ -> do
      return ()
      -- C.putStrLn msg
    Right v -> do
      print v
      handler s v
      putStrLn ""
  mainLoop s handler
