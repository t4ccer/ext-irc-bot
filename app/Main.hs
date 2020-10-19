{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Commands                (Command (..))
import           Data.List               (isPrefixOf)
import           Data.String.Interpolate (i)
import           ExtIrcBot
import           TCPClient               (sendCommand)

botName :: String
botName = "foobot"

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = flip isPrefixOf

main :: IO ()
main = runBot BotSettings
  { host ="irc.freenode.net"
  , port = "6667"
  , name = botName
  , channels = ["#botwar"]
  , handler =  handleCommand
  }

handlerNotDefined :: Handler 
handlerNotDefined _ msg = 
  putStrLn [i|Handler not defined: for #{msg}|]

handlePrivMsg :: String -> Maybe String
handlePrivMsg msg
  | msg == [i|#{botName} help|] = Just [i|Hi, I am reverser bot. Type [#{botName} reverse <msg>] and I will send <msg> reversed|]
  | msg `startsWith` [i|#{botName} reverse |] = Just $ reverse $ drop (length ([i|#{botName} reverse |] :: String)) msg
  | otherwise = Nothing

handleCommand :: Handler
handleCommand s (PING v) = do
  sendCommand s $ PONG v

handleCommand s (PRIVMSG a c m) = do
  let res = handlePrivMsg m
  case res of
    Just res' -> sendCommand s $ PRIVMSG "" c res'
    Nothing -> handlerNotDefined s (PRIVMSG a c m) 

handleCommand _ x = do
  putStrLn [i|Handler not defined: for #{x}|]

