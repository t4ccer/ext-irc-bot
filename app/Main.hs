{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           ChatEvents
import           Data.String.Interpolate (i)
import           ExtIrcBot
import           Utils

botName :: String
botName = "foobot"

main :: IO ()
main = runBot BotSettings
  { host ="irc.freenode.net"
  , port = "6667"
  , name = botName
  , channels = ["#botwar"]
  , handler =  handleEvent
  , async_handlers = False
  }

handleEvent :: Handler
handleEvent (UserJoined u c)       = return $ SendChannelMessage c [i|Hello #{humanizeName u}|]
handleEvent (ChannelMessage u c m) = return $ SendChannelMessage c [i|#{humanizeName u} said: #{m}|]
handleEvent _                      = return $ NoAction
