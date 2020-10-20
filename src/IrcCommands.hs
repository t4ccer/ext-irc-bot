{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module IrcCommands where

import           ChatEvents
import           Data.String.Interpolate (i)

data IrcCommand
  = NICK
      { nickname :: String
      }
  | USER
      { username   :: String
      , hostname   :: String
      , servername :: String
      , realname   :: String
      }
  | JOIN
      { nickname :: String
      , channel  :: String
      }
  | PART
      { nickname :: String
      , channel  :: String
      }
  | PING
      { message :: String
      }
  | PONG
      { message :: String
      }
  | PRIVMSG
      { author  :: String
      , channel :: String
      , message :: String
      }
  | NoCommad
  deriving (Show)

stringifyCommand :: IrcCommand -> String
stringifyCommand (NICK n)        = [i|NICK #{n}|]
stringifyCommand (USER u h s r)  = [i|USER #{u} #{h} #{s} #{r}|]
stringifyCommand (JOIN u c)      = [i|:#{u} JOIN #{c}|]
stringifyCommand (PART u c)      = [i|:#{u} PART #{c}|]
stringifyCommand (PING v)        = [i|PING #{v}|]
stringifyCommand (PONG v)        = [i|PONG #{v}|]
stringifyCommand (PRIVMSG _ c m) = [i|PRIVMSG #{c} :#{m}|]
stringifyCommand NoCommad        = ""

commandToEvent :: IrcCommand -> ChatEvent
commandToEvent cmd = case cmd of
  JOIN u c            -> UserJoined u c
  PART u c            -> UserLeft u c
  PRIVMSG u ('#':c) m -> ChannelMessage u ('#':c) m
  PRIVMSG u _ m       -> PrivMessage u m
  _                   -> OtherEvent $ show cmd

actionToCommand :: ChatAction -> IrcCommand
actionToCommand a = case a of
  SendChannelMessage c m -> PRIVMSG "" c m
  SendPrivMessage u m    -> PRIVMSG "" u m
  _                      -> NoCommad
