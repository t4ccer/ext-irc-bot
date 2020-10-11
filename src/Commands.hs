{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Commands where

import           Data.String.Interpolate (i)

data Command
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
  deriving (Show)

stringifyCommand :: Command -> String
stringifyCommand (NICK n)        = [i|NICK #{n}|]
stringifyCommand (USER u h s r)  = [i|USER #{u} #{h} #{s} #{r}|]
stringifyCommand (JOIN u c)      = [i|:#{u} JOIN #{c}|]
stringifyCommand (PART u c)      = [i|:#{u} PART #{c}|]
stringifyCommand (PING v)        = [i|PING #{v}|]
stringifyCommand (PONG v)        = [i|PONG #{v}|]
stringifyCommand (PRIVMSG _ c m) = [i|PRIVMSG #{c} :#{m}|]
