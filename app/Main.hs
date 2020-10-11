{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Commands                (Command (..))
import           Control.Monad           (when)
import           Data.List               (isPrefixOf)
import           Data.String.Interpolate (i)
import           ExtIrcBot               (Handler, runBot)
import           TCPClient               (sendCommand)

main :: IO ()
main = runBot "irc.freenode.net" "6667" "foobot" "#botwar" handleCommand

handleCommand :: Handler
handleCommand s (PING v) = do
  putStrLn "Sent ping"
  sendCommand s $ PONG v

handleCommand _ (PRIVMSG _ _ "\SOHVERSION\SOH") = do
  return ()

handleCommand s (PRIVMSG _ c m) = do
  when ("!r " `isPrefixOf` m) $ sendCommand s $ PRIVMSG "" c $ reverse $ drop 3 m

handleCommand _ x = do
  putStrLn [i|Handler not defined: for #{x}|]

