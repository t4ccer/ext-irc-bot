{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ExtIrcBot
import Commands
import TCPClient
import Data.String.Interpolate (i)

main :: IO ()
main = runBot "irc.freenode.net" "6667" "foobot" "#botwar" handleCommand


handleCommand :: Socket -> Command -> IO ()
handleCommand s (PING v) = do
  putStrLn "Sent ping"
  sendCommand s $ PONG v
handleCommand _ (PRIVMSG _ _ "\SOHVERSION\SOH") = do
  return ()
handleCommand s (PRIVMSG _ c m) = do
  sendCommand s $ PRIVMSG "" c $ reverse m
handleCommand _ x = do
  putStrLn [i|Handler not defined: for #{x}|]