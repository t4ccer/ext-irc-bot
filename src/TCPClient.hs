
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TCPClient(Socket, sendCommand, recvUntill, runTCPClient) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Commands

type BS = C.ByteString


sendCommand :: Socket -> Command -> IO ()
sendCommand s c = do
  putStr "Sent: "
  let str = C.pack $ stringifyCommand c <> "\r\n"
  C.putStr str
  sendAll s str

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

recvUntill :: Socket -> BS -> IO BS
recvUntill s c = go s c ""
  where
    go :: Socket -> BS -> BS -> IO BS
    go s' c' buf = do
      new <- recv s 1
      if new == c'
        then return buf
        else go s' c' $ buf <> new


-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock