
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module TCPClient(Socket, sendCommand, recvUntill, runTCPClient) where

import           IrcCommands
import qualified Control.Exception         as E
import qualified Data.ByteString.Char8     as C
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)

type BS = C.ByteString

-- | Sends `IrcCommands` through `Socket` connected to IRC server
sendCommand :: Socket -> IrcCommand -> IO ()
sendCommand s c = do
  let str = C.pack $ stringifyCommand c <> "\r\n"
  sendAll s str

-- | Resolves address and opens `Socket`
openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

-- | Receives from socket unitl received certain Char
recvUntill :: Socket -> Char -> IO BS
recvUntill s c = go s c ""
  where
    go :: Socket -> Char -> BS -> IO BS
    go s' c' buf = do
      new <- recv s 1
      if new == C.singleton c'
        then return buf
        else go s' c' $ buf <> new

-- | From the "network-run" package.
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
