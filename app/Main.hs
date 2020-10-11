{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Data.String.Interpolate (i)
import Data.Void
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseCommand :: Parser Command
parseCommand = choice $ fmap try
  [ parsePing
  , parsePrivMsg
  , parseJoin
  , parsePart
  ]
    

parsePing :: Parser Command
parsePing = do
  _ <- string "PING"
  _ <- optional space
  val <- some $ anySingleBut '\r'
  return $ PING val

parsePrivMsg :: Parser Command
parsePrivMsg = do
  char ':'
  author <- many $ anySingleBut ' '
  optional space
  string "PRIVMSG"
  optional space
  chan <- many $ anySingleBut ' '
  optional space
  char ':'
  msg <- some $ anySingleBut '\r'
  return $ PRIVMSG author chan msg

parsePart :: Parser Command
parsePart = do
  char ':'
  user <- many $ anySingleBut ' '
  optional space
  string "PART"
  optional space
  chan <- many $ anySingleBut '\r'
  return $ PART user chan

parseJoin :: Parser Command
parseJoin = do
  char ':'
  user <- many $ anySingleBut ' '
  optional space
  string "JOIN"
  optional space
  chan <- many $ anySingleBut '\r'
  return $ JOIN user chan


runCommandParser :: String -> Maybe Command
runCommandParser = parseMaybe parseCommand

handleCommand :: Socket -> Command -> IO ()
handleCommand s (PING v) = do
  putStrLn "Sent ping"
  sendCommand s $ PONG v
handleCommand _ (PRIVMSG _ _ "\SOHVERSION\SOH") = do
  return ()
handleCommand s (PRIVMSG _ c m) = do
  sendCommand s $ PRIVMSG botName c $ reverse m
handleCommand _ x = do
  putStrLn [i|Handler not defined: for #{x}|]


type BS = C.ByteString

data Command
  = NICK String
  | USER String String String String
  | JOIN String String -- No multi-channel or password support
  | PART String String
  | PING String
  | PONG String
  | PRIVMSG String String String
  deriving (Show)

botName :: String
botName = "foobarfhuosafdfhiuasf"

stringifyCommand :: Command -> String
stringifyCommand (NICK n) = [i|NICK #{n}|]
stringifyCommand (USER u h s r) = [i|USER #{u} #{h} #{s} #{r}|]
stringifyCommand (JOIN u c) = [i|:#{u} JOIN #{c}|]
stringifyCommand (PONG v) = [i|PONG #{v}|]
stringifyCommand (PRIVMSG u c m) = [i|:#{u} PRIVMSG #{c} :#{m}|]

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

main :: IO ()
main = runTCPClient "irc.freenode.net" "6667" $ \s -> do
  sendCommand s $ NICK botName
  sendCommand s $ USER botName botName botName botName
  sendCommand s $ JOIN botName "#botwar"
  mainLoop s

mainLoop :: Socket -> IO ()
mainLoop s = do
  msg <- recvUntill s "\n"
  let x = parse parseCommand "foobar" $ C.unpack msg
  case x of
    Left _ -> do 
      return ()
      -- C.putStrLn msg
    Right v -> do
      print v
      handleCommand s v
      putStrLn ""

  mainLoop s

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