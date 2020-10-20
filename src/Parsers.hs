{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}

module Parsers (runCommandParser) where

import           IrcCommands
import qualified Data.ByteString.Char8 as C
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

manyAnyToSpace :: Parser String
manyAnyToSpace = do
  v <- many $ anySingleBut ' '
  _ <- optional space
  return v

parseCommand :: Parser IrcCommand
parseCommand = choice $ fmap try
  [ parsePing
  , parsePrivMsg
  , parseJoin
  , parsePart
  ]

parsePing :: Parser IrcCommand
parsePing = do
  _     <- string "PING"
  _     <- optional space
  p_val <- some $ anySingleBut '\r'
  return $ PING p_val

parsePrivMsg :: Parser IrcCommand
parsePrivMsg = do
  _        <- char ':'
  p_author <- manyAnyToSpace
  _        <- string "PRIVMSG"
  _        <- optional space
  p_chan   <- manyAnyToSpace
  _        <- char ':'
  p_msg    <- some $ anySingleBut '\r'
  return $ PRIVMSG p_author p_chan p_msg

parsePart :: Parser IrcCommand
parsePart = do
  _      <- char ':'
  p_user <- many $ anySingleBut ' '
  _      <- optional space
  _      <- string "PART"
  _      <- optional space
  p_chan <- many $ anySingleBut '\r'
  return $ PART p_user p_chan

parseJoin :: Parser IrcCommand
parseJoin = do
  _      <- char ':'
  p_user <- many $ anySingleBut ' '
  _      <- optional space
  _      <- string "JOIN"
  _      <- optional space
  p_chan <- many $ anySingleBut '\r'
  return $ JOIN p_user p_chan

-- | Runs `IrcCommand` parser
runCommandParser :: C.ByteString -> Either (ParseErrorBundle String Void) IrcCommand
runCommandParser = parse parseCommand "error" . C.unpack
