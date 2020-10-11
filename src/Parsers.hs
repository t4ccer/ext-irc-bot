{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}

module Parsers (runCommandParser) where

import qualified Data.ByteString.Char8 as C
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Commands

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

runCommandParser :: C.ByteString -> Either (ParseErrorBundle String Void) Command
runCommandParser = parse parseCommand "foobar" . C.unpack