module Streaming.Network.Client.Telnet.Parser
  ( parseTelnet
  , telnetHandler
  ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString, pack, singleton)

import Data.Attoparsec.ByteString

import Streaming.Network.Client.Telnet.Cmd

-- Telnet verb.
--
-- e.g. 255(IAC),251(WILL),3
verb :: Parser [Cmd]
verb = do
  word8 255
  v <- word8 251 <|> word8 252 <|> word8 253 <|> word8 254
  c <- anyWord8
  return [IAC, codeToCmd v, codeToCmd c]

-- Sub negotiation.
--
-- e.g. IAC,SB,<option code number>,1,IAC,SE
neg :: Parser [Cmd]
neg = do
  word8 255
  word8 250
  c <- anyWord8
  word8 1
  a <- anyWord8
  word8 255
  word8 240
  return [IAC, SB, codeToCmd c, Misc 1, codeToCmd a, IAC, SE]

-- Unmatched data gets forwarded along.
pass :: Parser [Cmd]
pass = do
  w <- anyWord8
  return [codeToCmd w]

parseTelnet :: Parser [Cmd]
parseTelnet = verb <|> neg <|> pass

squash :: [Cmd] -> Maybe ByteString
squash [] = Nothing
squash cs = Just . pack . fmap cmdToCode $ cs

-- Given a command list, return a tuple of (bytes to forward, bytes to send).
-- Forwarded bytes go to the next parser, sent bytes go out the network socket.
--
-- These states are not being tracked and simply notify a server that requires
-- this kind of exchange that the client is behaving (but not really).
--
-- TODO: This needs more extensive testing.
telnetHandler :: [Cmd] -> (Maybe ByteString, Maybe ByteString)
telnetHandler [Misc w]            = (Just . singleton $ w, Nothing)
telnetHandler [IAC, WILL, Misc a] = (Nothing, squash [IAC, DO,   Misc a])
telnetHandler [IAC, DO,   Misc a] = (Nothing, squash [IAC, WILL, Misc a])
telnetHandler [IAC, WONT, Misc a] = (Nothing, squash [IAC, DONT, Misc a])
telnetHandler [IAC, DONT, Misc a] = (Nothing, squash [IAC, WONT, Misc a])
telnetHandler [IAC, SB,   Misc a, Misc 1, IAC, SB] =
  (Nothing, squash [IAC, SB, Misc a, Misc 0, IAC, SE]) -- not exactly valid
telnetHandler _ = (Nothing, Nothing)

