{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Streaming.Network.Client.Telnet
  ( runTelnet
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Function ((&))
import Data.Maybe (isJust)

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import qualified Data.ByteString.Streaming as Q
import Streaming
import qualified Streaming.Prelude as S
import Streaming.Network.TCP

import Streaming.Network.Client.Telnet.Parser

type UserStateF s a = (s -> a -> (s, Maybe ByteString))

runTelnet :: (MonadIO m, Show a)
  => String         -- ^ Host
  -> String         -- ^ Port
  -> A.Parser a     -- ^ User supplied parser
  -> s              -- ^ User supplied initial state
  -> UserStateF s a -- ^ User supplied state function
  -> m ()
runTelnet h p q s f =
  liftIO $ connect h p $ \(sock, _) ->
    void $
      fromSocket sock 4096      -- Read from handle
        & AS.parsed parseTelnet -- Parse telnet codes -> Stream (Of Cmd)
        & S.map telnetHandler   -- Run codes through handler -> Stream (Of (Maybe ByteString, Maybe ByteString))
        & S.mapM (runt sock)    -- Send telnet commands, forward rest
        & S.concat              -- Stream (Of (Maybe ByteString) -> Stream (Of ByteString)
        & Q.fromChunks          -- Stream (Of ByteString) -> ByteString m r
        & AS.parsed q           -- Parse user commands -> Stream (Of a)
        & S.mapM (runu sock)    -- Send user commands and track app state
        & S.print
  where
    -- Output response telnet codes to the socket.
    -- Forward actual data to the outer parser.
    runt sock (fwd, out) = do
      -- Send telnet command out
      S.yield out         -- Stream (Of (Maybe ByteString))
        & S.filter isJust -- Filter out Nothing
        & S.concat        -- Strip off Maybe
        & Q.fromChunks    -- Stream (Of ByteString) -> ByteString m r
        & toSocket sock   -- Send over handle
      -- Send rest of data forward
      return fwd

    -- Update the user supplied state with the parsed command.
    -- If it emits a command send it over the socket.
    --
    -- TODO: Can we hold state with S.fold?
    runu sock v = do
      let (a', v') = f s v
      S.yield v'          -- Stream (Of (Maybe ByteString))
        & S.filter isJust -- Filter out Nothing
        & S.concat        -- Strip off Maybe
        & Q.fromChunks    -- Stream (Of ByteString) -> ByteString m r
        & toSocket sock   -- Send over handle
      return v
