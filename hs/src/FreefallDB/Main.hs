{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module FreefallDB.Main
(
    main
) where

import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll, getContents)
import Data.Serialize
import Control.Monad (forever, guard)
import Control.Exception (tryJust)
import System.IO.Error (isEOFError)
import Data.Acid
import qualified Data.Map as M
import Network.MessagePackRpc.Server

import FreefallDB.Network.TcpServer
import FreefallDB.Network.Commands
import FreefallDB.World.Board

main :: IO()
main = do
  let host = "0.0.0.0"
  let port = 5847

  board <- openLocalStateFrom "boards/test/" (newBoard "test")

  serve port [
    ("makeThread", prep makeThread)
   ]

prep f = fun $ f board
