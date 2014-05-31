{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

import Data.Acid
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import qualified Data.ByteString.Char8 as C
import Network.MessagePackRpc.Server
import Data.Time
import Data.SafeCopy
import qualified Data.Map as M

import FreefallDB.World.Board
import FreefallDB.World.BoardServer

--(makeAcidic ''Board ['addThread, 'addPost, 'getThreads, 'getName])

main :: IO()
main = do
  acid <- openLocalStateFrom "boards/test" (Board (C.pack "test") (PostID 0) M.empty [])

  serve 5894 [
     ("getThreadIDs", prep acid getThreadIDs),
     ("makeThread", prep acid makeThread),
     ("reply", prep acid reply),
     ("getPost", prep acid getPost)
   ]

prep a f = toMethod $ f a

--  closeAcidState acid
