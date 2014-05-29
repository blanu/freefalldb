{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module FreefallDB.World.Board
(
  Post(..),
  newBoard,
  newThread,
  getThreads
)
where

import Data.Acid
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import Data.SafeCopy
import Data.Map (Map, lookup, update)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Time

data PostID = PostID Integer

data Post = Post
{
  id    :: PostID,
  date  :: UTCTime,
  title :: ByteString,
  body  :: ByteString,
  replies :: [PostID]
}

data Board = Board
{
  name :: ByteString,
  lastid :: PostID,
  posts :: [Post],
  idmap :: M.Map PostID Post
  threads :: [PostID]
}

$(deriveSafeCopy 0 'base ''PostID)
$(deriveSafeCopy 0 'base ''Post)
$(deriveSafeCopy 0 'base ''Board)

newBoard :: ByteString -> Board
newBoard n = Board n 0 [] M.empty []

newThread :: (ByteString, ByteString) -> UTCTime -> Update Board ()
newThread (t, b) now = do
  board <- get
  let postid = (lastid board) + 1
  let post = Post postid now t b []
  let posts' = (posts board)
  let idmap' = update postid post (idmap board)
  let threads' = postid : (threads board)
  let board' = Board postid posts' idmap' threads'
  put board'

getThreads :: Query Board [PostID]
getThreads = do
  board <- ask
  return (threads board)

$(makeAcidic ''Galaxy ['newThread, 'getThreads])
