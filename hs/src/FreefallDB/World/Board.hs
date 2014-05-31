{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}

module FreefallDB.World.Board
(
  PostID(..),
  Post(..),
  Board(..),
  addThread,
  getName,
  getThreads,
  addPost,
  addReply,
  getPostByID
)
where

import qualified Data.ByteString.Char8 as C
import Data.Acid
import Data.Acid.Advanced (query')
import Data.Typeable
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import Data.SafeCopy
import Data.Map (Map, lookup)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Time
import Network.MessagePackRpc.Server
import Data.Label (mkLabels)
import qualified Data.Label as L

data PostID = PostID Integer deriving (Eq, Ord, Show, Typeable)

data Post = Post {
  _postid :: PostID,
  _date  :: UTCTime,
  _title :: ByteString,
  _body  :: ByteString,
  _replies :: [PostID]
} deriving (Show, Typeable)

data Board = Board {
  _name :: ByteString,
  _lastid :: PostID,
  _idmap :: M.Map PostID Post,
  _threads :: [PostID]
} deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''PostID)
$(deriveSafeCopy 0 'base ''Post)
$(deriveSafeCopy 0 'base ''Board)

mkLabels [''PostID, ''Post, ''Board]

newBoard :: ByteString -> Board
newBoard n = Board n (PostID 0) M.empty []

idinc :: PostID -> PostID
idinc (PostID i) = PostID (i+1)

addThread :: (ByteString, ByteString) -> UTCTime -> Update Board (PostID)
addThread (t, b) now = do
  board <- get
  let pid = idinc (L.get lastid board)
  let post = Post pid now t b []
  addPost board post
  return pid

addPost :: Board -> Post -> Update Board ()
addPost board post = do
  let pid = L.get postid post
  let idmap' = M.insert pid post (L.get idmap board)
  let threads' = pid : (L.get threads board)
  let board' = Board (L.get name board) pid idmap' threads'
  put board'

addReply :: (ByteString, ByteString) -> UTCTime -> PostID -> Update Board (Maybe PostID)
addReply (t, b) now parentID = do
  board <- get
  let pid = idinc $ L.get lastid board
  let post = Post pid now t b []
  let maybeParent = M.lookup parentID (L.get idmap board)
  case maybeParent of
    Just parent -> do
      let parent' = L.set replies (pid : L.get replies parent) parent
      let pid = L.get postid post
      let idmap' = M.insert parentID parent' $ M.insert pid post (L.get idmap board)
      put $ Board (L.get name board) pid idmap' (L.get threads board)
      return $ Just pid
    Nothing -> return Nothing

getThreads :: Query Board [PostID]
getThreads = do
  board <- ask
  return $ L.get threads board

getName :: Query Board ByteString
getName = do
  board <- ask
  return $ L.get name board

getPostByID :: PostID -> Query Board (Maybe Post)
getPostByID pid = do
  board <- ask
  return $ M.lookup pid (L.get idmap board)
