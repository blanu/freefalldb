{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}

module FreefallDB.World.BoardServer
(
  getThreadIDs,
  makeThread,
  getPost,
  reply
)
where

import Data.Acid
import Data.Acid.Advanced (query', update')
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
import Control.Monad.IO.Class
import Data.Label (mkLabel)
import qualified Data.Label as L

import FreefallDB.World.Board

$(makeAcidic ''Board ['addThread, 'addPost, 'addReply, 'getThreads, 'getName, 'getPostByID])

makeThread :: AcidState Board -> (ByteString, ByteString) -> Method (Int)
makeThread acid (t, b) = do
  now <- liftIO getCurrentTime
  (PostID i) <- update' acid $ AddThread (t, b) now
  return $ fromIntegral i

reply :: AcidState Board -> (ByteString, ByteString) -> Int -> Method (Maybe Int)
reply acid (t, b) pid = do
  now <- liftIO getCurrentTime
  maybePostID <- update' acid $ AddReply (t, b) now (PostID $ fromIntegral pid)
  case maybePostID of
    Just (PostID i) -> return $ Just $ fromIntegral i
    Nothing -> return Nothing

getThreadIDs :: AcidState Board -> Method [Int]
getThreadIDs acid = do
  result <- query' acid GetThreads
  let ints = map unwrapPostID result
  return ints

unwrapPostID :: PostID -> Int
unwrapPostID (PostID i) = fromIntegral i

mkLabel ''Post

getPost :: AcidState Board -> Int -> Method (Maybe (ByteString, ByteString, [Int]))
getPost acid pid = do
  maybePost <- query' acid (GetPostByID $ PostID $ fromIntegral pid)
  case maybePost of
    Just post -> return $ Just (L.get title post, L.get body post, map unwrapPostID $ L.get replies post)
    Nothing   -> return Nothing
