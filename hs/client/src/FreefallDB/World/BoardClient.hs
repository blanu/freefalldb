module FreefallDB.World.BoardClient
(
  getThreadIDs,
  makeThread,
  reply,
  getPost
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Network.MessagePackRpc.Client

makeThread :: (ByteString, ByteString) -> Client (Int)
makeThread = call "makeThread"

reply :: (ByteString, ByteString) -> Int -> Client (Maybe Int)
reply = call "reply"

getThreadIDs :: Client [Int]
getThreadIDs = call "getThreadIDs"

getPost :: Int -> Client (ByteString, ByteString, [Int])
getPost = call "getPost"
