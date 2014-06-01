{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}

module FreefallDB.World.BoardClientLibrary
(
  getThreadIDs,
  makeThread,
  reply,
  getPost
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.MessagePackRpc.Client
import System.IO.Unsafe
import Foreign.C.String
import Foreign.C.Types

import qualified FreefallDB.World.BoardClient as BC

makeThread :: CString -> CString -> IO CDouble
makeThread title body = do
  title' <- B.packCString title
  body' <- B.packCString body
  i <- runClientResult "166.78.129.122" 5894 $ BC.makeThread (title', body')
  return $ CDouble $ fromIntegral i

reply :: (ByteString, ByteString) -> Int -> (Maybe Int)
reply post postid = do
  unsafePerformIO $ runClientResult "166.78.129.122" 5894 $ BC.reply post postid

getThreadIDs :: [Int]
getThreadIDs = do
  unsafePerformIO $ runClientResult "166.78.129.122" 5894 $ BC.getThreadIDs
  
getPost :: Int -> (ByteString, ByteString, [Int])
getPost postid = do
  unsafePerformIO $ runClientResult "166.78.129.122" 5894 $ BC.getPost postid

foreign export stdcall makeThread :: CString -> CString -> IO CDouble
