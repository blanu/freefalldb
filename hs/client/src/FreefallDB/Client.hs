{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import Network.MessagePackRpc.Client
import Control.Monad.IO.Class (liftIO)
import System.Environment

import FreefallDB.World.BoardClient

main :: IO()
main = do
  args <- getArgs
  case args of
    ("list":[]) -> runClient "localhost" 5894 listThreads
    ("new":title:body:[]) -> runClient "localhost" 5894 $ post (C.pack title) (C.pack body)
    ("reply":pids:title:body:[]) -> runClient "localhost" 5894 $ postReply (read pids::Int) (C.pack title) (C.pack body)
    ("read":pids:[]) -> runClient "localhost" 5894 $ readPost (read pids::Int)
    otherwise -> help

listThreads = do
  threads <- getThreadIDs
  mapM_ printThread threads

printThread thread = do
  (title,_,replies) <- getPost thread
  if length replies > 0
    then liftIO $ putStrLn $ (show thread) ++ ": " ++ C.unpack title ++ " (" ++ show (length replies) ++ " replies)"
    else liftIO $ putStrLn $ (show thread) ++ ": " ++ C.unpack title

post title body = do
  result <- makeThread (title, body)
  liftIO $ print result  

postReply pid title body = do
  result <- reply (title, body) pid
  liftIO $ print result

readPost pid = do
  (title,body,replies) <- getPost pid
  liftIO $ putStrLn $ "Title: " ++ C.unpack title
  liftIO $ putStrLn ""
  liftIO $ putStrLn $ C.unpack body
  if length replies > 0
    then do
      liftIO $ putStrLn ""
      liftIO $ putStrLn $ "Replies: " ++ show replies
    else return ()

help = do
  putStrLn "client list -- list threads"
  putStrLn "client new [title] [body] -- new post in new thread"
  putStrLn "client reply [postid] [title] [body] -- reply to a post"
  putStrLn "client read [postid] -- read post"

