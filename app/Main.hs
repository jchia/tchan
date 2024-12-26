{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ClassyPrelude
import Control.Concurrent (threadDelay)

test1 :: IO ()
test1 = do
  say "Starting TEST 1"
  wchan <- newBroadcastTChanIO
  writer <- async $ do
    forM_ [0..9] $ \n -> do
      atomically $ writeTChan wchan n
      say $ "Writer wrote: " <> tshow n
      threadDelay 500000

  reader1 <- async $ do
    threadDelay 3000000
    rchan <- atomically $ dupTChan wchan
    x1 <- atomically $ readTChan rchan
    say $ "Reader 1 read: " <> tshow x1
    x2 <- atomically $ readTChan rchan
    say $ "Reader 1 read: " <> tshow x2

  reader2 <- async $ do
    threadDelay 3000000
    rchan <- atomically $ dupTChan wchan
    x1 <- atomically $ readTChan rchan
    say $ "Reader 2 read: " <> tshow x1
    x2 <- atomically $ readTChan rchan
    say $ "Reader 2 read: " <> tshow x2

  traverse_ wait [reader1, reader2, writer]
  say "Finished TEST 1"

test2 :: IO ()
test2 = do
  say "Starting TEST 2"
  wchan <- newBroadcastTChanIO
  writer <- async $ do
    say "Writer starting"
    forM_ [0..2147483647] $ \n -> do
      atomically $ writeTChan wchan n
      when (n `mod` 10000000 == 0) $
        say $ "Writer progress: " <> tshow n
    say "Writer finished"

  reader1 <- async $ do
    threadDelay 10000000
    rchan <- atomically $ dupTChan wchan
    replicateM_ 10 $ do
      x <- atomically $ readTChan rchan
      say $ "Reader1 read: " <> tshow x

  reader2 <- async $ do
    threadDelay 20000000
    rchan <- atomically $ dupTChan wchan
    replicateM_ 10 $ do
      x <- atomically $ readTChan rchan
      say $ "Reader2 read: " <> tshow x

  traverse_ wait [reader1, reader2, writer]
  say "Finished TEST 2"

main :: IO ()
main = do
  test1
  say ""
  test2
