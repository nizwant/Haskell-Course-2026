-- | Temporary smoke test for the FFI layer: register with the server, then
-- print whatever arrives for a few seconds. Replaced by the real TUI later.
module Main where

import Control.Monad (forM_)
import Data.Text qualified as T
import PeerChat.FFI qualified as Net
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [user, password] -> run (T.pack user) (T.pack password)
    _ -> putStrLn "usage: PeerChat <username> <password>"

run :: T.Text -> T.Text -> IO ()
run user password = do
  ok <- Net.connect user password
  putStrLn ("connect: " ++ show ok)

  fd <- Net.getFd
  putStrLn ("socket fd: " ++ show fd)

  -- Ten one-second windows; each either yields an event or times out.
  forM_ [1 :: Int .. 10] $ \_ -> do
    event <- Net.receive 1000
    print event

  Net.disconnect
  putStrLn "disconnected"
