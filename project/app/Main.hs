{-# LANGUAGE OverloadedStrings #-}

-- | Command line entry point.
module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import PeerChat.App
import System.Environment (getArgs, setEnv)
import System.Exit (die)

usage :: String
usage =
  unlines
    [ "usage: PeerChat --user <name> --password <pass> [--server <ip>] [--db <path>]",
      "",
      "  --server defaults to the address compiled into the C library.",
      "  --db defaults to <user>.db in the current directory.",
      "",
      "keys: Enter send   Up/Down switch contact   PgUp/PgDn scroll   Esc quit",
      "      /connect <user> <password>   find a peer through the server",
      "      /quit                        leave"
    ]

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> die (err ++ "\n\n" ++ usage)
    Right (user, password, server, database) -> do
      -- The C library reads the server address from the environment, so this
      -- has to be set before PeerChat.App registers.
      mapM_ (setEnv "PEERCHAT_SERVER_IP") server
      runApp
        Options
          { optUser = T.pack user,
            optPassword = T.pack password,
            optDatabase = fromMaybe (user ++ ".db") database
          }

-- | Minimal @--flag value@ parsing; the program only has four options.
parseArgs :: [String] -> Either String (String, String, Maybe String, Maybe FilePath)
parseArgs = go Nothing Nothing Nothing Nothing
  where
    go (Just u) (Just p) server database [] = Right (u, p, server, database)
    go Nothing _ _ _ [] = Left "missing --user"
    go _ Nothing _ _ [] = Left "missing --password"
    go _ p server database ("--user" : v : rest) = go (Just v) p server database rest
    go u _ server database ("--password" : v : rest) = go u (Just v) server database rest
    go u p _ database ("--server" : v : rest) = go u p (Just v) database rest
    go u p server _ ("--db" : v : rest) = go u p server (Just v) rest
    go _ _ _ _ (flag : _) = Left ("unrecognised argument: " ++ flag)
