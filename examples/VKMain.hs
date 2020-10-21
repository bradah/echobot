{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           VK.Env
import           VK.Methods

main :: IO ()
main = do
  env <- mkEnv
  print env
  ups <- getUpdates env
  print $ encode ups
