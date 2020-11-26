{-# LANGUAGE OverloadedStrings #-}
module Main where

import           VK.Bot
import           VK.Config
import           VK.Env

main :: IO ()
main = do
  conf <- load
  env <- mkEnv conf
  print env
  runBot env
