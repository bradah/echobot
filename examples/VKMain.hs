{-# LANGUAGE OverloadedStrings #-}
module Main where

import           VK.Bot
import           VK.Config
import           VK.Env
import           VK.Methods

main :: IO ()
main = do
  conf <- load
  env <- mkEnv conf
  print env
  ups <- checkLPS env
  print ups
