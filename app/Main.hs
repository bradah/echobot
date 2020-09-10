{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Maybe
import qualified Data.Text.IO          as TIO
import           Telegram.Methods
import           Telegram.Request
import           Telegram.Types
import           Telegram.UpdateParser

main :: IO ()
main = do
    token <- TIO.putStrLn "Enter your bot token" >> TIO.getLine
    go token 0
    where
        decide :: [Update] -> [SendMessageRequest]
        decide [] = []
        decide (u:us) = case updateMessageText u of
            Just t -> SendMessageRequest
                (chatId . messageChat . fromJust . updateMessage $ u)
                t : decide us
            Nothing -> []

        go :: Token -> UpdateId -> IO ()
        go token uid = do
            ups <- responseResult <$> (getUpdates token $ GetUpdatesRequest (Just uid) Nothing)
            reqbodies <- return $ decide ups
            mapM_ (sendMessage token >=> (\_ -> threadDelay 250000)) reqbodies
            let nextUid = case ups of
                    [] -> 1 + uid
                    _  -> (+1) . updateUpdateId . last $ ups
            go token nextUid


initialGetUpdateRequest :: GetUpdatesRequest
initialGetUpdateRequest = GetUpdatesRequest Nothing Nothing

