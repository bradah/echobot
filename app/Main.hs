{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO
import           Telegram.Bot

main :: IO ()
main = do
    token <- TIO.putStrLn "Enter your bot token" >> TIO.getLine
    go token Nothing
    where
        processUpdates :: Token -> [Update] -> [IO (Response (Message))]
        processUpdates _ []     = []
        processUpdates token (u:us) = 
            let u' = do
                case (handleUpdate >=> handleAction token >=> (\r -> return $ threadDelay 500000 >> r)) u of
                    Just ioact -> ioact
                    Nothing    -> fail "processUpdates: failed"
            in u' : processUpdates token us

        go :: Token -> Maybe UpdateId -> IO ()
        go token uid = do
            resp <- getUpdates token $ GetUpdatesRequest uid (Just [UpdateMessage])
            ups <- return $ responseResult resp
            mapM_ id (processUpdates token ups)
            let nextUid = case ups of
                    [] -> (+1) <$> uid
                    _  -> return . (+1) . updateUpdateId . last $ ups
            go token nextUid


initialGetUpdateRequest :: GetUpdatesRequest
initialGetUpdateRequest = GetUpdatesRequest Nothing (Just [UpdateMessage])

data Action
    = NoAction
    | Start ChatId
    | EchoText ChatId Text
    | EchoSticker ChatId Sticker

handleUpdate :: Update -> Maybe Action
handleUpdate = runUpdateParser $ asum
    [ Start <$ command "start" <*> updateChatId
    , EchoText <$> updateChatId <*> text
    , EchoSticker <$> updateChatId <*> sticker
    ]

handleAction :: Token -> Action -> Maybe (IO (Response (Message)))
handleAction token action = case action of
    Start cid         -> return $ replyText token cid "Don't support this command yet :("
    EchoText cid t    -> return $ replyText token cid t
    EchoSticker cid s -> return $ replySticker token cid s
