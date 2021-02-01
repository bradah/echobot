{-# LANGUAGE OverloadedStrings #-}

module Telegram.Methods where

import           Colog
import           Control.Monad.State
import qualified Data.HashMap.Lazy         as Map
import           Data.Text                 (Text, unpack)
import           Servant.Client

import           API.Utils
import           Telegram.Internal.Bot
import qualified Telegram.Internal.Methods as Int
import           Telegram.Internal.Request
import           Telegram.Internal.Types
import           Telegram.UpdateParser

liftClient :: ClientM a -> Bot a
liftClient = Bot . lift . lift

getUpdates :: Bot [Update]
getUpdates = do
    logInfo "Waiting for updates..."
    body <- mkBody
    resp <- liftClient $ Int.getUpdates body
    let ups = responseResult resp
    logInfo $ "Updates received: " <> showP (length ups)
    logDebug $ "Response: " <> showP resp
    modify (\st -> st {bStateUid = getNewUid ups})
    conversations <- addConversations ups
    modify (\st -> st {bStateConversations = conversations})
    logDebug $ "Current conversations: " <> showP conversations
    return ups

  where
    mkBody :: Bot GetUpdatesBody
    mkBody = do
        uid <- gets bStateUid
        return $ GetUpdatesBody uid [UpdateMessage] (Just 25)

    getNewUid :: [Update] -> Maybe UpdateId
    getNewUid ups
        | null ups = Nothing
        | otherwise = (1+) <$> updateId <?> last ups

    addConversations :: [Update] -> Bot ConvMap
    addConversations ups = do
        convMap <- gets bStateConversations
        let cIds = extractChatId ups
        return $ insertIfNew cIds convMap
      where
        insertIfNew :: [ChatId] -> ConvMap -> ConvMap
        insertIfNew [] convs = convs
        insertIfNew (c:cs) convs =
            if not $ c `Map.member` convs
                then
                    Map.insert
                        c
                        (Conversation defaultRepeatNumber)
                        convs
                else
                    insertIfNew cs convs

        extractChatId :: [Update] -> [ChatId]
        extractChatId [] = []
        extractChatId (u:us) = case updateChatId <?> u of
            Just cid -> cid : extractChatId us
            Nothing  -> extractChatId us


sendText :: ChatId -> Text -> Maybe InlineKeyboardMarkup -> Bot ()
sendText cid t markup = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending text "
        <> showP t
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendMessage body)
    logDebug $ "Response: " <> showP resp
  where
    body = SendMessageBody cid t markup

sendSticker :: ChatId -> FileId -> Bot ()
sendSticker cid fid = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending sticker "
        <> showP fid
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendSticker body)
    logDebug $ "Response: " <> showP resp
  where
    body = SendStickerBody cid fid

answerRepeatCallback :: ChatId -> MessageId -> CallbackId -> Text -> Bot ()
answerRepeatCallback cid mid cbid t = do
    answerCallbackQuery cbid Nothing Nothing
    let n = read $ unpack t
        newText = repeatNewText n
    editMessageText cid mid newText Nothing
    logDebug $ "Changing repetition number for "
        <> showP cid
        <> " to "
        <> showP t
    modify (changeRepeatNumber cid n)

editMessageText :: ChatId -> MessageId -> Text -> Maybe InlineKeyboardMarkup -> Bot ()
editMessageText cid mid t mMarkup = do
    logInfo $ "Editing message "
        <> showP mid
        <> " at chat "
        <> showP cid
        <> " to "
        <> showP t
        <> " with markup "
        <> showP mMarkup
    resp <- liftClient $ Int.editMessageText body
    logDebug $ "Response: " <> showP resp
  where
    body = EditMessageTextBody cid mid t mMarkup

answerCallbackQuery :: CallbackId -> Maybe Text -> Maybe Bool -> Bot ()
answerCallbackQuery cbId mt mb = do
    logInfo $ "Answering callback"
        <> showP cbId
        <> " with text "
        <> showP mt
    resp <- liftClient $ Int.answerCallbackQuery body
    logDebug $ "Response: " <> showP resp
  where
    body = AnswerCallbackBody cbId mt mb

repeatCommand :: ChatId -> Bot ()
repeatCommand cid = do
    logInfo "Received /repeat command"
    logDebug $ "Sending inline keyboard "
        <> showP defaultRepeatInlineKeyboard
        <> " to "
        <> showP cid
    resp <- liftClient $ Int.sendMessage body
    logDebug $ "Reponse: " <> showP resp
  where
    body = SendMessageBody cid repeatGreeting (Just defaultRepeatInlineKeyboard)

defaultRepeatInlineKeyboard :: InlineKeyboardMarkup
defaultRepeatInlineKeyboard = InlineKeyboardMarkup
    [
        [ InlineKeyboardButton "1️⃣" (Just "1")
        , InlineKeyboardButton "2️⃣" (Just "2")
        , InlineKeyboardButton "3️⃣" (Just "3")
        ]
    ,
        [ InlineKeyboardButton "4️⃣" (Just "4")
        , InlineKeyboardButton "5️⃣" (Just "5")
        ]
    ]

repeatGreeting :: Text
repeatGreeting = "Choose how many times you want me to repeat your messages"

repeatNewText :: Int -> Text
repeatNewText n
    | n == 1     = "Okay, I won't piss you off...😒"
    | otherwise  = "From now on I will repeat your messages "
        <> showT n
        <> " times 👌🏾"

sendPhoto :: ChatId -> FileId -> Caption -> Bot ()
sendPhoto cid fid cap = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending photo "
        <> showP fid
        <> " with caption "
        <> showP cap
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendPhoto body)
    logDebug $ "Response " <> showP resp
  where
    body = SendPhotoBody cid fid cap

sendAnimation :: ChatId -> FileId -> Caption -> Bot ()
sendAnimation cid fid cap = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending animation "
        <> showP fid
        <> " with caption "
        <> showP cap
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendAnimation body)
    logDebug $ "Response " <> showP resp
  where
    body = SendAnimationBody cid fid cap

sendAudio :: ChatId -> FileId -> Caption -> Bot ()
sendAudio cid fid cap = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending audio "
        <> showP fid
        <> " with caption "
        <> showP cap
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendAudio body)
    logDebug $ "Response " <> showP resp
  where
    body = SendAudioBody cid fid cap

sendDocument :: ChatId -> FileId -> Caption -> Bot ()
sendDocument cid fid cap = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending document "
        <> showP fid
        <> " with caption "
        <> showP cap
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendDocument body)
    logDebug $ "Response " <> showP resp
  where
    body = SendDocumentBody cid fid cap

sendVideo :: ChatId -> FileId -> Caption -> Bot ()
sendVideo cid fid cap = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending video "
        <> showP fid
        <> " with caption "
        <> showP cap
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendVideo body)
    logDebug $ "Response " <> showP resp
  where
    body = SendVideoBody cid fid cap

sendVideoNote :: ChatId -> FileId -> Bot ()
sendVideoNote cid fid = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending videonote "
        <> showP fid
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendVideoNote body)
    logDebug $ "Response " <> showP resp
  where
    body = SendVideoNoteBody cid fid

sendVoice :: ChatId -> FileId -> Caption -> Bot ()
sendVoice cid fid cap = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending voice message "
        <> showP fid
        <> " with caption "
        <> showP cap
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendVoice body)
    logDebug $ "Response " <> showP resp
  where
    body = SendVoiceBody cid fid cap
