{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module contains high-level functions for Telegram API methods.
-}

module Telegram.Methods where

import           Control.Monad.Freer        (Eff, Member, Members)
import           Control.Monad.Freer.Reader (Reader)
import           Control.Monad.Freer.State  (State, gets, modify)

import           Https                      (Https, ReqBodyJson, json, post,
                                             (/:))
import           Log                        (Log, logDebug, logInfo, showT)
import           Telegram.Config            (Config, baseUrl)
import           Telegram.Data              (Response (resp'result), Update)
import           Telegram.Parser            (chatId, updateId, (<?>))
import           Telegram.Requests          (GetUpdatesRequest (..),
                                             UpdateType (UpdateMessage))
import           Telegram.State             (SesMap, newSession)
import qualified Telegram.State             as Tg (State (..))


type Method r = Members
    [ Reader Config
    , State Tg.State
    , Https
    , Log
    ] r


{- | Use this method to get updates from Telegram server.
Incoming updates are stored on the server until the bot
receives them either way, but they will not be kept
longer than 24 hours.

You will receive JSON-serialized 'Update' objects as a result.
-}
getUpdates :: Method r => Eff r [Update]
getUpdates = do
    bUrl <- baseUrl
    body <- mkBody
    resp <- post (bUrl /: "getUpdates") body
    let ups = resp'result resp
    logInfo $ "Updates received: " <> showT (length ups)
    logDebug $ showT resp
    putNewOffset ups
    sessions <- putNewSessions ups
    logDebug $ showT sessions
    pure ups
  where
    mkBody :: Member (State Tg.State) r => Eff r (ReqBodyJson GetUpdatesRequest)
    mkBody = do
        offset <- gets Tg.st'offset
        pure . json $ GetUpdatesRequest
            { updates'offset = offset
            , updates'update_type = [UpdateMessage]
            , updates'timeout = Just 25
            }

    putNewOffset :: Member (State Tg.State) r => [Update] -> Eff r ()
    putNewOffset ups
        | null ups = pure ()
        | otherwise = let newOffset = fmap (+1) $ last ups <?> updateId
                      in modify (\st -> st { Tg.st'offset = newOffset })

    putNewSessions :: Member (State Tg.State) r => [Update] -> Eff r SesMap
    putNewSessions [] = gets Tg.st'sessions
    putNewSessions (u:us) = case u <?> chatId of
        Just cid -> modify (newSession cid) >> putNewSessions us
        _        -> putNewSessions us

{- | Use this method to send answers to callback
queries sent from inline keyboards. The answer will be
displayed to the user as a notification at the top of
the chat screen or as an alert. On success, True is returned.
-}
{- answerCallbackQuery
    :: Text -- ^ Callback id
    -> Maybe Text
    -> Maybe Bool
    -> Bot () -}

-- | Answer callback query issued after "\/repeat" command.
{- answerRepeatCallback
    :: Int -- ^ Chat id
    -> Int -- ^ Message id
    -> Text -- Callback id
    -> Text
    -> Bot ()
answerRepeatCallback cid mid cbid t = do -}

-- | Answer "\/repeat" command.
-- This function will also send inline keyboard to the user.
-- repeatCommand :: Int -> Bot ()
{-     repeatGreeting :: Text
    repeatGreeting = "Choose how many times you want me to repeat your messages"

    defaultRepeatInlineKeyboard :: InlineKeyboardMarkup
    defaultRepeatInlineKeyboard = InlineKeyboardMarkup
        [
            [ InlineKeyboardButton "1" (Just "1")
            , InlineKeyboardButton "2" (Just "2")
            , InlineKeyboardButton "3" (Just "3")
            ]
        ,
            [ InlineKeyboardButton "4" (Just "4")
            , InlineKeyboardButton "5" (Just "5")
            ]
        ] -}

{- | Use this method to edit text and game messages.
On success, if the edited message is not an inline message,
the edited 'Telegram.Internal.Types.Message' is returned, otherwise True is returned.
-}
{- editMessageText
    :: Int -- ^ Chat id
    -> Int -- ^ Message id
    -> Text
    -> Maybe InlineKeyboardMarkup
    -> Bot () -}

{- | Use this method to send text messages.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
{- sendText
    :: Int -- ^ Chat id
    -> Text
    -> Maybe InlineKeyboardMarkup
    -> Bot () -}

{- | Use this method to send static .WEBP or animated
 .TGS stickers. On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
{- sendSticker
    :: Int -- ^ Chat id
    -> Text -- Sticker id
    -> Bot () -}

{- | Use this method to send photos.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
{- sendPhoto
    :: Int -- ^ Chat id
    -> Text -- ^ Photo id
    -> Maybe Text -- ^ Caption
    -> Bot () -}

{- | Use this method to send animation files.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
{- sendAnimation
    :: Int -- ^ Chat id
    -> Text -- ^ Animation id
    -> Maybe Text -- ^ Caption
    -> Bot ()
-}

{- | Use this method to send audio files, if you want
Telegram clients to display them in the music player.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
{- sendAudio
    :: Int -- ^ Chat id
    -> Text -- ^ Audio id
    -> Maybe Text -- ^ Caption
    -> Bot () -}

{- | Use this method to send general files.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
{- sendDocument
    :: Int -- ^ Chat id
    -> Text -- ^ Document id
    -> Maybe Text -- ^ Caption
    -> Bot () -}

{- | Use this method to send video files, Telegram clients
support mp4 videos (other formats may be sent as 'Document').
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
{- sendVideo
    :: Int -- ^ Chat id
    -> Text -- ^ Video id
    -> Maybe Text -- ^ Caption
    -> Bot () -}

{- | As of v.4.0, Telegram clients support rounded square mp4
videos of up to 1 minute long. Use this method to send
video messages. On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
{- sendVideoNote
    :: Int -- ^ Chat id
    -> Text -- ^ VideoNote id
    -> Bot () -}

{- | Use this method to send audio files, if you want Telegram clients
to display the file as a playable voice message. For this to
work, your audio must be in an .OGG file encoded with OPUS
(other formats may be sent as Audio or Document). On success,
the sent 'Telegram.Internal.Types.Message' is returned.
-}
{- sendVoice
    :: Int -- ^ Chat id
    -> Text -- ^ Voice id
    -> Maybe Text -- ^ Caption
    -> Bot () -}

-- | Simple abstraction above all methods sending media with 'Caption'.
{- sendMediaWithCaption
    :: (Show response)
    => (body -> ClientM response) -- ^ Bot method
    -> body -- ^ Request body
    -> Text -- ^ Name of method (for logging)
    -> Int -- ^ Destination 'ChatId'
    -> Bot ()
 -}
