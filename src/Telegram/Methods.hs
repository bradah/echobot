{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Telegram API methods.
-}

module Telegram.Methods
    ( -- * Available methods
      Method
      -- ** getMe
    , getMe
      -- ** Getting updates
    , getUpdates
      -- ** Callbacks
    , answerRepeatCallback
      -- ** Answering commands
    , repeatCommand
    , startCommand
      -- ** Updating messages
    , editMessageText
      -- ** Send text
    , sendMessage
      -- ** Send sticker
    , sendSticker
      -- ** Send media
    , sendPhoto
    , sendAnimation
    , sendAudio
    , sendDocument
    , sendVideo
    , sendVideoNote
    , sendVoice
    ) where

import           AppState
import           Control.Monad              (replicateM)
import           Control.Monad.Freer        (Eff, Member, Members)
import           Control.Monad.Freer.Reader (Reader, asks)
import           Control.Monad.Freer.State  (State, gets, modify)
import           Data.Aeson                 (ToJSON)
import           Data.Text                  (Text, unpack)
import           Eff.Https
import           Eff.Log                    (HasCallStack, Log, logDebug,
                                             logInfo, (<+>))
import           Telegram.Config            (Config (initialRepNum, startMessage),
                                             baseUrl)
import           Telegram.Data
import           Telegram.Parser
import           Telegram.Requests

-- | Simple shortcut for methods constraints
type Method r = Members
    [ Reader Config
    , State (AppState (Maybe Int))
    , Https
    , Log
    ] r


type MethodWithCallStack r = (Method r, HasCallStack)

{- | A simple method for testing your bot's auth token.
Requires no parameters. Returns basic information about
the bot in form of a 'User' object.
-}
getMe :: MethodWithCallStack r => Eff r (Response User)
getMe = do
    bUrl <- baseUrl
    get (bUrl /: "getMe")

{- | Use this method to get updates from Telegram server.
Incoming updates are stored on the server until the bot
receives them either way, but they will not be kept
longer than 24 hours.

You will receive JSON-serialized 'Update' objects as a result.
-}
getUpdates :: MethodWithCallStack r => Eff r (Response [Update])
getUpdates = do
    bUrl <- baseUrl
    body <- mkBody
    logInfo "Waiting for updates..."
    resp <- post (bUrl /: "getUpdates") (json body)
    let ups = resp'result resp
    logInfo $ "Updates received: " <+> length ups
    logDebug $ "Response: " <+> resp
    putNewOffset ups
    sessions <- putNewSessions ups
    logDebug $ "Current sessions: " <+> sessions
    pure resp
  where
    mkBody :: Member (State TgState) r => Eff r GetUpdatesRequest
    mkBody = do
        offset <- gets st'offset
        pure $ GetUpdatesRequest
            { updates'offset = offset
            , updates'update_type = [UpdateMessage]
            , updates'timeout = Just 25
            }

    putNewOffset :: Member (State TgState) r => [Update] -> Eff r ()
    putNewOffset ups
        | null ups = pure ()
        | otherwise = let newOffset = fmap (+1) $ last ups <?> updateId
                      in modify (\st -> st { st'offset = newOffset })

    putNewSessions :: Members [State TgState, Reader Config] r => [Update] -> Eff r SesMap
    putNewSessions [] = (gets @TgState) st'sessions
    putNewSessions (u:us) = do
        n <- asks initialRepNum
        case u <?> chatId of
            Just cid -> modify @TgState (newSession cid n) >> putNewSessions us
            _        -> putNewSessions us

{- | Use this method to send answers to callback
queries sent from inline keyboards. The answer will be
displayed to the user as a notification at the top of
the chat screen or as an alert. On success, True is returned.
-}
answerCallbackQuery :: MethodWithCallStack r
                    => Text -- ^ Callback id
                    -> Maybe Text -- Text
                    -> Maybe Bool -- ^ Show alert
                    -> Eff r (Response Bool)
answerCallbackQuery cbId mt mb = do
    logInfo $ "Answering callback " <+> cbId <> " with text " <+> mt
    bUrl <- baseUrl
    resp <- post (bUrl /: "answerCallbackQuery") body
    logDebug $ "Response: " <+> resp
    pure resp
  where body = json $ AnswerCallbackRequest
            { callback'callback_query_id = cbId
            , callback'text = mt
            , callback'show_alert = mb
            }

-- | Answer callback query issued after "\/repeat" command.
answerRepeatCallback :: MethodWithCallStack r
                     => Int -- ^ Chat id
                     -> Int -- ^ Message id
                     -> Text -- ^ Callback id
                     -> Text
                     -> Eff r (Response Bool)
answerRepeatCallback cid mid cbid t = do
    resp <- answerCallbackQuery cbid Nothing Nothing
    let n = read $ unpack t
        newText = confirmationText n
    _ <- editMessageText cid mid newText Nothing
    logDebug $ "Changing repetition number for " <+> cid <> " to " <+> t
    (modify @TgState) (changeRepNum cid n)
    pure resp
  where
    confirmationText :: Int -> Text
    confirmationText n =
        "I will repeat your messages "
        <+> n
        <> " time"
        <> if n > 1 then "s." else "."

-- | Answer "\/repeat" command.
-- This function will also send inline keyboard to the user.
repeatCommand :: MethodWithCallStack r
              => Int -- ^ Chat id
              -> Eff r (Response Message)
repeatCommand cid = do
    logInfo "Received /repeat command"
    logDebug $ "Sending inline keyboard "
        <+> defaultRepeatInlineKeyboard <> " to " <+> cid
    bUrl <- baseUrl
    resp <- post (bUrl /: "sendMessage") body
    logDebug $ "Response: " <+> resp
    pure resp
  where
    body = json $ SendMessageRequest
        { sendMsg'chat_id = cid
        , sendMsg'text = repeatGreeting
        , sendMsg'reply_markup = Just defaultRepeatInlineKeyboard
        }
    repeatGreeting :: Text
    repeatGreeting = "Choose how many times you want me to repeat your messages."

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
        ]

-- | Simple wrapper around 'sendMessage' to answer \/start command.
startCommand :: MethodWithCallStack r
             => Int -- ^ Chat id
             -> Eff r (Response Message)
startCommand cid = do
    t <- asks startMessage
    sendMessage cid t Nothing

{- | Use this method to edit text and game messages.
On success, if the edited message is not an inline message,
the edited 'Message' is returned, otherwise True is returned.
-}
editMessageText :: MethodWithCallStack r
                => Int -- ^ Chat id
                -> Int -- ^ Message id
                -> Text -- ^ New text
                -> Maybe InlineKeyboardMarkup
                -> Eff r (Response Message)
editMessageText cid mid t mMarkup = do
    logInfo $ "Editing message " <+> mid <> " at chat " <+> cid
        <> " to " <+> t <> " with markup " <+> mMarkup
    bUrl <- baseUrl
    resp <- post (bUrl /: "editMessageText") body
    logDebug $ "Response: " <+> resp
    pure resp
  where body = json $ EditMessageTextRequest
            { editMsgText'chat_id = cid
            , editMsgText'message_id = mid
            , editMsgText'text = t
            , editMsgText'reply_markup = mMarkup
            }

-- | Helper function for defining repeatable methods.
-- On success the first answer will be returned.
methodWithRepNum :: ( MethodWithCallStack r
                    , ToJSON b
                    , Show b
                    )
                 => Int -- ^ Chat id
                 -> Text -- ^ Endopoint (e.g. "sendMessage")
                 -> b -- ^ Request body
                 -> Text -- ^ Hint for logging
                 -> Eff r (Response Message)
methodWithRepNum cid endpoint body hint = do
    repeatNum <- (gets @TgState) $ getRepNum cid
    logInfo $ "Sending " <> hint <> " to chat " <+> cid
        <> " (repeat: " <+> repeatNum <> ")"
    logDebug $ "Request body: " <+> body
    bUrl <- baseUrl
    resp <- head <$> replicateM repeatNum (post (bUrl /: endpoint) (json body))
    logDebug $ "Response: " <+> resp
    pure resp

{- | Use this method to send text messages.
On success, the sent 'Message' is returned.
-}
sendMessage :: MethodWithCallStack r
            => Int -- ^ Chat id
            -> Text
            -> Maybe InlineKeyboardMarkup
            -> Eff r (Response Message)
sendMessage cid t mMarkup = methodWithRepNum cid "sendMessage" body "text"
  where body = SendMessageRequest
            { sendMsg'chat_id = cid
            , sendMsg'text = t
            , sendMsg'reply_markup = mMarkup
            }

{- | Use this method to send static .WEBP or animated
 .TGS stickers. On success, the sent 'Message' is returned.
-}
sendSticker :: MethodWithCallStack r
            => Int -- ^ Chat id
            -> Text -- ^ Sticker id
            -> Eff r (Response Message)
sendSticker cid fid = methodWithRepNum cid "sendSticker" body "sticker"
  where body = SendStickerRequest
            { sendStk'chat_id = cid
            , sendStk'sticker = fid
            }

{- | Use this method to send photos.
On success, the sent 'Message' is returned.
-}
sendPhoto :: MethodWithCallStack r
          => Int -- ^ Chat id
          -> Text -- ^ Photo id
          -> Maybe Text -- ^ Caption
          -> Eff r (Response Message)
sendPhoto cid pid cap = methodWithRepNum cid "sendPhoto" body "photo"
  where body = SendPhotoRequest
            { sendPh'chat_id = cid
            , sendPh'photo = pid
            , sendPh'caption = cap
            }

{- | Use this method to send animation files.
On success, the sent 'Message' is returned.
-}
sendAnimation :: MethodWithCallStack r
              => Int -- ^ Chat id
              -> Text -- ^ Animation id
              -> Maybe Text -- ^ Caption
              -> Eff r (Response Message)
sendAnimation cid anId cap = methodWithRepNum cid "sendAnimation" body "animation"
  where body = SendAnimationRequest
            { sendAnim'chat_id = cid
            , sendAnim'animation = anId
            , sendAnim'caption = cap
            }

{- | Use this method to send audio files, if you want
Telegram clients to display them in the music player.
On success, the sent 'Message' is returned.
-}
sendAudio :: MethodWithCallStack r
          => Int -- ^ Chat id
          -> Text -- ^ Audio id
          -> Maybe Text -- ^ Caption
          -> Eff r (Response Message)
sendAudio cid audId cap = methodWithRepNum cid "sendAudio" body "audio"
  where body = SendAudioRequest
            { sendAudio'chat_id = cid
            , sendAudio'audio = audId
            , sendAudio'caption = cap
            }
{- | Use this method to send general files.
On success, the sent 'Message' is returned.
-}
sendDocument :: MethodWithCallStack r
             => Int -- ^ Chat id
             -> Text -- ^ Document id
             -> Maybe Text -- ^ Caption
             -> Eff r (Response Message)
sendDocument cid dId cap = methodWithRepNum cid "sendDocument" body "document"
  where body = SendDocumentRequest
            { sendDoc'chat_id = cid
            , sendDoc'document = dId
            , sendDoc'caption = cap
            }

{- | Use this method to send video files, Telegram clients
support mp4 videos (other formats may be sent as 'Document').
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendVideo :: MethodWithCallStack r
          => Int -- ^ Chat id
          -> Text -- ^ Video id
          -> Maybe Text -- ^ Caption
          -> Eff r (Response Message)
sendVideo cid vid cap = methodWithRepNum cid "sendVideo" body "video"
  where body = SendVideoRequest
            { sendVideo'chat_id = cid
            , sendVideo'video = vid
            , sendVideo'caption = cap
            }

{- | As of v.4.0, Telegram clients support rounded square mp4
videos of up to 1 minute long. Use this method to send
video messages. On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendVideoNote :: MethodWithCallStack r
              => Int -- ^ Chat id
              -> Text -- ^ VideoNote id
              -> Eff r (Response Message)
sendVideoNote cid vnId = methodWithRepNum cid "sendVideoNote" body "videonote"
  where body = SendVideoNoteRequest
            { sendVideoNote'chat_id = cid
            , sendVideoNote'video_note = vnId
            }

{- | Use this method to send audio files, if you want Telegram clients
to display the file as a playable voice message. For this to
work, your audio must be in an .OGG file encoded with OPUS
(other formats may be sent as Audio or Document). On success,
the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendVoice :: MethodWithCallStack r
          => Int -- ^ Chat id
          -> Text -- ^ Photo id
          -> Maybe Text -- ^ Caption
          -> Eff r (Response Message)
sendVoice cid vid cap = methodWithRepNum cid "sendVoice" body "voice"
  where body = SendVoiceRequest
            { sendVoice'chat_id = cid
            , sendVoice'voice = vid
            , sendVoice'caption = cap
            }

