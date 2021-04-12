module Vk.ParserSpec where

import           Control.Monad         ((>=>))
import           Data.Maybe
import qualified Data.Text             as T (isPrefixOf, tail, unwords, words)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Vk.Data
import           Vk.Data.Arbitrary     ()
import           Vk.Parser


textSpec :: Spec
textSpec =
  context "when given an Update with text" $
    prop "extracts text from Update" $
      \arbU -> case pure . upd'object >=> obj'message >=> pure . msg'text $ arbU of
        Nothing -> isNothing (arbU <?> text)
        Just t  -> (arbU <?> text) == Just t

commandSpec :: Spec
commandSpec =
  context "when given an Update with command" $
    prop "returns ()" $
      \arbU arbT -> case pure . upd'object >=> obj'message >=> pure . msg'text $ arbU of
        Nothing -> isNothing (arbU <?> command arbT)
        Just t -> if "/" `T.isPrefixOf` t
          then case T.words t of
            (w:ws) -> (arbU <?> command (T.tail w)) == Just ()
          else isNothing (arbU <?> command arbT)

updateUserIdSpec :: Spec
updateUserIdSpec =
  context "when given an Update with Message" $
    prop "extracts Message's from_id" $
      \arbU -> case pure . upd'object >=> obj'message >=> msg'from_id $ arbU of
        Nothing -> isNothing (arbU <?> updateUserId)
        uid     -> (arbU <?> updateUserId) == uid

attachmentsSpec :: Spec
attachmentsSpec =
    context "when given an Update with Message" $
        prop "extracts Message's attachments" $
        \arbU -> case pure . upd'object >=> obj'message >=> pure . msg'attachments $ arbU of
            Nothing   -> isNothing (arbU <?> attachments)
            Just atts -> (arbU <?> attachments) == Just atts

payloadSpec :: Spec
payloadSpec =
    context "when given an Update with payload field" $
        prop "extracts it" $
        \arbU -> case pure . upd'object >=> obj'message >=> msg'payload $ arbU of
            Nothing -> isNothing (arbU <?> payload)
            p       -> (arbU <?> payload) == p

stickerSpec :: Spec
stickerSpec =
  context "when given an Update with Sticker" $
    prop "extracts sticker_id" $
    \arbU -> case pure . upd'object >=> obj'message >=> pure . msg'attachments $ arbU of
        Just [Attachment Sticker Media{..}] -> (arbU <?> sticker) == pure (fromMaybe 0 media'sticker_id)
        _ -> isNothing (arbU <?> sticker)

isAudioMessageSpec :: Spec
isAudioMessageSpec =
  context "when given an Update with audio_message" $
    prop "returns ()" $
    \arbU -> case pure . upd'object >=> obj'message >=> pure . msg'attachments $ arbU of
        Just [Attachment AudioMessage _] -> (arbU <?> isAudioMessage) == pure ()
        _                                -> isNothing (arbU <?> isAudioMessage)

spec :: Spec
spec = do
    describe "text" textSpec
    describe "command" commandSpec
    describe "updateUserId" updateUserIdSpec
    describe "attachments" attachmentsSpec
    describe "payload" payloadSpec
    describe "sticker" stickerSpec
    describe "isAudioMessage" isAudioMessageSpec
