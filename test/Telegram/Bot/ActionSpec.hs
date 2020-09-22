{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.ActionSpec where

import           Data.Maybe
import           Telegram.Bot.Action
import           Telegram.Types.Arbitrary ()
import           Telegram.UpdateParser
import           Test.Hspec
import           Test.Hspec.QuickCheck

updateToActionSpec :: Spec
updateToActionSpec =
  context "when given an Update" $
    prop "determines proper bot action" $
      \arbU -> case updateToAction arbU of
        Just (Start cid) ->
          isJust (command "start" <?> arbU)
          && Just cid == (updateChatId <?> arbU)
        Just (EchoText cid t) ->
          Just cid == (updateChatId <?> arbU)
          && Just t == (text <?> arbU)
        Just (EchoSticker cid fid) ->
          Just cid == (updateChatId <?> arbU)
          && Just fid == (sticker <?> arbU)
        Nothing -> (command "start" <?> arbU) == Nothing
          && (text <?> arbU) == Nothing
          && (sticker <?> arbU) == Nothing

spec :: Spec
spec = do
  describe "updateToAction" updateToActionSpec
