{-# LANGUAGE OverloadedStrings #-}
module Telegram.Types.Arbitrary where

import           Telegram.Types
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary Update where
  arbitrary = Update
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Message where
  arbitrary = Message
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> oneof
      [ arbitrary
      , (fmap ("/" <>)) <$> arbitrary
      ]
    <*> arbitrary
    <*> arbitrary


instance Arbitrary User where
  arbitrary = User
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


instance Arbitrary Chat where
  arbitrary = Chat
    <$> arbitrary
    <*> arbitrary

instance Arbitrary MessageEntity where
  arbitrary = MessageEntity
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary MessageEntityType where
  arbitrary = oneof $ return <$>
    [ MessageEntityMention
    , MessageEntityHashtag
    , MessageEntityBotCommand
    , MessageEntityUrl
    , MessageEntityEmail
    , MessageEntityBold
    , MessageEntityItalic
    , MessageEntityUnderline
    , MessageEntityStrikethrough
    , MessageEntityCode
    , MessageEntityPre
    , MessageEntityTextLink
    , MessageEntityTextMention
    , MessageEntityCashtag
    , MessageEntityPhoneNumber
    ]

instance Arbitrary Sticker where
  arbitrary = Sticker
    <$> arbitrary
