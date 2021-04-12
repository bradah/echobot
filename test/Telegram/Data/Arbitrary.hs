{-# LANGUAGE FlexibleInstances #-}

module Telegram.Data.Arbitrary where

import           Telegram.Data
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary Update where
  arbitrary = Update
    <$> arbitrary
    <*> arbitrary
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
      , fmap ("/" <>) <$> arbitrary
      ]
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


instance Arbitrary User where
  arbitrary = User
    <$> arbitrary
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
    [ Mention
    , Hashtag
    , BotCommand
    , Url
    , Email
    , Bold
    , Italic
    , Underline
    , Strikethrough
    , Code
    , Pre
    , TextLink
    , TextMention
    , Cashtag
    , PhoneNumber
    ]

instance Arbitrary InlineKeyboardMarkup where
    arbitrary = InlineKeyboardMarkup
        <$> arbitrary

instance Arbitrary InlineKeyboardButton where
    arbitrary = InlineKeyboardButton
        <$> arbitrary
        <*> arbitrary

instance Arbitrary Sticker where
    arbitrary = Sticker
        <$> arbitrary

instance Arbitrary CallbackQuery where
    arbitrary = CallbackQuery
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary PhotoSize where
    arbitrary = PhotoSize
        <$> arbitrary

instance {-# OVERLAPPING #-} Arbitrary [PhotoSize] where
    arbitrary = listOf1 arbitrary

instance Arbitrary Animation where
    arbitrary = Animation
        <$> arbitrary

instance Arbitrary Audio where
    arbitrary = Audio
        <$> arbitrary

instance Arbitrary Document where
    arbitrary = Document
        <$> arbitrary

instance Arbitrary Video where
    arbitrary = Video
        <$> arbitrary

instance Arbitrary VideoNote where
    arbitrary = VideoNote
        <$> arbitrary

instance Arbitrary Voice where
    arbitrary = Voice
        <$> arbitrary
