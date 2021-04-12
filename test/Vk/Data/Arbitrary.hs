module Vk.Data.Arbitrary where

import           Test.QuickCheck           hiding (Negative, Positive)
import           Test.QuickCheck.Instances ()
import           Vk.Data

instance Arbitrary Ts where
    arbitrary = Ts <$> arbitrary

instance Arbitrary Update where
    arbitrary = Update
        <$> arbitrary
        <*> arbitrary

instance Arbitrary UpdateType where
    arbitrary = oneof $ pure <$>
        [ MessageNew
        , MessageReply
        , MessageEdit
        , MessageAllow
        , MessageDeny
        , MessageTypingState
        , MessageEvent
        ]

instance Arbitrary Object where
    arbitrary = Object <$> arbitrary

instance Arbitrary Message where
    arbitrary = Message
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Attachment where
    arbitrary = Attachment
        <$> arbitrary
        <*> arbitrary

instance Arbitrary AttachmentType where
    arbitrary = oneof $ pure <$>
        [ Photo
        , Video
        , Audio
        , AudioMessage
        , Doc
        , Link
        , Market
        , Wall
        , Sticker
        ]

instance Arbitrary Media where
    arbitrary = Media
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Keyboard where
    arbitrary = Keyboard
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary


instance Arbitrary Button where
    arbitrary = Button
        <$> arbitrary
        <*> arbitrary

instance Arbitrary ButtonColor where
    arbitrary = oneof $ pure <$>
        [ Primary
        , Secondary
        , Negative
        , Positive
        ]

instance Arbitrary ButtonAction where
    arbitrary = ButtonAction
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary ButtonActionType where
    arbitrary = oneof $ pure <$>
        [ Text
        ]
