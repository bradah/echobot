{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Env where

import           Data.Text        (Text)
import           Network.HTTP.Req

-- * Environment

-- ** Env

-- | Environment in which bot runs
data Env = Env
    { envBaseUrl :: Url 'Https
    -- ^ Base URL for API 'Methods'.
    -- This includes 'Token'
    }

-- | Bot token
type Token = Text

-- | Smart constructor for 'Env'
mkEnv :: Token -> Env
mkEnv t = Env
    { envBaseUrl = https "api.telegram.org" /: "bot" <> t
    }

