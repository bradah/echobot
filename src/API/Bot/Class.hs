{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module API.Bot.Class where

import           Control.Monad.Reader

class (MonadIO b) => Bot (b :: * -> *) where
    data Env b
    type Update b = u | u -> b
    type UpdateId b = uid | uid -> b

    getUpdates :: Maybe (UpdateId b) -> b [Update b]
    getUpdateId :: [Update b] -> b (Maybe (UpdateId b))
    handleUpdate :: Update b -> b ()
    mkEnv :: IO (Env b)
    unwrap :: Env b -> b a -> IO ()
