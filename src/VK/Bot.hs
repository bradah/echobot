module VK.Bot where

import           VK.Env
import           VK.Methods
import           VK.Types

startBot :: Env -> IO ()
startBot env = do
  ups <- getUpdates env

