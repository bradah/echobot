module VK.Bot where

import           Data.Foldable  (forM_)
import           Data.Text      (pack)
import           Data.Text.Read (decimal)
import           VK.Bot.Action
import           VK.Env
import           VK.Methods

runBot :: Env -> IO ()
runBot env = do
  ups <- getUpdates env
  forM_ ups (handleUpdate env)
  print env
  runBot $ inc env

  where
    -- | TODO
    -- Actually Ts can be obtained from updates directily
    inc :: Env -> Env
    inc env = case decimal . envTs $ env of
      Left _       -> env
      Right (n, _) -> env {envTs = pack . show $ n + 1}
