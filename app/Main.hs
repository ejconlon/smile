module Main (main) where

import Options.Applicative
import Smile.App
import Smile.Core (Core)
import Smile.Exe (exe)
import Smile.Logging (LogC)
import Smile.Prelude

run :: LogC env m => m ()
run = do
  logInfo "We're inside the application!"

data Config = Config
  { param :: Int
  } deriving (Generic, Eq, Show)

data Value = Value
  { signal :: IORef Int
  } deriving (Generic)

-- BEGIN TemplateHaskell candidate section
newtype MyApp = MyApp { unMyApp :: App Value }
  deriving (Generic)

myAppL :: Lens' MyApp (App Value)
myAppL = field @"unMyApp"

instance Has Core MyApp where
  hasLens = myAppL . hasLens

instance Has Value MyApp where
  hasLens = myAppL . appRestL
-- END TemplateHaskell candidate section

configParser :: Parser Config
configParser =
    (Config
        <$> option auto (
            long "param"
            <> short 'p'
            <> help "some param"
            )
    )

initApp :: Config -> Core -> IO MyApp
initApp config core = do
  signal <- newIORef (param config)
  pure (MyApp (mkApp (Value signal) core))

main :: IO ()
main = exe configParser initApp (flip runRIO run)
