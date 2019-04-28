module Smile.Cli.Parser where

import qualified Data.Map as Map
import qualified RIO.Text as Text
import Smile.Prelude
import Smile.Cli.Fields
import Smile.Cli.Selective
import System.Environment (getEnvironment)
import Text.Read (readEither)

type Env = Map Text Text

newtype Parser a = Parser
    { unParser :: FreeSelective Ord (DefMap Field) Field a
    } deriving (Functor, Applicative)

emptyParser :: Parser ()
emptyParser = pure ()

execParser :: Parser a -> IO a
execParser = error "TODO execParser"

-- data Extractor m z a = Extractor
--     { _field :: Field a
--     , _entry :: z -> m a
--     }

-- $(makeSmileLenses ''Extractor)

-- rawTextField :: (Text -> Either Text a) -> Field a -> Parser a
-- rawTextField fun field = undefined

-- textField :: FromText a => Field a -> Parser a
-- textField = rawTextField fromText

getEnv :: IO Env
getEnv = do
    rawEnv <- getEnvironment
    let textEnv = (\(a, b) -> (Text.pack a, Text.pack b)) <$> rawEnv
    pure (Map.fromList textEnv)

class FromText a where
    fromText :: Text -> Either Text a

left :: (a -> b) -> Either a c -> Either b c
left f = \case
    Left a -> Left (f a)
    Right b -> Right b

readFromText :: Read a => Text -> Either Text a
readFromText v = left Text.pack (readEither (Text.unpack v))

instance FromText Text where
    fromText = Right

instance FromText Int where
    fromText = readFromText

instance FromText Bool where
    fromText v =
        case Text.toLower v of
            "true" -> Right True
            "t" -> Right True
            "yes" -> Right True
            "y" -> Right True
            "1" -> Right True
            "false" -> Right False
            "f" -> Right False
            "no" -> Right False
            "n" -> Right False
            "0" -> Right False
            _ -> Left ("Not a valid bool flag: " <> v)
