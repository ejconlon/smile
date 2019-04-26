module Smile.EnvParser where

import Data.Map (Map)
-- import qualified Data.Map as Map
-- import qualified RIO.Text as Text
import Smile.Prelude
-- import System.Environment (getEnvironment)
-- import Text.Read (readEither)

-- Implementing https://github.com/pcapriotti/optparse-applicative/issues/118#issuecomment-67329552

type Env = Map Text Text

data Field a = Field
    { _longName :: Maybe Text
    , _shortName :: Maybe Text
    , _required :: Maybe Text
    , _default :: Maybe a
    , _envVar :: Maybe Text
    , _metaVar :: Maybe Text
    , _help :: Maybe Text
    } deriving (Eq, Show)

$(makeSmileLenses ''Field)

win :: Maybe a -> Maybe a -> Maybe a
win _ y@(Just _) = y
win x Nothing = x

instance Semigroup (Field a) where
    (Field a1 b1 c1 d1 e1 f1 g1) <> (Field a2 b2 c2 d2 e2 f2 g2) =
        Field (win a1 a2) (win b1 b2) (win c1 c2) (win d1 d2) (win e1 e2) (win f1 f2) (win g1 g2)

instance Monoid (Field a) where
    mempty = Field Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data SomeField where
    SomeField :: Field a -> SomeField

newtype EnvParser a = EnvParser (Maybe a)

emptyEnvParser :: EnvParser ()
emptyEnvParser = EnvParser (Just ())

getAndRunEnvParser :: EnvParser a -> IO a
getAndRunEnvParser = undefined

-- data Extractor m z a = Extractor
--     { _field :: Field a
--     , _entry :: z -> m a
--     }

-- $(makeSmileLenses ''Extractor)

-- rawTextField :: (Text -> Either Text a) -> Field a -> Parser e a
-- rawTextField fun field = undefined

-- textField :: FromText a => Field a -> Parser e a
-- textField = rawTextField fromText

-- extract :: Alternative m => Extractor m z a -> z -> m a
-- extract = undefined

-- data ParserEnv e = ParserEnv
--     { _env :: Env
--     , _embedding :: SomeField -> Text -> e
--     }

-- $(makeSmileLenses ''ParserEnv)

-- newtype Parser e a = Parser { unParser :: ReaderT ParserEnv (Except e) a }
--     deriving (Functor, Applicative, Monad, MonadReader ParserEnv, MonadError e)

-- runParser :: Parser e a -> Env -> Either e a
-- runParser = undefined

-- getEnv :: IO Env
-- getEnv = do
--     rawEnv <- getEnvironment
--     let textEnv = (\(a, b) -> (Text.pack a, Text.pack b)) <$> rawEnv
--     pure (Map.fromList textEnv)

-- class FromText a where
--     fromText :: Text -> Either Text a

-- left :: (a -> b) -> Either a c -> Either b c
-- left f = \case
--     Left a -> Left (f a)
--     Right b -> Right b

-- readFromText :: Read a => Text -> Either Text a
-- readFromText v = left Text.pack (readEither (Text.unpack v))

-- instance FromText Text where
--     fromText = Right

-- instance FromText Int where
--     fromText = readFromText

-- instance FromText Bool where
--     fromText v =
--         case Text.toLower v of
--             "true" -> Right True
--             "t" -> Right True
--             "yes" -> Right True
--             "y" -> Right True
--             "1" -> Right True
--             "false" -> Right False
--             "f" -> Right False
--             "no" -> Right False
--             "n" -> Right False
--             "0" -> Right False
--             _ -> Left ("Not a valid bool flag: " <> v)
