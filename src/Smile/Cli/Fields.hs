module Smile.Cli.Fields where

import Smile.Prelude

data Field a = Field
    { _longName :: Maybe Text
    , _shortName :: Maybe Text
    , _required :: Maybe Text
    , _default :: Maybe a
    , _envVar :: Maybe Text
    , _metaVar :: Maybe Text
    , _help :: Maybe Text
    } deriving (Eq, Show, Functor)

$(makeSmileLenses ''Field)

field :: Field a
field = Field Nothing Nothing Nothing Nothing Nothing Nothing Nothing
