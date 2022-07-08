module Data.Aeson.Extended
  ( module Data.Aeson,
    module Data.Aeson.Extra,
    module Data.Aeson.Types,
    customOptions,
  )
where

import Universum
import Data.Aeson.Extra
import Data.Aeson
import Data.Aeson.Types

customOptions :: Options
customOptions = defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2, sumEncoding = UntaggedValue}
