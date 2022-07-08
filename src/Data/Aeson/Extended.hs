module Data.Aeson.Extended
  ( module Data.Aeson,
    module Data.Aeson.Extra,
    module Data.Aeson.Types,
    customOptions,
    customOptionsWithDrop
  )
where

import Data.Aeson
import Data.Aeson.Extra
import Data.Aeson.Types
import Universum

customOptions :: Options
customOptions = customOptionsWithDrop 2

customOptionsWithDrop :: Int -> Options
customOptionsWithDrop n = defaultOptions {fieldLabelModifier = camelTo2 '_' . drop n, sumEncoding = UntaggedValue}
