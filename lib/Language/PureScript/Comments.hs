{-# LANGUAGE TemplateHaskell #-}

-- | Defines the types of source code comments
module Language.PureScript.Comments where

import Data.Aeson.TH
  ( Options (..)
  , SumEncoding (..)
  , defaultOptions
  , deriveJSON
  )

data Comment
  = LineComment Text
  | BlockComment Text
  deriving stock (Show, Eq, Ord, Generic)

$(deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField}) ''Comment)
