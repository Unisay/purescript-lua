{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Lua.Name
  ( Name
  , toText
  , name
  , unsafeName
  , unsafeFromText
  , makeSafe
  , fromText
  ) where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Prettyprinter (Pretty)
import Prelude hiding (toText)

newtype Name = Name {toText ∷ Text}
  deriving newtype (Eq, Ord, Show, Pretty)

name ∷ QuasiQuoter
name =
  QuasiQuoter
    { quoteExp = \(s ∷ String) →
        case fromText (fromString s) of
          Nothing → error "Language.Lua.Name: invalid name"
          Just (Name n) → ⟦Name n⟧
    , quotePat = const $ error "Language.Lua.Name.quotePat: unsupported"
    , quoteType = const $ error "Language.Lua.Name.quoteType: unsupported"
    , quoteDec = const $ error "Language.Lua.Name.quoteDec: unsupported"
    }

fromText ∷ Text → Maybe Name
fromText t =
  case Text.strip t of
    n
      | Text.length n > 0
      , checkFirst (Text.head n)
      , Text.all checkRest (Text.tail n)
      , Set.notMember n reservedNames →
          Just (Name n)
    _ → Nothing
 where
  checkFirst c = Char.isAlpha c || c == '_'
  checkRest c = Char.isDigit c || checkFirst c

makeSafe ∷ HasCallStack ⇒ Text → Name
makeSafe unsafe = unsafeName safest
 where
  safest =
    if safer `Set.member` reservedNames
      then '_' `Text.cons` safer `Text.snoc` '_'
      else safer
  safer =
    Text.replace "$" "_S_" . Text.replace "." "_" $
      Text.replace "'" "Prime" unsafe

unsafeName ∷ HasCallStack ⇒ Text → Name
unsafeName n =
  fromMaybe
    (error $ unwords ["Language.Lua.Name.unsafeName:", "invalid name", show n])
    (fromText n)

unsafeFromText ∷ Text → Name
unsafeFromText = Name

reservedNames ∷ Set Text
reservedNames =
  Set.fromList
    [ "and"
    , "break"
    , "do"
    , "else"
    , "elseif"
    , "end"
    , "false"
    , "for"
    , "function"
    , "goto"
    , "if"
    , "in"
    , "local"
    , "nil"
    , "not"
    , "or"
    , "repeat"
    , "return"
    , "then"
    , "true"
    , "until"
    , "while"
    ]
