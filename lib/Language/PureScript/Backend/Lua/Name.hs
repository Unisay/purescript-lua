{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.PureScript.Backend.Lua.Name
  ( Name
  , fromText
  , toText
  , name
  , parser
  , unsafeName
  , makeSafe
  , specialNameType
  , specialNameCtor
  , join2
  ) where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Prettyprinter (Pretty)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Prelude hiding (toText)

newtype Name = Name {toText ∷ Text}
  deriving newtype (Eq, Ord, Show, Pretty)

name ∷ QuasiQuoter
name =
  QuasiQuoter
    { quoteExp = \(s ∷ String) →
        case fromText (fromString s) of
          Nothing →
            error "Language.PureScript.Backend.Lua.Name: invalid name"
          Just (Name n) → ⟦Name n⟧
    , quotePat =
        const $
          error "Language.PureScript.Backend.Lua.Name.quotePat: unsupported"
    , quoteType =
        const $
          error "Language.PureScript.Backend.Lua.Name.quoteType: unsupported"
    , quoteDec =
        const $
          error "Language.PureScript.Backend.Lua.Name.quoteDec: unsupported"
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

parser ∷ M.Parsec Void Text Name
parser =
  Name <$> do
    c ← M.letterChar <|> M.char '_'
    cs ← M.many (M.alphaNumChar <|> M.char '_')
    pure $ Text.pack (c : cs)

makeSafe ∷ HasCallStack ⇒ Text → Name
makeSafe unsafe = unsafeName safest
 where
  safest =
    if safer `Set.member` reservedNames
      then '_' `Text.cons` safer `Text.snoc` '_'
      else safer
  safer =
    Text.replace "$" "_S_"
      . Text.replace "." "_"
      $ Text.replace "'" "Prime" unsafe

unsafeName ∷ HasCallStack ⇒ Text → Name
unsafeName n =
  fromMaybe
    ( error . unwords $
        [ "Language.PureScript.Backend.Lua.Name.unsafeName:"
        , "invalid name"
        , show n
        ]
    )
    (fromText n)

specialNameType ∷ Name
specialNameType = Name "$type"

specialNameCtor ∷ Name
specialNameCtor = Name "$ctor"

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

join2 ∷ Name → Name → Name
join2 (Name a) (Name b) = Name (a <> "_I_" <> b)
