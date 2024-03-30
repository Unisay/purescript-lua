{- | This module defines the data type `Key` which is used to represent the
keys of a Lua table.
-}
module Language.PureScript.Backend.Lua.Key
  ( Key (..)
  , parser
  , toSafeName
  ) where

import Language.PureScript.Backend.Lua.Name (Name)
import Language.PureScript.Backend.Lua.Name qualified as Name
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char qualified as M

data Key = KeyName Name | KeyReserved Text
  deriving stock (Eq, Show)

toSafeName ∷ Key → Name
toSafeName (KeyName n) = n
toSafeName (KeyReserved t) = Name.makeSafe t

type Parser = Mega.Parsec Void Text

parser ∷ Parser Key
parser = (nameParser <|> reservedParser) <* M.space
 where
  nameParser ∷ Parser Key
  nameParser = KeyName <$> Name.parser

  reservedParser ∷ Parser Key
  reservedParser = brackets $ quotes do
    KeyReserved <$> Mega.choice (M.string <$> toList Name.reserved)

  brackets ∷ Parser a → Parser a
  brackets = between '[' ']'

  quotes ∷ Parser a → Parser a
  quotes = between '\"' '\"'

  between ∷ Char → Char → Parser c → Parser c
  between open close p = char open *> p <* char close

  char ∷ Char → Parser ()
  char c = M.char c *> M.space
