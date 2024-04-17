module Language.PureScript.Backend.IR.Inliner where

import Language.PureScript.Backend.IR.Names (Name, nameParser)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as ML

type Pragma = (Name, Annotation)

data Annotation = Always | Never
  deriving stock (Show, Eq, Ord)

type Parser = Megaparsec.Parsec Void Text

pragmaParser ∷ Parser Pragma
pragmaParser = do
  symbol "@inline"
  (,) <$> (nameParser <* sc) <*> annotationParser

annotationParser ∷ Parser Annotation
annotationParser = (Always <$ symbol "always") <|> (Never <$ symbol "never")

symbol ∷ Text → Parser ()
symbol = void . ML.symbol sc

sc ∷ Parser ()
sc = ML.space (MC.hspace1 @_ @Text) empty empty
