module Language.PureScript.Backend.IR.Inliner where

import Control.Monad.Combinators (choice)
import Language.PureScript.Backend.IR.Names (Name, nameParser)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as ML

type Pragma = (Name, Annotation)

data Annotation = Annotation InlineScope InlineRecipe
  deriving stock (Show, Eq, Ord)

data InlineScope = InModule | Global
  deriving stock (Show, Eq, Ord)

data InlineRecipe = Default | Always | Never
  deriving stock (Show, Eq, Ord)

type Parser = Megaparsec.Parsec Void Text

pragmaParser ∷ Parser Pragma
pragmaParser = do
  symbol "@inline"
  (,) <$> (nameParser <* sc) <*> annotationParser

annotationParser ∷ Parser Annotation
annotationParser = Annotation <$> scopeParser <*> recipeParser

recipeParser ∷ Parser InlineRecipe
recipeParser =
  choice
    [ Default <$ symbol "default"
    , Always <$ symbol "always"
    , Never <$ symbol "never"
    ]

scopeParser ∷ Parser InlineScope
scopeParser = maybe InModule (const Global) <$> optional (symbol "export")

symbol ∷ Text → Parser ()
symbol = void . ML.symbol sc

sc ∷ Parser ()
sc = ML.space (MC.hspace1 @_ @Text) empty empty
