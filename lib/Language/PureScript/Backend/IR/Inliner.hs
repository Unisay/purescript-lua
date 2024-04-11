module Language.PureScript.Backend.IR.Inliner where

import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as ML

data Annotation = Annotation InlineScope InlineRecipe
  deriving stock (Show, Eq, Ord)

data InlineScope = InModule | Global
  deriving stock (Show, Eq, Ord)

data InlineRecipe = Default | Always | Never
  deriving stock (Show, Eq, Ord)

type Parser = Megaparsec.Parsec Void Text

annotationParser ∷ Parser Annotation
annotationParser =
  symbol "@inline" *> (Annotation <$> scopeParser <*> recipeParser)

recipeParser ∷ Parser InlineRecipe
recipeParser =
  asum
    [ Default <$ symbol "default"
    , Always <$ symbol "always"
    , Never <$ symbol "never"
    ]

scopeParser ∷ Parser InlineScope
scopeParser = maybe InModule (const Global) <$> optional (symbol "export")

symbol ∷ Text → Parser ()
symbol = void . ML.symbol (ML.space (MC.hspace1 @_ @Text) empty empty)
