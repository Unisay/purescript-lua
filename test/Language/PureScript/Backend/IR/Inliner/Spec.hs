module Language.PureScript.Backend.IR.Inliner.Spec where

import Hedgehog (MonadTest, failure, footnote, (===))
import Language.PureScript.Backend.IR.Inliner
  ( Annotation (..)
  , InlineRecipe (..)
  , InlineScope (..)
  , Pragma
  )
import Language.PureScript.Backend.IR.Inliner qualified as Inliner
import Language.PureScript.Backend.IR.Names (Name (..))
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)
import Text.Megaparsec qualified as Megaparsec

spec ∷ Spec
spec = describe "IR Inliner" do
  describe "parses annotations" do
    test "@inline foo always" do
      ann ← parseAnn "@inline foo always "
      ann === (Name "foo", Annotation InModule Always)

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

parseAnn ∷ MonadTest m ⇒ Text → m Pragma
parseAnn src = do
  let parser = Inliner.pragmaParser <* Megaparsec.eof
  case Megaparsec.parse parser "<test>" src of
    Left eb → do
      footnote $ Megaparsec.errorBundlePretty eb
      failure
    Right ann → pure ann
