module Golden.ProfunctorDictLens.Test where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor (wrapIso) as Profunctor
import Effect (Effect)
import Effect.Console (logShow)

newtype Wrapped = Wrapped Int

derive instance newtypeWrapped :: Newtype Wrapped _

-- Same shape as Data.Lens._Newtype from issue #28: a binding whose body
-- is an application of a dict-consuming function, abstracted over the
-- Profunctor dictionary. Used twice in main so that the used-once
-- inliner cannot mask a lost dictionary lambda.
_Wrapped :: forall p. Profunctor p => p Int Int -> p Wrapped Wrapped
_Wrapped = dimap unwrap Wrapped

main :: Effect Unit
main = do
  logShow $ unwrap (_Wrapped (_ + 1) (Wrapped 10))
  logShow $ unwrap (_Wrapped (_ * 2) (Wrapped 10))
  logShow $ unwrap (Profunctor.wrapIso Wrapped (_ - 5) (Wrapped 10))
