{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Language.PureScript.Backend.IR.Spec where

import Data.List.NonEmpty qualified as NE
import Language.PureScript.Backend.IR (Context (..), RepM, mkCase, runRepM)
import Language.PureScript.Backend.IR.Names (Name (..), PropName (..))
import Language.PureScript.Backend.IR.Types
import Language.PureScript.CoreFn qualified as Cfn
import Language.PureScript.Names qualified as PS
import Language.PureScript.PSString qualified as PS
import Test.Hspec (Spec, describe, it, shouldBe)

spec ∷ Spec
spec = describe "IR representation" do
  describe "case expressions" do
    describe "singular" do
      it "null binder" do
        representedCase
          [cfnBool True]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders = [cfnNullB]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          ]
          >>= (`shouldBe` literalInt 1)

      let defaultAlternative =
            Cfn.CaseAlternative
              { caseAlternativeBinders = [cfnNullB]
              , caseAlternativeResult = Right $ cfnInt 0
              }

      it "literalInt literal binder" do
        representedCase
          [cfnInt 3]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [cfnLitB (Cfn.NumericLiteral (Left 9))]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          , defaultAlternative
          ]
          >>= ( `shouldBe`
                  ifThenElse (literalInt 9 `eq` literalInt 3) (literalInt 1) (literalInt 0)
              )

      it "literalFloat literal binder" do
        representedCase
          [cfnFloat 3.0]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [cfnLitB (Cfn.NumericLiteral (Right 9.0))]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          , defaultAlternative
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalFloat 9.0 `eq` literalFloat 3.0)
                    (literalInt 1)
                    (literalInt 0)
              )

      it "char literal binder" do
        representedCase
          [cfnCharE 'x']
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [cfnLitB (cfnCharL 'c')]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          , defaultAlternative
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalChar 'c' `eq` literalChar 'x')
                    (literalInt 1)
                    (literalInt 0)
              )

      it "array literal binder" do
        let x = refLocal (Name "x") 0
            expectedResult =
              ifThenElse
                (literalInt 3 `eq` arrayLength x)
                ( ifThenElse
                    (literalChar 'a' `eq` arrayIndex x 0)
                    ( ifThenElse
                        (literalChar 'b' `eq` arrayIndex x 1)
                        ( ifThenElse
                            (literalInt 2 `eq` arrayLength (arrayIndex x 2))
                            ( ifThenElse
                                (literalChar 'c' `eq` arrayIndex (arrayIndex x 2) 0)
                                ( ifThenElse
                                    ( literalChar 'd'
                                        `eq` arrayIndex (arrayIndex x 2) 1
                                    )
                                    (literalInt 1)
                                    (literalInt 0)
                                )
                                (literalInt 0)
                            )
                            (literalInt 0)
                        )
                        (literalInt 0)
                    )
                    (literalInt 0)
                )
                (literalInt 0)

        representedCase
          [cfnRef "x"]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ Cfn.LiteralBinder
                      ann
                      ( Cfn.ArrayLiteral
                          [ cfnLitB (cfnCharL 'a')
                          , cfnLitB (cfnCharL 'b')
                          , cfnLitB $
                              Cfn.ArrayLiteral
                                [ cfnLitB (cfnCharL 'c')
                                , cfnLitB (cfnCharL 'd')
                                ]
                          ]
                      )
                  ]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          , defaultAlternative
          ]
          >>= (`shouldBe` expectedResult)

      it "object literal binder" do
        representedCase
          [cfnRef "x"]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ Cfn.LiteralBinder
                      ann
                      ( Cfn.ObjectLiteral
                          [ ("foo", cfnLitB (cfnCharL 'a'))
                          ,
                            ( "bar"
                            , Cfn.LiteralBinder
                                ann
                                ( Cfn.ObjectLiteral
                                    [
                                      ( "baz"
                                      , cfnLitB (cfnCharL 'b')
                                      )
                                    ]
                                )
                            )
                          ]
                      )
                  ]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          , defaultAlternative
          ]
          >>= ( `shouldBe`
                  let x = refLocal (Name "x") 0
                   in ifThenElse
                        (literalChar 'a' `eq` objectProp x (PropName "foo"))
                        ( ifThenElse
                            ( literalChar 'b'
                                `eq` objectProp
                                  (objectProp x (PropName "bar"))
                                  (PropName "baz")
                            )
                            (literalInt 1)
                            (literalInt 0)
                        )
                        (literalInt 0)
              )

      it "local reference is not created for literal int expression" do
        representedCase
          [cfnInt 1]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders = [cfnLitB (Cfn.NumericLiteral (Left 2))]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalInt 2 `eq` literalInt 1)
                    (literalInt 1)
                    (exception "No patterns matched")
              )

      it "local reference is not created for literal literalFloat expression" do
        representedCase
          [cfnFloat 1.0]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [cfnLitB (Cfn.NumericLiteral (Right 2.0))]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalFloat 2.0 `eq` literalFloat 1.0)
                    (literalInt 1)
                    (exception "No patterns matched")
              )

      it "local reference is not created for literal literalChar expression" do
        representedCase
          [cfnCharE 'x']
          [ Cfn.CaseAlternative
              { caseAlternativeBinders = [cfnLitB (cfnCharL 'a')]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalChar 'a' `eq` literalChar 'x')
                    (literalInt 1)
                    (exception "No patterns matched")
              )

      it "local reference is not created for literal bool expression" do
        representedCase
          [cfnBool True]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders = [cfnLitB (Cfn.BooleanLiteral False)]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalBool False `eq` literalBool True)
                    (literalInt 1)
                    (exception "No patterns matched")
              )

      it "local reference is created for array literal expression" do
        representedCase
          [cfnArray []]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders = [cfnLitB (Cfn.BooleanLiteral False)]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          ]
          >>= ( `shouldBe`
                  let1
                    (Name "e0")
                    (literalArray [])
                    ( ifThenElse
                        (literalBool False `eq` refLocal (Name "e0") 0)
                        (literalInt 1)
                        (exception "No patterns matched")
                    )
              )

      it "local reference is created for object literal expression" do
        representedCase
          [cfnObject [("a", cfnInt 1)]]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ cfnLitB
                      ( Cfn.ObjectLiteral
                          [ ("a", cfnLitB (Cfn.NumericLiteral (Left 2)))
                          ]
                      )
                  ]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          ]
          >>= ( `shouldBe`
                  let1
                    (Name "e0")
                    (literalObject [(PropName "a", literalInt 1)])
                    ( ifThenElse
                        ( literalInt 2
                            `eq` objectProp
                              (refLocal (Name "e0") 0)
                              (PropName "a")
                        )
                        (literalInt 1)
                        (exception "No patterns matched")
                    )
              )

      it "local reference is not created for reference expression" do
        representedCase
          [cfnRef "r"]
          [ Cfn.CaseAlternative
              { caseAlternativeBinders = [cfnLitB (Cfn.BooleanLiteral False)]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalBool False `eq` refLocal (Name "r") 0)
                    (literalInt 1)
                    (exception "No patterns matched")
              )

    describe "plural" do
      it "two binders compiles to a nested if" do
        {-

        case 'x', 'y' of         if 'a' == 'x' then
          'a', 'b' -> 1    ==>     if 'b' == 'y' then
          'e',  _  -> 0              1
                                   else
                                     if 'e' == 'x' then
                                      0
                                     else
                                      exception "no patterns matched"
                                 else
                                   if 'e' == 'x' then
                                     0
                                   else
                                     exception "no patterns matched"

        -}
        representedCase
          [cfnCharE 'x', cfnCharE 'y']
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ cfnLitB (cfnCharL 'a')
                  , cfnLitB (cfnCharL 'b')
                  ]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          , Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ cfnLitB (cfnCharL 'e')
                  , cfnNullB
                  ]
              , caseAlternativeResult = Right $ cfnInt 0
              }
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalChar 'a' `eq` literalChar 'x')
                    ( ifThenElse
                        (literalChar 'b' `eq` literalChar 'y')
                        (literalInt 1)
                        ( ifThenElse
                            (literalChar 'e' `eq` literalChar 'x')
                            (literalInt 0)
                            (exception "No patterns matched")
                        )
                    )
                    ( ifThenElse
                        (literalChar 'e' `eq` literalChar 'x')
                        (literalInt 0)
                        (exception "No patterns matched")
                    )
              )

      it "Used var binders are pushed to the RHS" do
        representedCase
          [cfnCharE 't', cfnCharE 'z']
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ cfnVarB (PS.Ident "x")
                  , cfnLitB (cfnCharL 'a')
                  ]
              , caseAlternativeResult = Right $ cfnRef "x"
              }
          , Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ cfnLitB (cfnCharL 'b')
                  , cfnVarB (PS.Ident "y")
                  ]
              , caseAlternativeResult = Right $ cfnRef "y"
              }
          , Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ cfnNullB
                  , cfnNullB
                  ]
              , caseAlternativeResult = Right $ cfnInt 3
              }
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalChar 'a' `eq` literalChar 'z')
                    (let1 (Name "x") (literalChar 't') (refLocal (Name "x") 0))
                    ( ifThenElse
                        (literalChar 'b' `eq` literalChar 't')
                        (let1 (Name "y") (literalChar 'z') (refLocal (Name "y") 0))
                        (literalInt 3)
                    )
              )

      it "named wildcard binders compile to a let" do
        representedCase
          [cfnCharE 'x', cfnCharE 'y']
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ cfnNamB (PS.Ident "v") cfnNullB
                  , cfnNamB (PS.Ident "z") cfnNullB
                  ]
              , caseAlternativeResult = Right $ cfnRef "z"
              }
          ]
          >>= ( `shouldBe`
                  lets
                    ( Standalone (noAnn, Name "z", literalChar 'y')
                        :| [Standalone (noAnn, Name "v", literalChar 'x')]
                    )
                    (refLocal (Name "z") 0)
              )

      it "named binders compile to a let bindings" do
        representedCase
          [cfnCharE 'x', cfnCharE 'y']
          [ Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ cfnNamB (PS.Ident "a") (cfnLitB (cfnCharL 'a'))
                  , cfnNamB (PS.Ident "b") (cfnLitB (cfnCharL 'b'))
                  ]
              , caseAlternativeResult =
                  Right $ cfnApp (cfnRef "a") (cfnRef "b")
              }
          , Cfn.CaseAlternative
              { caseAlternativeBinders =
                  [ cfnNamB (PS.Ident "o1") cfnNullB
                  , cfnNamB (PS.Ident "o2") cfnNullB
                  ]
              , caseAlternativeResult =
                  Right $ cfnApp (cfnRef "o2") (cfnRef "o1")
              }
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (literalChar 'a' `eq` literalChar 'x')
                    ( ifThenElse
                        (literalChar 'b' `eq` literalChar 'y')
                        ( lets
                            ( Standalone (noAnn, Name "b", literalChar 'y')
                                :| [Standalone (noAnn, Name "a", literalChar 'x')]
                            )
                            ( application
                                (refLocal (Name "a") 0)
                                (refLocal (Name "b") 0)
                            )
                        )
                        ( lets
                            ( Standalone (noAnn, Name "o2", literalChar 'y')
                                :| [Standalone (noAnn, Name "o1", literalChar 'x')]
                            )
                            ( application
                                (refLocal (Name "o2") 0)
                                (refLocal (Name "o1") 0)
                            )
                        )
                    )
                    ( lets
                        ( Standalone (noAnn, Name "o2", literalChar 'y')
                            :| [Standalone (noAnn, Name "o1", literalChar 'x')]
                        )
                        ( application
                            (refLocal (Name "o2") 0)
                            (refLocal (Name "o1") 0)
                        )
                    )
              )

--------------------------------------------------------------------------------
-- Helper functions ------------------------------------------------------------

representedCase
  ∷ MonadFail m
  ⇒ [Cfn.Expr Cfn.Ann]
  → [Cfn.CaseAlternative Cfn.Ann]
  → m Exp
representedCase es alts = runRepresentM (mkCase noAnn es (NE.fromList alts))

runRepresentM ∷ MonadFail m ⇒ RepM Exp → m Exp
runRepresentM rm =
  either
    (fail . show)
    (pure . snd)
    ( runRepM
        Context
          { contextModule = cfnModule
          , contextDataTypes = mempty
          , lastGeneratedNameIndex = 0
          , needsRuntimeLazy = Any False
          , annotations = mempty
          }
        rm
    )

--------------------------------------------------------------------------------
-- Fixture ---------------------------------------------------------------------

ann ∷ Cfn.Ann
ann = Nothing

cfnModule ∷ ∀ {a}. Cfn.Module a
cfnModule =
  Cfn.Module
    { moduleName = PS.ModuleName "M"
    , moduleComments = mempty
    , modulePath = "M.purs"
    , moduleImports = mempty
    , moduleExports = mempty
    , moduleReExports = mempty
    , moduleForeign = mempty
    , moduleBindings = mempty
    }

cfnQualifyModule ∷ a → PS.Qualified a
cfnQualifyModule = PS.Qualified (PS.ByModuleName (PS.ModuleName "ModuleName"))

cfnLocalIdent ∷ Text → PS.Qualified PS.Ident
cfnLocalIdent = PS.Qualified (PS.BySourcePos (PS.SourcePos 0 0)) . PS.Ident

cfnRef ∷ Text → Cfn.Expr Cfn.Ann
cfnRef = Cfn.Var ann . cfnLocalIdent

cfnBool ∷ Bool → Cfn.Expr Cfn.Ann
cfnBool b = Cfn.Literal ann (Cfn.BooleanLiteral b)

cfnInt ∷ Integer → Cfn.Expr Cfn.Ann
cfnInt i = Cfn.Literal ann (Cfn.NumericLiteral (Left i))

cfnFloat ∷ Double → Cfn.Expr Cfn.Ann
cfnFloat f = Cfn.Literal ann (Cfn.NumericLiteral (Right f))

cfnCharE ∷ Char → Cfn.Expr Cfn.Ann
cfnCharE = Cfn.Literal ann . cfnCharL

cfnCharL ∷ Char → Cfn.Literal a
cfnCharL = Cfn.CharLiteral

cfnArray ∷ [Cfn.Expr Cfn.Ann] → Cfn.Expr Cfn.Ann
cfnArray a = Cfn.Literal ann (Cfn.ArrayLiteral a)

cfnObject ∷ [(Text, Cfn.Expr Cfn.Ann)] → Cfn.Expr Cfn.Ann
cfnObject o = Cfn.Literal ann $ Cfn.ObjectLiteral (first PS.mkString <$> o)

cfnLitB ∷ Cfn.Literal (Cfn.Binder Cfn.Ann) → Cfn.Binder Cfn.Ann
cfnLitB = Cfn.LiteralBinder ann

cfnVarB ∷ PS.Ident → Cfn.Binder Cfn.Ann
cfnVarB = Cfn.VarBinder ann

cfnNamB ∷ PS.Ident → Cfn.Binder Cfn.Ann → Cfn.Binder Cfn.Ann
cfnNamB = Cfn.NamedBinder ann

cfnNullB ∷ Cfn.Binder Cfn.Ann
cfnNullB = Cfn.NullBinder ann

cfnApp ∷ Cfn.Expr Cfn.Ann → Cfn.Expr Cfn.Ann → Cfn.Expr Cfn.Ann
cfnApp = Cfn.App ann

let1 ∷ Name → Exp → Exp → Exp
let1 n e = lets (pure (Standalone (noAnn, n, e)))
