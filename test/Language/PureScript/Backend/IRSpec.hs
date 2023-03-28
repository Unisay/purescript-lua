module Language.PureScript.Backend.IRSpec where

import Data.List.NonEmpty qualified as NE
import Language.PureScript.Backend.IR (Context (..), RepM, mkCase, runRepM)
import Language.PureScript.Backend.IR.Types
import Language.PureScript.Backend.IR.Types qualified as IR
import Language.PureScript.CoreFn qualified as Cfn
import Language.PureScript.Names qualified as PS
import Language.PureScript.PSString qualified as PS
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
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
          >>= (`shouldBe` integer 1)

      let defaultAlternative =
            Cfn.CaseAlternative
              { caseAlternativeBinders = [cfnNullB]
              , caseAlternativeResult = Right $ cfnInt 0
              }

      it "integer literal binder" do
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
                  ifThenElse (integer 9 `eq` integer 3) (integer 1) (integer 0)
              )

      it "float literal binder" do
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
                    (float 9.0 `eq` float 3.0)
                    (integer 1)
                    (integer 0)
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
                  ifThenElse (char 'c' `eq` char 'x') (integer 1) (integer 0)
              )

      it "array literal binder" do
        let x = refFreeLocal (Name "x")
            expectedResult =
              ifThenElse
                (integer 3 `eq` arrayLength x)
                ( ifThenElse
                    (char 'a' `eq` arrayIndex x 0)
                    ( ifThenElse
                        (char 'b' `eq` arrayIndex x 1)
                        ( ifThenElse
                            (integer 2 `eq` arrayLength (arrayIndex x 2))
                            ( ifThenElse
                                (char 'c' `eq` arrayIndex (arrayIndex x 2) 0)
                                ( ifThenElse
                                    ( char 'd'
                                        `eq` arrayIndex (arrayIndex x 2) 1
                                    )
                                    (integer 1)
                                    (integer 0)
                                )
                                (integer 0)
                            )
                            (integer 0)
                        )
                        (integer 0)
                    )
                    (integer 0)
                )
                (integer 0)

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
                  let x = refFreeLocal (Name "x")
                   in ifThenElse
                        (char 'a' `eq` objectProp x (PropName "foo"))
                        ( ifThenElse
                            ( char 'b'
                                `eq` objectProp
                                  (objectProp x (PropName "bar"))
                                  (PropName "baz")
                            )
                            (integer 1)
                            (integer 0)
                        )
                        (integer 0)
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
                    (integer 2 `eq` integer 1)
                    (integer 1)
                    (exception "No patterns matched")
              )

      it "local reference is not created for literal float expression" do
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
                    (float 2.0 `eq` float 1.0)
                    (integer 1)
                    (exception "No patterns matched")
              )

      it "local reference is not created for literal char expression" do
        representedCase
          [cfnCharE 'x']
          [ Cfn.CaseAlternative
              { caseAlternativeBinders = [cfnLitB (cfnCharL 'a')]
              , caseAlternativeResult = Right $ cfnInt 1
              }
          ]
          >>= ( `shouldBe`
                  ifThenElse
                    (char 'a' `eq` char 'x')
                    (integer 1)
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
                    (boolean False `eq` boolean True)
                    (integer 1)
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
                    (array [])
                    ( ifThenElse
                        (boolean False `eq` refFreeLocal (Name "e0"))
                        (integer 1)
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
                    (object [(PropName "a", integer 1)])
                    ( ifThenElse
                        ( integer 2
                            `eq` objectProp
                              (refFreeLocal (Name "e0"))
                              (PropName "a")
                        )
                        (integer 1)
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
                    (boolean False `eq` refFreeLocal (Name "r"))
                    (integer 1)
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
                    (char 'a' `eq` char 'x')
                    ( ifThenElse
                        (char 'b' `eq` char 'y')
                        (integer 1)
                        ( ifThenElse
                            (char 'e' `eq` char 'x')
                            (integer 0)
                            (exception "No patterns matched")
                        )
                    )
                    ( ifThenElse
                        (char 'e' `eq` char 'x')
                        (integer 0)
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
                    (char 'a' `eq` char 'z')
                    (let1 (Name "x") (char 't') (refFreeLocal (Name "x")))
                    ( ifThenElse
                        (char 'b' `eq` char 't')
                        (let1 (Name "y") (char 'z') (refFreeLocal (Name "y")))
                        (integer 3)
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
                    ( Standalone (Name "z", char 'y')
                        :| [Standalone (Name "v", char 'x')]
                    )
                    (refFreeLocal (Name "z"))
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
                    (char 'a' `eq` char 'x')
                    ( ifThenElse
                        (char 'b' `eq` char 'y')
                        ( lets
                            ( IR.Standalone (Name "b", char 'y')
                                :| [IR.Standalone (Name "a", char 'x')]
                            )
                            ( application
                                (refBound (Index 0 1))
                                (refBound (Index 0 0))
                            )
                        )
                        ( lets
                            ( IR.Standalone (Name "o2", char 'y')
                                :| [IR.Standalone (Name "o1", char 'x')]
                            )
                            ( application
                                (refBound (Index 0 0))
                                (refBound (Index 0 1))
                            )
                        )
                    )
                    ( lets
                        ( IR.Standalone (Name "o2", char 'y')
                            :| [IR.Standalone (Name "o1", char 'x')]
                        )
                        ( application
                            (refBound (Index 0 0))
                            (refBound (Index 0 1))
                        )
                    )
              )

--------------------------------------------------------------------------------
-- Helper functions ------------------------------------------------------------

representedCase
  :: MonadFail m
  => [Cfn.Expr Cfn.Ann]
  -> [Cfn.CaseAlternative Cfn.Ann]
  -> m IR.Exp
representedCase es alts = runRepresentM (mkCase es (NE.fromList alts))

runRepresentM :: MonadFail m => RepM Exp -> m Exp
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
          }
        rm
    )

--------------------------------------------------------------------------------
-- Fixture ---------------------------------------------------------------------

ann :: Cfn.Ann
ann = Nothing

cfnModule :: forall {a}. Cfn.Module a
cfnModule =
  Cfn.Module
    { moduleName = PS.ModuleName "M"
    , modulePath = "M.purs"
    , moduleImports = mempty
    , moduleExports = mempty
    , moduleReExports = mempty
    , moduleForeign = mempty
    , moduleBindings = mempty
    }

cfnQualifyModule :: a -> PS.Qualified a
cfnQualifyModule = PS.Qualified (PS.ByModuleName (PS.ModuleName "ModuleName"))

cfnLocalIdent :: Text -> PS.Qualified PS.Ident
cfnLocalIdent = PS.Qualified (PS.BySourcePos (PS.SourcePos 0 0)) . PS.Ident

cfnRef :: Text -> Cfn.Expr Cfn.Ann
cfnRef = Cfn.Var ann . cfnLocalIdent

cfnBool :: Bool -> Cfn.Expr Cfn.Ann
cfnBool b = Cfn.Literal ann (Cfn.BooleanLiteral b)

cfnInt :: Integer -> Cfn.Expr Cfn.Ann
cfnInt i = Cfn.Literal ann (Cfn.NumericLiteral (Left i))

cfnFloat :: Double -> Cfn.Expr Cfn.Ann
cfnFloat f = Cfn.Literal ann (Cfn.NumericLiteral (Right f))

cfnCharE :: Char -> Cfn.Expr Cfn.Ann
cfnCharE = Cfn.Literal ann . cfnCharL

cfnCharL :: Char -> Cfn.Literal a
cfnCharL = Cfn.CharLiteral

cfnArray :: [Cfn.Expr Cfn.Ann] -> Cfn.Expr Cfn.Ann
cfnArray a = Cfn.Literal ann (Cfn.ArrayLiteral a)

cfnObject :: [(Text, Cfn.Expr Cfn.Ann)] -> Cfn.Expr Cfn.Ann
cfnObject o = Cfn.Literal ann $ Cfn.ObjectLiteral (first PS.mkString <$> o)

cfnLitB :: Cfn.Literal (Cfn.Binder Cfn.Ann) -> Cfn.Binder Cfn.Ann
cfnLitB = Cfn.LiteralBinder ann

cfnVarB :: PS.Ident -> Cfn.Binder Cfn.Ann
cfnVarB = Cfn.VarBinder ann

cfnNamB :: PS.Ident -> Cfn.Binder Cfn.Ann -> Cfn.Binder Cfn.Ann
cfnNamB = Cfn.NamedBinder ann

cfnNullB :: Cfn.Binder Cfn.Ann
cfnNullB = Cfn.NullBinder ann

cfnApp :: Cfn.Expr Cfn.Ann -> Cfn.Expr Cfn.Ann -> Cfn.Expr Cfn.Ann
cfnApp = Cfn.App ann

let1 :: Name -> Exp -> Exp -> Exp
let1 n e = lets (pure (IR.Standalone (n, e)))
