module Language.PureScript.Backend.Lua.Gen where

import Data.Text qualified as Text
import Hedgehog (Gen, Range)
import Hedgehog.Gen.Extended qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.Lua.Name (Name, unsafeName)
import Language.PureScript.Backend.Lua.Printer (printStatement)
import Language.PureScript.Backend.Lua.Types (ParamF (..))
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Prelude hiding (local, return)

chunk ∷ Gen Lua.Chunk
chunk = Gen.list (Range.linear 1 16) statement

statement ∷ Gen Lua.Statement
statement = Gen.recursiveFrequency nonRecursiveStatements recursiveStatements

nonRecursiveStatement ∷ Gen Lua.Statement
nonRecursiveStatement = Gen.frequency nonRecursiveStatements

nonRecursiveStatements ∷ [(Int, Gen Lua.Statement)]
nonRecursiveStatements =
  [ (2, Lua.return <$> expression)
  , (2, assign)
  , (3, local)
  , (1, foreignSourceCode)
  ]

assign ∷ Gen Lua.Statement
assign = Lua.assign <$> nonRecursiveVar <*> expression

local ∷ Gen Lua.Statement
local = Lua.local <$> name <*> Gen.maybe expression

ifThenElse ∷ Gen Lua.Statement
ifThenElse = do
  cond ← expression
  then' ← Gen.list (Range.linear 1 5) statement
  else' ← Gen.list (Range.linear 1 5) statement
  pure $ Lua.ifThenElse cond then' else'

recursiveStatements ∷ [(Int, Gen Lua.Statement)]
recursiveStatements = [(2, ifThenElse)]

foreignSourceCode ∷ Gen Lua.Statement
foreignSourceCode =
  Lua.ForeignSourceCode
    . renderStrict
    . layoutPretty defaultLayoutOptions
    . printStatement
    . Lua.return
    <$> table

tableRow ∷ Gen Lua.TableRow
tableRow =
  Gen.frequency
    [ (1, Lua.tableRowKV <$> expression <*> expression)
    , (2, Lua.tableRowNV <$> name <*> expression)
    ]

name ∷ Gen Name
name = do
  firstChar ← Gen.frequency [(8, Gen.alpha), (2, Gen.constant '_')]
  let nextChar = Gen.frequency [(8, Gen.alphaNum), (2, Gen.constant '_')]
  followingChars ← Gen.list (Range.linearFrom 3 1 16) nextChar
  pure . unsafeName $ Text.cons firstChar (Text.pack followingChars)

expression ∷ Gen Lua.Exp
expression = Gen.recursiveFrequency nonRecursiveExpressions recursiveExpressions

nonRecursiveExpression ∷ Gen Lua.Exp
nonRecursiveExpression = Gen.frequency nonRecursiveExpressions

nonRecursiveExpressions ∷ [(Int, Gen Lua.Exp)]
nonRecursiveExpressions =
  [ (2, nil)
  , (1, literalBool)
  , (2, literalInt)
  , (1, literalFloat)
  , (2, literalString)
  , (3, Lua.var <$> nonRecursiveVar)
  ]

nil ∷ Gen Lua.Exp
nil = Gen.constant Lua.Nil

literalBool ∷ Gen Lua.Exp
literalBool = Lua.Boolean <$> Gen.bool

literalInt ∷ Gen Lua.Exp
literalInt = Lua.Integer <$> Gen.integral integerRange
 where
  integerRange ∷ Range Integer
  integerRange = fromIntegral <$> (Range.exponentialBounded ∷ Range Int64)

literalFloat ∷ Gen Lua.Exp
literalFloat =
  Lua.Float
    <$> Gen.double (Range.exponentialFloatFrom 0 (-1234567890.0) 1234567890)

literalString ∷ Gen Lua.Exp
literalString = Lua.String <$> Gen.text (Range.linear 1 16) Gen.unicode

nonRecursiveVar ∷ Gen Lua.Var
nonRecursiveVar = Gen.frequency [(1, Lua.VarName <$> name)]

recursiveExpressions ∷ [(Int, Gen Lua.Exp)]
recursiveExpressions =
  [ (3, function)
  , (1, unOp)
  , (2, binOp)
  , (1, table)
  , (5, recursiveVar)
  , (3, functionCall)
  ]

function ∷ Gen Lua.Exp
function =
  Lua.functionDef
    <$> Gen.list
      (Range.linear 0 5)
      (maybe ParamUnused ParamNamed <$> Gen.maybe name)
    <*> chunk

unOp ∷ Gen Lua.Exp
unOp = Lua.unOp <$> Gen.enumBounded <*> expression

binOp ∷ Gen Lua.Exp
binOp = Lua.binOp <$> Gen.enumBounded <*> expression <*> expression

table ∷ Gen Lua.Exp
table = Lua.table <$> Gen.list (Range.linear 0 5) tableRow

recursiveVar ∷ Gen Lua.Exp
recursiveVar = do
  Gen.choice
    [ Lua.varIndex <$> expression <*> expression
    , Lua.varField <$> expression <*> name
    ]

functionCall ∷ Gen Lua.Exp
functionCall =
  Lua.functionCall <$> expression <*> Gen.list (Range.linear 0 5) expression
