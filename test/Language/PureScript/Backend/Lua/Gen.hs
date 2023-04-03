module Language.PureScript.Backend.Lua.Gen where

import Data.Text qualified as Text
import Hedgehog (Gen, Range)
import Hedgehog.Gen.Extended qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.Lua.Name (Name, unsafeName)
import Language.PureScript.Backend.Lua.Printer (printStatement)
import Language.PureScript.Backend.Lua.Types
  ( Chunk
  , Exp (..)
  , ModuleName (..)
  , QualifiedName (..)
  , Statement (..)
  , TableRow (..)
  , Var (..)
  )
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Prelude hiding (local)

chunk :: Gen Chunk
chunk = Gen.list (Range.linear 1 16) statement

statement :: Gen Statement
statement = Gen.recursiveFrequency nonRecursiveStatements recursiveStatements

nonRecursiveStatements :: [(Int, Gen Statement)]
nonRecursiveStatements =
  [ (2, Return <$> expression)
  , (2, assign)
  , (3, local)
  , (1, foreignSourceCode)
  ]

assign :: Gen Statement
assign = do
  len <- Gen.int (Range.linear 1 5)
  vars <- Gen.nonEmpty (Range.singleton len) nonRecursiveVar
  vals <- Gen.nonEmpty (Range.singleton len) expression
  pure $ Assign vars vals

local :: Gen Statement
local = do
  len <- Gen.int (Range.linear 1 5)
  vars <- Gen.nonEmpty (Range.singleton len) name
  vals <- Gen.list (Range.constant 0 len) expression
  pure $ Local vars vals

ifThenElse :: Gen Statement
ifThenElse = do
  cond <- expression
  then' <- Gen.nonEmpty (Range.linear 1 5) statement
  elsif <-
    Gen.list
      (Range.linear 0 3)
      ((,) <$> expression <*> Gen.nonEmpty (Range.linear 1 5) statement)
  else' <- Gen.maybe (Gen.nonEmpty (Range.linear 1 5) statement)
  pure $ IfThenElse cond then' elsif else'

recursiveStatements :: [(Int, Gen Statement)]
recursiveStatements = [(2, ifThenElse)]

foreignSourceCode :: Gen Statement
foreignSourceCode =
  ForeignSourceCode
    . renderStrict
    . layoutPretty defaultLayoutOptions
    . printStatement
    . Return
    <$> table

tableRow :: Gen TableRow
tableRow =
  Gen.frequency
    [ (1, TableRowKV <$> expression <*> expression)
    , (2, TableRowNV <$> name <*> expression)
    , (3, TableRowV <$> expression)
    ]

name :: Gen Name
name = do
  firstChar <- Gen.frequency [(8, Gen.alpha), (2, Gen.constant '_')]
  let nextChar = Gen.frequency [(8, Gen.alphaNum), (2, Gen.constant '_')]
  followingChars <- Gen.list (Range.linearFrom 3 1 16) nextChar
  pure . unsafeName $ Text.cons firstChar (Text.pack followingChars)

moduleName :: Gen ModuleName
moduleName = do
  firstChar <- Gen.upper
  let nextChar = Gen.frequency [(8, Gen.alphaNum), (2, Gen.constant '_')]
  followingChars <- Gen.list (Range.linearFrom 3 1 16) nextChar
  let text = Text.cons firstChar (Text.pack followingChars)
  pure . ModuleName $ unsafeName text

qualifiedName :: Gen QualifiedName
qualifiedName =
  Gen.frequency
    [ (1, ImportedName <$> moduleName <*> name)
    , (3, LocalName <$> name)
    ]

expression :: Gen Exp
expression = Gen.recursiveFrequency nonRecursiveExpressions recursiveExpressions

nonRecursiveExpressions :: [(Int, Gen Exp)]
nonRecursiveExpressions =
  [ (2, nil)
  , (1, boolean)
  , (2, integer)
  , (1, float)
  , (2, string)
  , (3, Var <$> nonRecursiveVar)
  ]

nil :: Gen Exp
nil = Gen.constant Nil

boolean :: Gen Exp
boolean = Boolean <$> Gen.bool

integer :: Gen Exp
integer = Integer <$> Gen.integral integerRange
 where
  integerRange :: Range Integer
  integerRange = fromIntegral <$> (Range.exponentialBounded :: Range Int64)

float :: Gen Exp
float =
  Float <$> Gen.double (Range.exponentialFloatFrom 0 (-1234567890.0) 1234567890)

string :: Gen Exp
string = String <$> Gen.text (Range.linear 1 16) Gen.unicode

nonRecursiveVar :: Gen Var
nonRecursiveVar = Gen.frequency [(1, VarName <$> qualifiedName)]

recursiveExpressions :: [(Int, Gen Exp)]
recursiveExpressions =
  [ (3, function)
  , (1, unOp)
  , (2, binOp)
  , (1, table)
  , (5, recursiveVar)
  , (3, functionCall)
  ]

function :: Gen Exp
function = Function <$> Gen.list (Range.linear 0 5) name <*> chunk

unOp :: Gen Exp
unOp = UnOp <$> Gen.enumBounded <*> expression

binOp :: Gen Exp
binOp = BinOp <$> Gen.enumBounded <*> expression <*> expression

table :: Gen Exp
table = TableCtor <$> Gen.list (Range.linear 0 5) tableRow

recursiveVar :: Gen Exp
recursiveVar = fmap Var do
  Gen.choice
    [ VarIndex <$> expression <*> expression
    , VarField <$> expression <*> name
    ]

functionCall :: Gen Exp
functionCall =
  FunctionCall <$> expression <*> Gen.list (Range.linear 0 5) expression
