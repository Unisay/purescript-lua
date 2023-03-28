module Language.PureScript.Backend.Lua.Printer where

import Data.List.NonEmpty qualified as NE
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Types
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , brackets
  , comma
  , dquotes
  , flatAlt
  , group
  , hsep
  , indent
  , lbrace
  , parens
  , punctuate
  , rbrace
  , sep
  , tupled
  , vsep
  , (<+>)
  )
import Prelude hiding (group)

-- | Document with no annotations
type ADoc = Doc ()

type PADoc = (Precedence, ADoc)

printLuaChunk :: Lua.Chunk -> ADoc
printLuaChunk = vsep . fmap printStatement

printStatement :: Lua.Statement -> ADoc
printStatement = \case
  Lua.Assign varlist explist ->
    printAssign varlist explist
  Lua.Local names values ->
    printLocal names (printedExp <$> values)
  Lua.IfThenElse predicate thenBlock elsifs elseBlock ->
    printIfThenElse predicate thenBlock elsifs elseBlock
  Lua.Block statements ->
    printBlock statements
  Lua.Return expr ->
    "return" <+> printedExp expr
  Lua.ForeignSourceCode code ->
    pretty code

printAssign :: NonEmpty Lua.Var -> NonEmpty Lua.Exp -> ADoc
printAssign vars exps = varlist <+> "=" <+> explist
 where
  varlist = hsep $ punctuate comma $ NE.toList $ fmap printVar vars
  explist = hsep $ punctuate comma $ NE.toList $ fmap printedExp exps

-- | Printed expression without a precedence
printedExp :: Lua.Exp -> ADoc
printedExp = snd . printExp

printExp :: Lua.Exp -> PADoc
printExp = \case
  Lua.Nil -> (PrecAtom, "nil")
  Lua.Boolean b -> (PrecAtom, if b then "true" else "false")
  Lua.Float f -> (PrecAtom, pretty f)
  Lua.Integer i -> (PrecAtom, pretty i)
  Lua.String t -> (PrecAtom, dquotes (pretty t))
  Lua.Function params body -> (PrecFunction, printFunction params body)
  Lua.TableCtor rows -> (PrecAtom, printTableCtor rows)
  Lua.UnOp op a -> printUnaryOp op (printExp a)
  Lua.BinOp op l r -> printBinaryOp op (printExp l) (printExp r)
  Lua.Var v -> (PrecAtom, printVar v)
  Lua.FunctionCall prefix args ->
    (PrecPrefix, printFunctionCall (printExp prefix) (printExp <$> args))

printUnaryOp :: Lua.UnaryOp -> PADoc -> PADoc
printUnaryOp op (_, a) = (prec op, pretty (sym op) <> parens a)

printBinaryOp :: Lua.BinaryOp -> PADoc -> PADoc -> PADoc
printBinaryOp op l r =
  (prec op, wrapPrec op l <+> pretty (sym op) <+> wrapPrec op r)

printFunction :: [Lua.Name] -> [Lua.Statement] -> ADoc
printFunction params body =
  sep [group ("function" <> tupled fparams), flex fbody, "end"]
 where
  fparams = printName <$> params
  fbody = printStatement <$> body

printTableCtor :: [Lua.TableRow] -> ADoc
printTableCtor [] = "{}"
printTableCtor tableRows = sep [lbrace, flex rows, rbrace]
 where
  rows = punctuate comma $ fmap printRow tableRows

printRow :: Lua.TableRow -> ADoc
printRow = \case
  Lua.TableRowKV kexp vexp ->
    brackets (printedExp kexp) <+> "=" <+> printedExp vexp
  Lua.TableRowNV name vexp ->
    printName name <+> "=" <+> printedExp vexp
  Lua.TableRowV vexp ->
    printedExp vexp

printVar :: Lua.Var -> ADoc
printVar = \case
  Lua.VarName name -> printQualifiedName name
  Lua.VarIndex e i -> printedExp e <> brackets (printedExp i)
  Lua.VarField e n -> printedExp e <> "." <> printName n

printFunctionCall :: PADoc -> [PADoc] -> ADoc
printFunctionCall prefix args =
  wrapPrec PrecPrefix prefix
    <> parens (hsep (punctuate comma (snd <$> args)))

printLocal :: NonEmpty Lua.Name -> [ADoc] -> ADoc
printLocal names values =
  hsep $ "local" : if null values then [namelist] else [namelist, "=", explist]
 where
  namelist = hsep (punctuate comma (NE.toList (fmap printName names)))
  explist = hsep (punctuate comma values)

printRequire :: Lua.ChunkName -> ADoc
printRequire name =
  vsep ["require" <> parens (dquotes (printChunkName name)), ""]

printBlock :: NonEmpty Statement -> ADoc
printBlock (NE.toList -> statements) =
  sep ["do", flex (printStatement <$> statements), "end"]

printIfThenElse
  :: Lua.Exp
  -> NonEmpty Statement
  -> [(Lua.Exp, NonEmpty Statement)]
  -> Maybe (NonEmpty Statement)
  -> ADoc
printIfThenElse predicate thenBlock elsifs elseBlock =
  sep . join $
    [ [hsep ["if", printedExp predicate, "then"], thenDoc]
    , elsifs >>= \(p, t) ->
        [hsep ["elseif", printedExp p, "then"], flex (printStatement <$> t)]
    , elseBlock & maybe [] \e -> ["else", flex (printStatement <$> e)]
    , ["end"]
    ]
 where
  thenDoc = flex (printStatement <$> thenBlock)

printQualifiedName :: Lua.QualifiedName -> ADoc
printQualifiedName = \case
  Lua.ImportedName (Lua.ModuleName modname) name ->
    printName modname <> "." <> printName name
  Lua.LocalName name -> printName name

printName :: Lua.Name -> ADoc
printName = pretty

printChunkName :: Lua.ChunkName -> ADoc
printChunkName = pretty

--------------------------------------------------------------------------------
-- Utility functions -----------------------------------------------------------

flex :: Foldable t => t ADoc -> ADoc
flex b =
  flatAlt
    (indent 2 $ vsep $ toList b) -- if doesn't fit one line
    (hsep $ toList b) -- when fits into one line

wrapPrec :: HasPrecedence p => p -> PADoc -> ADoc
wrapPrec = wrapPrecWith (>)

wrapPrecGte :: HasPrecedence p => p -> PADoc -> ADoc
wrapPrecGte = wrapPrecWith (>=)

wrapPrecWith
  :: HasPrecedence p
  => (Precedence -> Precedence -> Bool)
  -> p
  -> PADoc
  -> ADoc
wrapPrecWith f p1 (p2, doc)
  | prec p1 `f` p2 = parens doc
  | otherwise = doc
