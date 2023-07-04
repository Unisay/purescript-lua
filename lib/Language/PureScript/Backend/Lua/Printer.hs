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

printLuaChunk ∷ Lua.Chunk → ADoc
printLuaChunk = vsep . fmap printStatement

printStatement ∷ Lua.Statement → ADoc
printStatement = \case
  Lua.Assign (Ann variable) (Ann expr) →
    printAssign variable expr
  Lua.Local name value →
    printLocal name (printedExp . unAnn <$> value)
  Lua.IfThenElse (Ann predicate) thenBlock elseBlock →
    printIfThenElse predicate (unAnn <$> thenBlock) (unAnn <$> elseBlock)
  Lua.Return (Ann expr) →
    "return" <+> printedExp expr
  Lua.ForeignSourceCode code →
    pretty code

printAssign ∷ Lua.Var → Lua.Exp → ADoc
printAssign variable expr = printVar variable <+> "=" <+> printedExp expr

-- | Printed expression without a precedence
printedExp ∷ Lua.Exp → ADoc
printedExp = snd . printExp

printExp ∷ Lua.Exp → PADoc
printExp = \case
  Lua.Nil → (PrecAtom, "nil")
  Lua.Boolean b → (PrecAtom, if b then "true" else "false")
  Lua.Float f → (PrecAtom, pretty f)
  Lua.Integer i → (PrecAtom, pretty i)
  Lua.String t → (PrecAtom, dquotes (pretty t))
  Lua.Function args body →
    let argNames =
          args >>= \case
            Ann (ParamNamed n) → [n]
            Ann ParamUnused → []
     in (PrecFunction, printFunction argNames (unAnn <$> body))
  Lua.TableCtor rows → (PrecTable, printTableCtor (unAnn <$> rows))
  Lua.UnOp op (Ann a) → printUnaryOp op (printExp a)
  Lua.BinOp op (Ann l) (Ann r) → printBinaryOp op (printExp l) (printExp r)
  Lua.Var (Ann v) → (PrecAtom, printVar v)
  Lua.FunctionCall (Ann prefix) args →
    ( PrecPrefix
    , printFunctionCall
        (printExp prefix)
        (printExp . unAnn <$> args)
    )

printUnaryOp ∷ Lua.UnaryOp → PADoc → PADoc
printUnaryOp op (_, a) = (prec op, pretty (sym op) <> parens a)

printBinaryOp ∷ Lua.BinaryOp → PADoc → PADoc → PADoc
printBinaryOp op l r =
  (prec op, wrapPrec op l <+> pretty (sym op) <+> wrapPrec op r)

printFunction ∷ [Lua.Name] → [Lua.Statement] → ADoc
printFunction params body =
  sep [group ("function" <> tupled fparams), flex fbody, "end"]
 where
  fparams = printName <$> params
  fbody = printStatement <$> body

printTableCtor ∷ [Lua.TableRow] → ADoc
printTableCtor [] = "{}"
printTableCtor tableRows = sep [lbrace, flex rows, rbrace]
 where
  rows = punctuate comma $ fmap printRow tableRows

printRow ∷ Lua.TableRow → ADoc
printRow = \case
  Lua.TableRowKV (Ann kexp) (Ann vexp) →
    brackets (printedExp kexp) <+> "=" <+> printedExp vexp
  Lua.TableRowNV name (Ann vexp) →
    printName name <+> "=" <+> printedExp vexp

printVar ∷ Lua.Var → ADoc
printVar = \case
  Lua.VarName name → printName name
  Lua.VarIndex (Ann e) (Ann i) → printedExp e <> brackets (printedExp i)
  Lua.VarField (Ann e) n → wrapPrec PrecAtom (printExp e) <> "." <> printName n

printFunctionCall ∷ PADoc → [PADoc] → ADoc
printFunctionCall prefix args =
  wrapPrec PrecPrefix prefix
    <> parens (hsep (punctuate comma (snd <$> args)))

printLocal ∷ Lua.Name → Maybe ADoc → ADoc
printLocal name value =
  "local" <+> case value of
    Nothing → printName name
    Just v → printName name <+> "=" <+> v

printRequire ∷ Lua.ChunkName → ADoc
printRequire name =
  vsep ["require" <> parens (dquotes (printChunkName name)), ""]

printBlock ∷ NonEmpty Statement → ADoc
printBlock (NE.toList → statements) =
  sep ["do", flex (printStatement <$> statements), "end"]

printIfThenElse
  ∷ Lua.Exp
  → [Statement]
  → [Statement]
  → ADoc
printIfThenElse predicate thenBlock elseBlock =
  sep . join $
    [ [hsep ["if", printedExp predicate, "then"], thenDoc]
    , if not (null elseBlock)
        then ["else", flex (printStatement <$> elseBlock)]
        else []
    , ["end"]
    ]
 where
  thenDoc = flex (printStatement <$> thenBlock)

printName ∷ Lua.Name → ADoc
printName = pretty

printChunkName ∷ Lua.ChunkName → ADoc
printChunkName = pretty

--------------------------------------------------------------------------------
-- Utility functions -----------------------------------------------------------

flex ∷ Foldable t ⇒ t ADoc → ADoc
flex b =
  flatAlt
    (indent 2 $ vsep $ toList b) -- if doesn't fit one line
    (hsep $ toList b) -- when fits into one line

wrapPrec ∷ HasPrecedence p ⇒ p → PADoc → ADoc
wrapPrec = wrapPrecWith (>)

wrapPrecGte ∷ HasPrecedence p ⇒ p → PADoc → ADoc
wrapPrecGte = wrapPrecWith (>=)

wrapPrecWith
  ∷ HasPrecedence p
  ⇒ (Precedence → Precedence → Bool)
  → p
  → PADoc
  → ADoc
wrapPrecWith f p1 (p2, doc)
  | prec p1 `f` p2 = parens doc
  | otherwise = doc
