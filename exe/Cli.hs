{-# LANGUAGE QuasiQuotes #-}

module Cli where

import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NE
import Data.Tagged (Tagged (..))
import Data.Text (splitOn)
import Data.Text qualified as Text
import Language.PureScript.Backend.AppOrModule (AppOrModule (..))
import Language.PureScript.CoreFn
import Options.Applicative
  ( Parser
  , eitherReader
  , execParser
  , flag
  , fullDesc
  , header
  , helpDoc
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , short
  , value
  )
import Path
  ( Dir
  , File
  , SomeBase (..)
  , parseSomeDir
  , parseSomeFile
  , reldir
  , relfile
  )
import Prettyprinter (Doc, annotate, flatAlt, indent, line, vsep, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..))
import Prettyprinter.Render.Terminal qualified as PT

data Args = Args
  { foreignPath ∷ Tagged "foreign" (SomeBase Dir)
  , psOutputPath ∷ Tagged "output" (SomeBase Dir)
  , luaOutputFile ∷ Tagged "output-lua" (SomeBase File)
  , outputIR ∷ Maybe ExtraOutput
  , outputLuaAst ∷ Maybe ExtraOutput
  , appOrModule ∷ AppOrModule
  }
  deriving stock (Show)

data ExtraOutput = OutputIR | OutputLuaAst
  deriving stock (Eq, Show)

options ∷ Parser Args
options = do
  foreignPath ←
    option (eitherReader (bimap displayException Tagged . parseSomeDir)) $
      fold
        [ metavar "FOREIGN-PATH"
        , long "foreign-path"
        , value $ Tagged $ Rel [reldir|foreign|]
        , helpDoc . Just $
            "Path to a directory containing foreign files."
              <> linebreak
              <> bold "Default: foreign"
        ]

  psOutputPath ←
    option (eitherReader (bimap displayException Tagged . parseSomeDir)) $
      fold
        [ metavar "PS-PATH"
        , long "ps-output"
        , value $ Tagged $ Rel [reldir|output|]
        , helpDoc . Just $
            "Path to purs output directory."
              <> linebreak
              <> bold "Default: output"
        ]

  luaOutputFile ←
    option (eitherReader (bimap displayException Tagged . parseSomeFile)) $
      fold
        [ metavar "LUA-OUT-FILE"
        , long "lua-output-file"
        , value $ Tagged $ Rel [relfile|main.lua|]
        , helpDoc . Just $
            "Path to write compiled Lua file to."
              <> linebreak
              <> bold "Default: main.lua"
        ]

  outputLuaAst ←
    flag Nothing (Just OutputLuaAst) . fold $
      [ long "output-lua-ast"
      , helpDoc . Just $
          "Output Lua AST."
            <> linebreak
            <> bold "Default: false"
      ]
  outputIR ←
    flag Nothing (Just OutputIR) . fold $
      [ long "output-ir"
      , helpDoc . Just $
          "Output IR."
            <> linebreak
            <> bold "Default: false"
      ]
  appOrModule ←
    option (eitherReader parseAppOrModule) . fold $
      [ metavar "ENTRY"
      , short 'e'
      , long "entry"
      , value $
          AsApplication
            (unsafeModuleNameFromText "Main")
            (Ident "main")
      , helpDoc . Just $
          vsep
            [ "Where to start compilation."
                <> softbreak
                <> "Could be one of the following formats:"
            , "- Application format:" <+> magenta "<Module>.<binding>"
            , green $ indent 2 "Example: Acme.App.main"
            , "- Module format:" <+> magenta "<Module>"
            , green $ indent 2 "Example: Acme.Lib"
            , bold "Default: Main.main"
            ]
      ]
  pure Args {..}

parseAppOrModule ∷ String → Either String AppOrModule
parseAppOrModule s = case splitOn "." (toText s) of
  [] → Left "Invalid entry point format"
  [name] | isModule name → maybeToRight "Invalid module name" do
    AsModule <$> moduleNameFromText name
  (NE.fromList → segments) →
    case moduleNameFromText (Text.intercalate "." (init segments)) of
      Nothing → Left $ "Invalid module name: " <> s
      Just mn →
        let segment = last segments
         in Right
              if isModule segment
                then AsModule mn
                else AsApplication mn (Ident segment)
 where
  isModule = Char.isAsciiUpper . Text.head

parseArguments ∷ IO Args
parseArguments =
  execParser $
    info
      (options <**> helper)
      ( fullDesc
          <> progDesc "Compile PureScript's CoreFn to Lua"
          <> header "pslua - a PureScript backend for Lua"
      )

--------------------------------------------------------------------------------
-- Helpers for pretty-printing -------------------------------------------------

linebreak ∷ Doc AnsiStyle
linebreak = flatAlt line mempty

softbreak ∷ Doc AnsiStyle
softbreak = PP.group linebreak

green ∷ Doc AnsiStyle → Doc AnsiStyle
green = annotate (PT.color Green)

magenta ∷ Doc AnsiStyle → Doc AnsiStyle
magenta = annotate (PT.color Magenta)

bold ∷ Doc AnsiStyle → Doc AnsiStyle
bold = annotate PT.bold
