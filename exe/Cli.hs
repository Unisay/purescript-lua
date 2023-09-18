{-# LANGUAGE QuasiQuotes #-}

module Cli where

import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NE
import Data.Tagged (Tagged (..))
import Data.Text (splitOn)
import Data.Text qualified as Text
import Language.PureScript.Backend.Types (AppOrModule (..))
import Language.PureScript.Names qualified as PS
import Options.Applicative
  ( Parser
  , eitherReader
  , execParser
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
import Path (reldir, relfile, Dir, File, SomeBase (..), parseSomeDir, parseSomeFile)
import Prettyprinter (Doc, annotate, flatAlt, indent, line, vsep, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..))
import Prettyprinter.Render.Terminal qualified as PT

data Args = Args
  { foreignPath ∷ Tagged "foreign" (SomeBase Dir)
  , psOutputPath ∷ Tagged "output" (SomeBase Dir)
  , luaOutputFile ∷ Tagged "output-lua" (SomeBase File)
  , appOrModule ∷ AppOrModule
  }
  deriving stock (Show)

options ∷ Parser Args
options = do
  foreignPath ←
    option
      (eitherReader (bimap displayException Tagged . parseSomeDir))
      ( fold
          [ metavar "FOREIGN-PATH"
          , long "foreign-path"
          , value $ Tagged $ Rel [reldir|foreign|]
          , helpDoc . Just $
              "Path to a directory containing foreign files."
                <> linebreak
                <> bold "Default: foreign"
          ]
      )
  psOutputPath ←
    option
      (eitherReader (bimap displayException Tagged . parseSomeDir))
      ( fold
          [ metavar "PS-PATH"
          , long "ps-output"
          , value $ Tagged $ Rel [reldir|output|]
          , helpDoc . Just $
              "Path to purs output directory."
                <> linebreak
                <> bold "Default: output"
          ]
      )
  luaOutputFile ←
    option
      (eitherReader (bimap displayException Tagged . parseSomeFile))
      ( fold
          [ metavar "LUA-OUT-FILE"
          , long "lua-output-file"
          , value $ Tagged $ Rel [relfile|main.lua|]
          , helpDoc . Just $
              "Path to write compiled Lua file to."
                <> linebreak
                <> bold "Default: main.lua"
          ]
      )
  appOrModule ←
    option (eitherReader parseAppOrModule) . fold $
      [ metavar "ENTRY"
      , short 'e'
      , long "entry"
      , value $ AsApplication (PS.ModuleName "Main") (PS.Ident "main")
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
  [name] | isModule name → pure . AsModule $ PS.ModuleName name
  segments → do
    let name = last (NE.fromList segments)
    pure
      if isModule name
        then AsModule . PS.ModuleName $ Text.intercalate "." segments
        else
          let modname = Text.intercalate "." (init (NE.fromList segments))
           in AsApplication (PS.ModuleName modname) (PS.Ident name)
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
