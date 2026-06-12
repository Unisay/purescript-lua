-- Char literals must be escaped in generated Lua: an unescaped '\n' inside
-- a quoted Lua string splits it across lines, producing a chunk no Lua
-- interpreter can parse.
module Golden.CharLiterals.Test where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log (show '\n')
  log (show '\t')
  log (show '\r')
  log (show '\'')
  log (show '\\')
  log (show 'a')
  log (show ('\n' == '\n'))
  log (show ('\t' < '\n'))
