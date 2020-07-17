module Foreign.Hasky.ParseExports (parseExports, parseModname) where

import Prelude hiding (mod)
import Text.Parsec.String (Parser)

import HaskyFFI.ParseUtils (mod, parens, commaSep, funcName, whe, strip, skip)

parseExports :: Parser [String]
parseExports = parseModname *> parens (commaSep $ strip funcName) <* whe

parseModname :: Parser String
parseModname = skip *> mod *> funcName

