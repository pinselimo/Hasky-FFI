{- |
Module          : Foreign.Pythas.Parser
Description     : Parse Haskell type statements.
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Parsing functions to convert the contents of a Haskell
    module file to a list of the occurring top level type
    declarations.
 -}
module Foreign.Pythas.Parser (
      parseTypeDefs, parseIfTypeDef, parseTypeDef
    , parseExports, parseModname
) where

import qualified Text.Parsec.Token as P
import Data.Functor (($>))
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec
import Prelude hiding (mod)

import Foreign.Pythas.HTypes (HType(..), htype)
import Foreign.Pythas.Utils (TypeDef(..))

parseExports :: Parser [String]
parseExports = parseModname *> parens (commaSep $ strip funcName) <* whe

parseModname :: Parser String
parseModname = skip *> mod *> funcName
skip = whiteSpace
strip x = skip *> x <* skip

parseTypeDefs :: Parser [TypeDef]
parseTypeDefs = manyTill parseIfTypeDef eof

parseIfTypeDef :: Parser TypeDef
parseIfTypeDef = manyTill skipLine isTypeDef *> parseTypeDef

parseTypeDef :: Parser TypeDef
parseTypeDef = do
  fname <- funcName
  types <- typeDef *> parseTypes
  manyTill (skipLine <|> [] <$ whiteSpace) (() <$ isTypeDef <|> eof)
  return $ TypeDef fname types

parseTypes :: Parser [HType]
parseTypes = skip *> sepBy1 (strip parseType) (strip arrow) <* skipLine

parseType :: Parser HType
parseType = func <|> tuple <|> list <|> io <|> unit <|> htype

skipLine = manyTill anyToken (endOfLine <|> semi <|> ('\n' <$ eof))

io    = HIO    <$> (try iomonad *> parseType)
unit  = HUnit  <$  (parens skip)
func  = HFunc  <$> (isFunc *> parens (sepBy1 (strip parseType) (strip arrow)))
tuple = HTuple <$> (isTuple *> parens (commaSep $ strip parseType))
list  = HList  <$> (brackets (strip parseType))

-- Checkers:
isFunc = try $ lookAhead $ parens (identifier *> many1 (strip $ arrow *> parseType))
isTuple = try $ lookAhead $ parens (parseType *> many1 (strip $ comma *> parseType))
isTypeDef = try $ lookAhead parseTypeDef

-- Lexer:
lexer = P.makeTokenParser haskellDef

mod = P.reserved lexer "module"
whe = P.reserved lexer "where"
commaSep = P.commaSep lexer
parens = P.parens lexer
brackets = P.brackets lexer
barrow = P.reservedOp lexer "=>"
arrow = P.reservedOp lexer "->"
iomonad = P.reservedOp lexer "IO"
typeDef = P.reservedOp lexer "::"
semi = P.semi lexer $> ';'
comma = P.comma lexer
funcName = P.identifier lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer

