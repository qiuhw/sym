module Sym.Parser (parseStmt) where

import Control.Monad (liftM)
import Text.Parsec (ParseError, (<|>), (<?>), parse)
import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)
import qualified Text.Parsec.Token as Parsec
import Text.Parsec.Language (haskellStyle)

import Sym.SyntaxTree (Expr(..))

parseStmt :: String -> Either ParseError Expr
parseStmt = parse expr "(unknown)"

expr =  buildExpressionParser table term
    <?> "expression"

term =  parens expr
    <|> liftM ID identifier
    <?> "simple expression"

table = [ [binary "not inside" Not AssocLeft]
        , [binary "interact" And AssocLeft, binary "inside" And AssocLeft]
        , [binary "or" Or AssocLeft]
        ]

prefix  name fun = Prefix  (reservedOp name >> return fun )
postfix name fun = Postfix (reservedOp name >> return fun)
binary  name fun = Infix   (reservedOp name >> return fun )

lexer = Parsec.makeTokenParser haskellStyle

parens     = Parsec.parens lexer
identifier = Parsec.identifier lexer
reservedOp = Parsec.reservedOp lexer
