module Carbon.Parser 
  ( parseExpr
  , parseProgram 
  ) where

import Carbon.AST
import Control.Arrow ((>>>))
import qualified Control.Monad.Combinators.Expr as E 
import Data.Functor
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- HELPERS
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

block :: Parser [Expr]
block = between (symbol "{") (symbol "}") (many space1 *> program)

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` symbol ",")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

name :: Parser Name
name = lexeme $ (:) 
  <$> (letterChar <|> oneOf "_'") 
  <*> many (alphaNumChar <|> oneOf "_'")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

program :: Parser [Expr]
program = expr `sepEndBy` (void (some $ lexeme eol) <|> void (symbol ";"))

symbol :: String -> Parser String
symbol = L.symbol hspace

-- EXPR
expr :: Parser Expr
expr = E.makeExprParser atom table 
 where
  atom = choice 
    [ arrayLit
    , boolLit
    , for
    , function
    , if_
    , return_
    , stringLit
    , throw
    , tryCatch
    , unitLit
    , numLit 
    , while
    , var
    , parens expr 
    ]
  table = 
    [ [ E.Postfix calls
      , E.Postfix indexes 
      , dot
      ]
    , [ prefix "-" NegOp
      , prefix "!" NotOp 
      ]
    , [ binary ".." RangeOp 
      ]
    , [ binary' "*" MulOp
      , binary' "/" DivOp 
      , binary' "%" ModOp 
      ]
    , [ binary' "+" AddOp
      , binary' "-" SubOp 
      ]
    , [ binary "<=" LessEqOp
      , binary ">=" GreaterEqOp
      , binary "<" LessOp 
      , binary ">" GreaterOp 
      ]
    , [ binary "==" EqEqOp
      , binary "!=" NotEqOp 
      ]
    , [ binary "&&" AndOp 
      ]
    , [ binary "||" OrOp 
      ]
    , [ binary "=" EqOp 
      , binary ":=" DeclOp
      , binary "+=" AddEqOp
      , binary "-=" SubEqOp
      , binary "*=" MulEqOp
      , binary "/=" DivEqOp
      , binary "%=" ModEqOp
      ]
    ]
  binary s op = E.InfixL $ Infix op <$ symbol s
  binary' s op = E.InfixL $ Infix op <$ try (symbol s <* notFollowedBy (symbol "="))
    -- for operators with compound counterparts
  dot = E.InfixL $ Infix DotOp <$ try (symbol "." <* notFollowedBy (symbol "."))
  prefix s op = E.Prefix $ Prefix op <$ symbol s

arrayLit :: Parser Expr
arrayLit = ArrayLit <$> brackets 
  (many space1 *> commaSep (many space1 *> expr) <* many space1)

boolLit :: Parser Expr
boolLit = BoolLit <$> choice 
  [ symbol "false" $> False,
    symbol "true" $> True ]

calls :: Parser (Expr -> Expr)
calls = do
  apps <- some . parens $ commaSep expr
  pure $ \fn -> foldl Call fn apps

for :: Parser Expr
for = do
  symbol "for"
  varName <- name
  symbol "in"
  array <- expr
  For varName array <$> block

function :: Parser Expr
function = do
  symbol "fn"
  params <- commaSep name
  Function params <$> block

indexes :: Parser (Expr -> Expr)
indexes = do
  indices <- some $ brackets expr
  pure $ \fn -> foldl Index fn indices

if_ :: Parser Expr
if_ = do
  symbol "if"
  cond <- expr
  body <- block
  elseBody <- option [] $ try do
    space
    symbol "else" 
    block <|> (pure <$> if_)
  pure $ If cond body elseBody

return_ :: Parser Expr
return_ = symbol "return" *> (Return <$> option UnitLit expr)

stringLit :: Parser Expr
stringLit = StringLit <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

throw :: Parser Expr
throw = symbol "throw" *> (Throw <$> expr)

tryCatch :: Parser Expr
tryCatch = do
  symbol "try"
  body <- block
  symbol "catch"
  errName <- name
  catchBody <- block
  pure $ Try body errName catchBody

unitLit :: Parser Expr
unitLit = symbol "unit" $> UnitLit

numLit :: Parser Expr
numLit = NumLit . read <$> lexeme (some digitChar)

var :: Parser Expr
var = Var <$> name

while :: Parser Expr
while = do
  symbol "while"
  cond <- expr
  While cond <$> block

parseExpr :: String -> Either String Expr
parseExpr = parse (expr <* eof) "" >>> \case
  Right ast -> Right ast 
  Left err -> Left $ errorBundlePretty err

parseProgram :: String -> Either String [Expr]
parseProgram = parse (program <* eof) "" >>> \case
  Right ast -> Right ast
  Left err -> Left $ errorBundlePretty err
