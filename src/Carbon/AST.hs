module Carbon.AST where

type Name = String
type Block = [Expr]

data Op = 
  EqOp
  | AddOp
  | AddEqOp
  | DeclOp
  | DivOp
  | DivEqOp
  | ModOp
  | ModEqOp
  | MulOp
  | MulEqOp
  | SubOp
  | SubEqOp
  | EqEqOp
  | NotEqOp
  | LessOp
  | LessEqOp
  | GreaterOp
  | GreaterEqOp
  | AndOp
  | OrOp
  | NotOp
  | NegOp
  | RangeOp
  deriving (Eq, Show)

data Expr = 
  ArrayLit [Expr]
  | BoolLit Bool
  | Call Expr [Expr]
  | For Name Expr Block
  | Function [Name] Block
  | If Expr Block Block
  | Index Expr Expr
  | Infix Op Expr Expr
  | NumLit Int
  | Prefix Op Expr
  | Return Expr
  | StringLit String
  | UnitLit
  | Var Name
  | While Expr Block
  deriving (Eq, Show)
