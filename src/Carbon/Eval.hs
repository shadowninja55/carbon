{-# LANGUAGE ViewPatterns #-}
module Carbon.Eval 
  ( interpret 
  ) where

import Carbon.AST qualified as AST
import Carbon.Parser
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Vector qualified as V
import Data.Vector.Growable qualified as GV
import System.IO (hFlush, stdout)

data Value = 
  Array (GV.GrowableIOVector Value)
  | Bool Bool
  | Builtin ([Value] -> Interpreter Value)
  | Closure [AST.Name] [AST.Expr] [Scope]
  | Unit
  | Num Int
  | String String
type Scope = M.Map AST.Name (IORef Value)
type Interpreter = ExceptT Value (StateT [Scope] IO)

builtins :: M.Map AST.Name Value
builtins = M.fromList
  [ ("clone", Builtin bClone)
  , ("eval", Builtin bEval)
  , ("include", Builtin bInclude)
  , ("length", Builtin bLength)
  , ("print", Builtin bPrint)
  , ("prompt", Builtin bPrompt)
  , ("push", Builtin bPush)
  , ("show", Builtin bShow) 
  ]

bClone :: [Value] -> Interpreter Value
bClone [arg] = case arg of
  Array array -> do
    array' <- GV.freeze array
    Array <$> GV.thaw array'
  value -> pure value

bEval :: [Value] -> Interpreter Value
bEval [String arg] = case parseExpr arg of
  Right expr -> eval expr
  Left err -> error $ "eval: failed to parse expression passed to builtin `eval`\n" <> err

bInclude :: [Value] -> Interpreter Value
bInclude [String arg] = do
  source <- liftIO $ readFile arg
  let program = case parseProgram source of
       Right program -> program
       Left err -> error $ "eval: failed to parse expression passed to builtin `include`\n" <> err
  Unit <$ traverse_ eval program

bLength :: [Value] -> Interpreter Value
bLength = \case 
  [Array array] -> Num <$> GV.length array
  [String string] -> pure . Num $ length string

bPrint :: [Value] -> Interpreter Value
bPrint [arg] = do
  String string <- case arg of
    string@(String _) -> pure string
    value -> bShow [value]
  liftIO $ putStrLn string
  pure Unit

bPrompt :: [Value] -> Interpreter Value
bPrompt [String arg] = String <$> liftIO do
  putStr arg
  hFlush stdout
  getLine

bPush :: [Value] -> Interpreter Value
bPush [Array array, value] = Unit <$ GV.push array value

bShow :: [Value] -> Interpreter Value
bShow [arg] = String <$> showValue arg

-- HELPERS
declareVar :: AST.Name -> Value -> Interpreter ()
declareVar name value = do
  ref <- liftIO $ newIORef value
  modify $ declare ref
 where
  declare ref (scope : scopes) = M.insert name ref scope : scopes

getVar :: AST.Name -> Interpreter Value
getVar name = gets search >>= liftIO . readIORef
 where
  search = \case 
    [] -> error $ "getVar: variable `" ++ name ++ "` not found"
    scope : scopes -> case M.lookup name scope of
      Nothing -> search scopes
      Just value -> value

mutateVar :: AST.Name -> Value -> Interpreter ()
mutateVar name value = do
  mutation <- gets mutate
  liftIO mutation
 where
  mutate = \case
    [] -> error $ "mutateVar: variable `" ++ name ++ "` not found"
    scope : scopes -> case M.lookup name scope of
      Nothing -> mutate scopes
      Just ref -> writeIORef ref value

eqValue :: Value -> Value -> Interpreter Bool
eqValue = curry $ \case
  (Array xs, Array ys) -> (==) <$> GV.length xs <*> GV.length ys >>= \case
    True -> do
      xs' <- V.toList <$> GV.freeze xs
      ys' <- V.toList <$> GV.freeze ys
      and <$> zipWithM eqValue xs' ys'
    False -> pure False
  (Bool x, Bool y) -> pure $ x == y
  (Unit, Unit) -> pure True
  (Num x, Num y) -> pure $ x == y
  (String x, String y) -> pure $ x == y
  _ -> pure False

showValue :: Value -> Interpreter String
showValue = \case
  Array array -> do
    values <- V.toList <$> GV.freeze array
    shown <- traverse showValue values
    pure $ "[" ++ intercalate ", " shown ++ "]"
  Bool False -> pure "false"
  Bool True -> pure "true"
  Builtin _ -> pure "builtin"
  Closure params _ _ -> pure $ "fn (" ++ intercalate ", " params ++ ")"
  Unit -> pure "unit"
  Num n -> pure $ show n
  String string -> pure $ show string

-- EVAL
eval :: AST.Expr -> Interpreter Value
eval = \case
  AST.ArrayLit exprs -> do
    values <- traverse eval exprs
    Array <$> GV.thaw (V.fromList values)
  AST.BoolLit bool -> pure $ Bool bool
  AST.Call fnExpr argExprs -> do
    args <- traverse eval argExprs
    eval fnExpr >>= \case
      Builtin f -> f args
      fn@Closure {} -> evalCall fn args
      _ -> error "eval: only a function or builtin can be called"
  AST.Declare name expr -> do
    value <- eval expr
    declareVar name value
    pure value
  AST.For name arrayExpr block -> do
    Array array <- eval arrayExpr
    values <- V.toList <$> GV.freeze array
    traverse_ evalBody values
    pure Unit
   where
    evalBody value = do
      ref <- liftIO $ newIORef value
      evalBlock block $ M.singleton name ref
  AST.Function params body -> do
    scopes <- gets init
    pure $ Closure params body scopes
  AST.If cond block elseBlock -> evalCond cond >>= \case
    True -> evalBlock block M.empty
    False -> evalBlock elseBlock M.empty
  AST.Index arrayExpr indexExpr -> do
    Num index <- eval indexExpr
    eval arrayExpr >>= \case
      Array array -> GV.read array index
      String string -> pure $ String [string !! index]
      _ -> error "eval: invalid type for `Index` expression"
  AST.Infix AST.EqOp lvalue rvalue -> evalAssign lvalue rvalue
  AST.Infix op leftExpr rightExpr -> do
    (left, right) <- (,) <$> eval leftExpr <*> eval rightExpr
    evalInfix op left right 
  AST.UnitLit -> pure Unit
  AST.NumLit num -> pure $ Num num
  AST.Prefix op expr -> (op ,) <$> eval expr <&> \case
    (AST.NotOp, Bool bool) -> Bool $ not bool
    (AST.NegOp, Num n) -> Num $ -n
    _ -> error "eval: invalid `Prefix` expression"
  AST.Return expr -> get >>= \case
    [_] -> error "eval: `return` not allowed at top level"
    _ -> eval expr >>= throwError
  AST.StringLit string -> pure $ String string
  AST.Var name -> getVar name
  AST.While cond block -> Unit <$ whileM_ (evalCond cond) (evalBlock block M.empty)

evalAssign :: AST.Expr -> AST.Expr -> Interpreter Value
evalAssign lvalue rvalue = do
  value <- eval rvalue
  case lvalue of
    AST.Var name -> do
      mutateVar name value
      pure value
    AST.Index arrayExpr indexExpr -> do 
      Array array <- eval arrayExpr
      Num index <- eval indexExpr
      GV.write array index value
      pure value
    _ -> error "eval: invalid lvalue for assignment"

evalCond :: AST.Expr -> Interpreter Bool
evalCond cond = eval cond <&> \case
  Bool bool -> bool
  _ -> error "eval: condition of loop must be of type Bool"

evalInfix :: AST.Op -> Value -> Value -> Interpreter Value
evalInfix op left right = case (op, left, right) of
  (AST.AddOp, Array xs, Array ys) -> do
    vxs <- GV.freeze xs
    vys <- GV.freeze ys
    zs <- GV.withCapacity $ V.length vxs + V.length vys
    traverse_ (GV.push zs) vxs
    traverse_ (GV.push zs) vys
    pure $ Array zs
  (AST.AddOp, String x, String y) -> pure . String $ x ++ y
  (AST.EqEqOp, _, _) -> Bool <$> eqValue left right
  (AST.NotEqOp, _, _) -> Bool . not <$> eqValue left right
  (AST.AndOp, Bool x, Bool y) -> pure . Bool $ x && y
  (AST.OrOp, Bool x, Bool y) -> pure . Bool $ x || y
  (AST.RangeOp, Num x, Num y) -> do
    let range = V.generate (y - x + 1) (Num . (+ x))
    Array <$> GV.thaw range
  (_, Num x, Num y) -> pure $ case op of
    AST.AddOp -> Num $ x + y
    AST.SubOp -> Num $ x - y
    AST.MulOp -> Num $ x * y
    AST.DivOp -> Num $ x `div` y
    AST.ModOp -> Num $ x `mod` y
    AST.LessOp -> Bool $ x < y
    AST.LessEqOp -> Bool $ x <= y
    AST.GreaterOp -> Bool $ x > y
    AST.GreaterEqOp -> Bool $ x >= y
    _ -> error "eval: unimplemented infix operator for type `Num`"
  (_, x, y) -> do
    sx <- showValue x
    sy <- showValue y
    error $ "eval: unimplemented infix expression `" ++ sx ++ " " ++ show op
      ++ " " ++ sy ++ "`"

evalBlock :: [AST.Expr] -> Scope -> Interpreter Value
evalBlock block env = do
  modify (env :)
  result <- evalExprs block
  modify tail
  pure result

evalCall :: Value -> [Value] -> Interpreter Value
evalCall (Closure params body closureScopes) args = do
  argRefs <- liftIO $ traverse newIORef args
  let bodyEnv = M.fromList $ zip params argRefs
  globalEnv <- gets last
  (either id id -> result, last -> globalEnv') <- liftIO $ runStateT 
    (runExceptT $ evalExprs body) ((bodyEnv : closureScopes) ++ [globalEnv])
  modify $ update globalEnv'
  pure result
 where
  update globalEnv' scopes = init scopes ++ [globalEnv']
evalCall _ _ = error "eval: only a function can be called"

evalExprs :: [AST.Expr] -> Interpreter Value
evalExprs = \case
  [] -> pure Unit
  [expr] -> eval expr
  expr:exprs -> eval expr *> evalExprs exprs

interpret :: [AST.Expr] -> IO ()
interpret program = do
  globalEnv <- traverse newIORef builtins
  void $ runStateT (runExceptT $ traverse_ eval program) [globalEnv]
