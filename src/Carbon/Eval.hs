{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Carbon.Eval 
  ( interpret 
  ) where

import Carbon.AST qualified as AST
import Carbon.Parser
import Control.Monad (join, zipWithM)
import Control.Monad.Loops
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Vector qualified as V
import Data.Vector.Growable qualified as GV
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import System.IO (hFlush, stdout)

data Value 
  = Array (GV.GrowableIOVector Value)
  | Bool Bool
  | Builtin Builtin
  | Closure [AST.Name] [AST.Expr] [Scope]
  | Unit
  | Num Int
  | String String
type Scope = M.Map AST.Name (IORef Value)
type Builtin = forall es. Interpreter :>> es => [Value] -> Eff es Value 
type Interpreter = [Error Exception, Error Value, IOE, State [Scope]]
newtype Exception = Exception Value

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

bClone :: Builtin
bClone [arg] = case arg of
  Array array -> Array <$> liftIO (GV.freeze array >>= GV.thaw)
  value -> pure value

bEval :: Builtin
bEval [String arg] = case parseExpr arg of
  Right expr -> eval expr
  Left err -> throwString $ "failed to parse expression passed to builtin `eval`\n" <> err

bInclude :: Builtin
bInclude [String arg] = do
  source <- liftIO $ readFile arg
  program <- case parseProgram source of
       Right program -> pure program
       Left err -> throwString $ "failed to parse expression passed to builtin `include`\n" <> err
  Unit <$ traverse_ eval program

bLength :: Builtin
bLength = \case 
  [Array array] -> Num <$> liftIO (GV.length array)
  [String string] -> pure . Num $ length string

bPrint :: Builtin
bPrint [arg] = do
  value <- case arg of
    string@(String _) -> pure string
    value -> bShow [value]
  case value of
    String s -> Unit <$ liftIO (putStrLn s)

bPrompt :: Builtin
bPrompt [String arg] = String <$> liftIO do
  putStr arg
  hFlush stdout
  getLine

bPush :: Builtin
bPush [Array array, value] = Unit <$ liftIO (GV.push array value)

bShow :: Builtin
bShow [arg] = String <$> showValue arg

-- HELPERS
declareVar :: Interpreter :>> es => AST.Name -> Value -> Eff es ()
declareVar name value = do
  ref <- liftIO $ newIORef value
  modify $ declare ref
 where
  declare ref (scope : scopes) = M.insert name ref scope : scopes

getVar :: Interpreter :>> es => AST.Name -> Eff es Value
getVar name = join $ gets search
 where
  search = \case 
    [] -> throwString $ "variable `" ++ name ++ "` not found"
    scope : scopes -> case M.lookup name scope of
      Nothing -> search scopes
      Just value -> liftIO $ readIORef value

mutateVar :: Interpreter :>> es => AST.Name -> Value -> Eff es ()
mutateVar name value = join $ gets mutate
 where
  mutate = \case
    [] -> throwString $ "variable `" ++ name ++ "` not found"
    scope : scopes -> case M.lookup name scope of
      Nothing -> mutate scopes
      Just ref -> liftIO $ writeIORef ref value

eqValueIO :: Value -> Value -> IO Bool
eqValueIO = curry $ \case
  (Array xs, Array ys) -> (==) <$> liftIO (GV.length xs) <*> liftIO (GV.length ys) >>= \case
    True -> do
      xs' <- V.toList <$> liftIO (GV.freeze xs)
      ys' <- V.toList <$> liftIO (GV.freeze ys)
      and <$> zipWithM eqValueIO xs' ys'
    False -> pure False
  (Bool x, Bool y) -> pure $ x == y
  (Unit, Unit) -> pure True
  (Num x, Num y) -> pure $ x == y
  (String x, String y) -> pure $ x == y
  _ -> pure False

eqValue :: Interpreter :>> es => Value -> Value -> Eff es Bool
eqValue = liftIO .: eqValueIO
 where
  (.:) = (.) . (.)

showValueIO :: Value -> IO String
showValueIO = \case
  Array array -> do
    values <- V.toList <$> liftIO (GV.freeze array)
    shown <- traverse showValueIO values
    pure $ "[" ++ intercalate ", " shown ++ "]"
  Bool False -> pure "false"
  Bool True -> pure "true"
  Builtin _ -> pure "builtin"
  Closure params _ _ -> pure $ "fn (" ++ intercalate ", " params ++ ")"
  Unit -> pure "unit"
  Num n -> pure $ show n
  String string -> pure $ show string

showValue :: Interpreter :>> es => Value -> Eff es String
showValue = liftIO . showValueIO

throwString :: Error Exception :> es => String -> Eff es a
throwString = throwError . Exception . String

-- EVAL
eval :: Interpreter :>> es => AST.Expr -> Eff es Value
eval = \case
  AST.ArrayLit exprs -> do
    values <- traverse eval exprs
    Array <$> liftIO (GV.thaw $ V.fromList values)
  AST.BoolLit bool -> pure $ Bool bool
  AST.Call fnExpr argExprs -> do
    args <- traverse eval argExprs
    eval fnExpr >>= \case
      Builtin f -> f args
      fn@Closure {} -> evalCall fn args
      _ -> throwString "only a function or builtin can be called"
  AST.For name arrayExpr block -> eval arrayExpr >>= \case
    Array array -> do
      values <- V.toList <$> liftIO (GV.freeze array)
      traverse_ evalBody values
      pure Unit
    _ -> throwString "for loops can only iterate over arrays"
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
  AST.Index arrayExpr indexExpr -> eval indexExpr >>= \case
    Num index -> eval arrayExpr >>= \case
      Array array -> liftIO (GV.read array index)
      String string -> pure $ String [string !! index]
      _ -> throwString "value cannot be indexed into"
    value -> throwString "value cannot be used as index"
  AST.Infix AST.DeclOp (AST.Var name) rvalue -> do
    value <- eval rvalue 
    declareVar name value
    pure value
  AST.Infix op lvalue rvalue 
    | op `elem` [AST.AddEqOp, AST.DivEqOp, AST.EqOp, AST.ModEqOp, AST.MulEqOp, AST.SubEqOp] 
    -> evalAssign op lvalue rvalue
  AST.Infix op leftExpr rightExpr -> do
    (left, right) <- (,) <$> eval leftExpr <*> eval rightExpr
    evalInfix op left right 
  AST.UnitLit -> pure Unit
  AST.NumLit num -> pure $ Num num
  AST.Prefix op expr -> (op ,) <$> eval expr >>= \case
    (AST.NegOp, Num n) -> pure $ Num -n
    (AST.NotOp, Bool bool) -> pure . Bool $ not bool
    _ -> throwString "invalid prefix expression"
  AST.Return expr -> get @[Scope] >>= \case
    [_] -> throwString "return not allowed at top level"
    _ -> eval expr >>= throwError
  AST.StringLit string -> pure $ String string
  AST.Throw expr -> eval expr >>= throwError . Exception
  AST.TryCatch body errName catchBody -> runErrorNoCallStack @Exception (evalBlock body M.empty) >>= \case
    Right value -> pure value
    Left (Exception value) -> do
      err <- liftIO $ newIORef value 
      evalBlock catchBody $ M.singleton errName err
  AST.Var name -> getVar name
  AST.While cond block -> Unit <$ whileM_ (evalCond cond) (evalBlock block M.empty)

evalAssign :: Interpreter :>> es => AST.Op -> AST.Expr -> AST.Expr -> Eff es Value
evalAssign op lvalue rvalue = do
  value <- eval $ case op of
    AST.EqOp -> rvalue
    op -> AST.Infix (fromCompound op) lvalue rvalue
  case lvalue of
    AST.Var name -> mutateVar name value
    AST.Index arrayExpr indexExpr -> eval arrayExpr >>= \case
      Array array -> eval indexExpr >>= \case
        Num index -> liftIO $ GV.write array index value
        _ -> throwString "value cannot be used as index"
      _ -> throwString "value cannot be indexed into"
    _ -> throwString "invalid lvalue for assignment"
  pure value
 where
  fromCompound = \case
    AST.AddEqOp -> AST.AddOp
    AST.DivEqOp -> AST.DivOp
    AST.ModEqOp -> AST.ModOp
    AST.MulEqOp -> AST.MulOp
    AST.SubEqOp -> AST.SubOp

evalCond :: Interpreter :>> es => AST.Expr -> Eff es Bool
evalCond cond = eval cond >>= \case
  Bool bool -> pure bool
  _ -> throwString "condition must be of type `Bool`"

evalInfix :: Interpreter :>> es => AST.Op -> Value -> Value -> Eff es Value
evalInfix op left right = case (op, left, right) of
  (AST.AddOp, Array xs, Array ys) -> liftIO do
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
    Array <$> liftIO (GV.thaw range)
  (_, Num x, Num y) -> case op of
    AST.AddOp -> pure . Num $ x + y
    AST.SubOp -> pure . Num $ x - y
    AST.MulOp -> pure . Num $ x * y
    AST.DivOp -> pure . Num $ x `div` y
    AST.ModOp -> pure . Num $ x `mod` y
    AST.LessOp -> pure . Bool $ x < y
    AST.LessEqOp -> pure . Bool $ x <= y
    AST.GreaterOp -> pure . Bool $ x > y
    AST.GreaterEqOp -> pure . Bool $ x >= y
    _ -> throwString "invalid infix expression"
  _ -> throwString "invalid infix expression"

evalBlock :: Interpreter :>> es => [AST.Expr] -> Scope -> Eff es Value
evalBlock block env = do
  modify (env :)
  result <- evalExprs block
  modify @[Scope] tail
  pure result

evalCall :: Interpreter :>> es => Value -> [Value] -> Eff es Value
evalCall (Closure params body closureScopes) args = do
  argRefs <- liftIO $ traverse newIORef args
  let bodyEnv = M.fromList $ zip params argRefs
  globalEnv <- gets last
  (either id id -> result, env') <- runState (bodyEnv : closureScopes ++ [globalEnv])
    . runErrorNoCallStack @Value
    $ evalExprs body
  modify . update $ last env'
  pure result
 where
  update globalEnv' scopes = init scopes ++ [globalEnv']
evalCall _ _ = throwString "invalid type in call expression"

evalExprs :: Interpreter :>> es => [AST.Expr] -> Eff es Value
evalExprs = \case
  [] -> pure Unit
  [expr] -> eval expr
  expr : exprs -> eval expr *> evalExprs exprs

interpret :: [AST.Expr] -> IO ()
interpret program = do
  globalEnv <- traverse newIORef builtins
  result <- runEff 
    . runErrorNoCallStack @Exception
    . void
    . runError @Value
    . runState [globalEnv] 
    $ traverse_ eval program
  case result of
    Left (Exception value) -> do
      s <- case value of 
        String s -> pure s
        v -> showValueIO value
      putStrLn $ "unhandled exception: " <> s
    _ -> pure ()
