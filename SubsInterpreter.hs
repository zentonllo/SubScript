module SubsInterpreter
       ( runProg
       , Error (..)
       , Value(..)
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Char -- Used to promote digits to chars

-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)

-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.

-- I left this constructor because it will be simpler to propagate the errors through the program
data Error = Error String
             deriving (Show, Eq)

type Env = Map Ident Value
type Primitive = [Value] -> SubsM Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialEnv :: Env
initialEnv = Map.empty
initialPEnv :: PEnv
initialPEnv = Map.fromList [ ("===", primEq)
                       , ("<", primLT)
                       , ("+", primPlus)
                       , ("*", primMul)
                       , ("-", primMinus)
                       , ("%", primMod)
                       , ("Array.new", arrayNew)
                       ]

initialContext :: Context 
initialContext = (Map.empty, initialPEnv) 

                     
                       
                       
newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

-- Instatiated both type classes as it was provided in the lecture slides
instance Functor SubsM where
  fmap f xs = xs >>= return . f

instance Applicative SubsM where
  pure = return
  (<*>) = ap

{-
I use the do syntax for the >>= operator because it is more readable. Nevertheless, an equivalent implementation would be:  
    f >>= m = SubsM $ \ (env, pEnv) -> case runSubsM f (env, pEnv) of 
                                          Left $ Error s -> Left $ Error s 
                                          Right (a, env') -> case runSubsM (m a) (env', pEnv) of 
                                                                Left $ Error s -> Left $ Error s
                                                                Right v -> Right v 
-}

instance Monad SubsM where
  return x = SubsM $ \ (env, _) -> Right (x, env)
  f >>= m = SubsM $ \ (env, pEnv) -> do (a, env') <- runSubsM f (env, pEnv)
                                        (a', env'') <- runSubsM (m a) (env', pEnv)
                                        return $ (a', env'')
  fail s = SubsM $ \ _ -> Left $ Error s


runProg :: Program -> Either Error Env
runProg prog = getResult (recursiveRun prog)

-- First I set the initial environment. Then I interpret the whole program and finally I get the resulting environment 
recursiveRun :: Program -> SubsM Env
recursiveRun p = do
                   putEnv initialEnv
                   program p
                   env <- getEnv
                   return $ env 
                   
program :: Program -> SubsM ()
program (Prog []) = return ()
program (Prog (x:xs)) = do
                          stm x
                          program $ Prog xs


-- For variables declared without a value I give them an "undefined" value                            
stm :: Stm -> SubsM ()
stm (VarDecl ident Nothing) = do
                                modify (insertEnv ident UndefinedVal)
                                return ()
                                
stm (VarDecl ident (Just expr) ) = do 
                                    val <- evalExpr expr
                                    modify(insertEnv ident val)
                                    return ()
stm (ExprAsStm expr) = do
                         evalExpr expr
                         return ()
                         
stm s = fail $ "The statement " ++ show s ++ " couldn't be interpreted (Doesn't fit pattern matching for SubScript)"  
  
arrayNew :: Primitive
arrayNew [IntVal n] | n > 0 = return $ ArrayVal(take n $ repeat UndefinedVal)
arrayNew _ = fail ("Array.new called with wrong number of arguments")

-- I've suppossed that two undefinedVal are equal ( I recalled that nil is equivalent to nil in other programming languages )
primEq :: Primitive
primEq [IntVal n, IntVal n' ] = if n == n' then return $ TrueVal
                                else return $ FalseVal
primEq [UndefinedVal, UndefinedVal] = return $ TrueVal
primEq [TrueVal, FalseVal] = return $ FalseVal
primEq [FalseVal, TrueVal] = return $ FalseVal
primEq [TrueVal, TrueVal] = return $ TrueVal
primEq [FalseVal, FalseVal] = return $ TrueVal
primEq [StringVal s, StringVal s'] = if s == s' then return $ TrueVal
                                     else return $ FalseVal
primEq [ArrayVal a1, ArrayVal a2] = if a1 == a2 then return $ TrueVal
                                    else return $ FalseVal
primEq _ = fail (" '===' called with wrong number of arguments ")

primLT :: Primitive
primLT [IntVal n, IntVal n'] = if n < n' then return $ TrueVal
                               else return $ FalseVal
primLT [StringVal s, StringVal s'] = if s < s' then return $ TrueVal
                                     else return $ FalseVal
primLT _ = fail (" '<' called with wrong number of arguments ")
                                     

primPlus :: Primitive
primPlus [IntVal n, IntVal n' ] = return $ IntVal $ n+n'
primPlus [IntVal n, StringVal s] = return $ StringVal $ (intToString n) ++ s
primPlus [StringVal s, IntVal n] = primPlus [IntVal n, StringVal s]
primPlus [StringVal s, StringVal s'] = return $ StringVal $ s++s'
primPlus _ = fail (" '+' arguments must be either integers or one string and one number ")

-- Convert each char of the string to a digit (I consider 0 as a different case)
intToString :: Int -> String
intToString 0 = [intToDigit 0]
intToString n = intToStringRec n []

intToStringRec :: Int -> String -> String
intToStringRec 0 s = s
intToStringRec n s = intToStringRec (n `div` 10) ( [ intToDigit $ n `mod` 10 ] ++ s )

primMul :: Primitive
primMul [IntVal n, IntVal n' ] = return $ IntVal $ n*n'
primMul _ = fail (" '*' arguments must be integers")

primMinus :: Primitive
primMinus [IntVal n, IntVal n' ] = return $ IntVal $ n-n'
primMinus _ = fail(" '-' arguments must be integers")

primMod :: Primitive
primMod [IntVal n, IntVal n' ] = return $ IntVal $ n `mod` n'
primMod _ = fail(" '%' arguments must be integers")
                           
-- Get the result from the monad using the initial context                            
getResult :: SubsM a -> Either Error a
getResult m = case runSubsM m initialContext of
                Left e -> Left e
                Right (a,_) -> Right a

-- Getter for the current environment                
getEnv :: SubsM Env
getEnv = SubsM $ \ (env, _) -> Right (env, env)

-- Getter for the environment of the build-in functions
getPEnv :: SubsM PEnv
getPEnv = SubsM $ \ (env, pEnv) -> Right (pEnv, env)

-- Setter for the current environment
putEnv :: Env -> SubsM () 
putEnv e = SubsM $ \ _ -> Right ((), e)
                                 
-- Modifier of the current environment                                 
modify :: (Env -> Env) -> SubsM ()
modify f = do
            env <- getEnv
            putEnv (f env)

-- Inserting the identifier and the value of a variable returning a function which takes an environment and returns the environment after having inserted the name and the value           
insertEnv :: Ident -> Value -> (Env -> Env)
insertEnv name val = Map.insert name val             
            
-- Update the value of a variable already defined in the current environment             
updateEnv :: Ident -> Value -> SubsM ()
updateEnv name val = do
                      env <- getEnv 
                      case Map.lookup name env of 
                        Nothing -> fail $ "Variable " ++ name ++ " not declared"
                        Just _ -> putEnv (Map.insert name val env)
                    
-- Getter for the value of a given variable
getVar :: Ident -> SubsM Value
getVar name = do
               env <- getEnv
               case Map.lookup name env of 
                    Nothing -> fail $ "Variable " ++ name ++ " not declared"
                    Just v -> return v
                    
-- Getter for primitive functions  
getFunction :: FunName -> SubsM Primitive
getFunction name = do
                    pEnv <- getPEnv 
                    case Map.lookup name pEnv of 
                        Nothing -> fail $ "Primitive function " ++ name ++ " doesn't exist"
                        Just p -> return p

                        
evalExpr :: Expr -> SubsM Value
evalExpr (Number n) = return $ IntVal n
evalExpr (String s) = return $ StringVal s

-- I stop if there is a expression inside the Array which couldn't be interpreted
evalExpr (Array lExpr) = do
                            env <- getEnv
                            case mapExpr lExpr env of 
                                Right v -> return $ ArrayVal v
                                Left s -> fail s
                            

    
evalExpr Undefined = return $ UndefinedVal
evalExpr TrueConst =  return $ TrueVal
evalExpr FalseConst = return $ FalseVal
evalExpr (Var ident) = do
                         v <- getVar ident 
                         return v


 

{-
    For evaluating array comprehension expressions I send all the information to an auxiliar function which keeps track of the environment used in every nested loop
    
    I only left as undefined the array comprehensions with an if statement which holds another for loop. The cause is that I didn't feel comfortable with the design I've approached.
    When I started to code this last case I realized that I couldn't use any of the functions defined previously in evalExprAux and that I had to consider more cases.
    This would make the code to grow exponentially so I assume that my approach for interpreting array comprehensions is not the right one. However, I haven't been able to get
    a different and easier one. There also a problem with the scopes of the variables defined in the for loops

-}
 
evalExpr (Compr (ident, expr, Nothing) expr2) = do 
                                                 env <- getEnv
                                                 evalExprAux (Compr (ident, expr, Nothing) expr2) env
                                                
                                                   

evalExpr (Compr (ident, expr, Just (ArrayForCompr a ) ) expr2 ) = do
                                                                    env <- getEnv
                                                                    evalExprAux (Compr (ident, expr, Just (ArrayForCompr a)) expr2) env
                                                                    
evalExpr (Compr (ident, expr, Just (ArrayIf expr' Nothing) ) expr2 ) = do
                                                                         env <- getEnv
                                                                         evalExprAux (Compr (ident, expr, Just (ArrayIf expr' Nothing ) ) expr2 ) env
                                                                         
evalExpr (Compr (ident, expr, Just (ArrayIf expr' (Just arrComp) )) expr2) = do
                                                                               env <- getEnv 
                                                                               evalExprAux (Compr (ident, expr, Just (ArrayIf expr' (Just arrComp) )) expr2) env 
                                                                                


-- I call the mapExpr function to get the values of the Expression list  
evalExpr (Call funName lExpr) = do
                                 prim <- getFunction funName
                                 env <- getEnv
                                 case mapExpr lExpr env of 
                                     Right v -> prim v
                                     Left s -> fail s
                                


evalExpr (Assign ident expr) = do
                                val <- evalExpr expr
                                updateEnv ident val 
                                return $ val
                              
evalExpr (Comma expr expr2) = do
                               evalExpr expr
                               evalExpr expr2
                               
evalExpr expr = fail $ "Expression " ++ show expr ++ " couldn't be interpreted (Doesn't fit pattern matching for SubScript)"                            

-- Maps a list of expressions to a list of values evaluating each one of the elements of the first list. Notice that I have to call runSubsM to get the value from
-- the monad with the current environment

mapExpr :: [Expr] -> Env -> Either String [Value]
mapExpr lExpr env = case x of
                  Left (Error s) -> Left s
                  Right (v,_) -> Right v
    where x = runSubsM (mapExpr2 lExpr) (env, initialPEnv)

-- Recursive function of mapExpr     
mapExpr2 :: [Expr] -> SubsM [Value]
mapExpr2 [] = return $ []
mapExpr2 (x:xs) = do
                  v <- evalExpr x
                  lVal <- mapExpr2 xs
                  return $ [v] ++ lVal

-- First of all, the auxiliar functions detect that the syntax of expr in the array is correct, then the results will be treated different since the strings require a bit of handling
evalExprAux :: Expr -> Env -> SubsM Value                        
evalExprAux (Compr (ident, expr, Nothing) expr2 ) env = do 
                                                          v <- evalExpr expr
                                                          case v of 
                                                            StringVal s ->  stringCase env expr2 ident s 
                                                            ArrayVal lVal -> arrayCase env expr2 ident lVal 
                                                            _ -> fail $ "Expression " ++ show expr ++ " not valid in Array syntax"
                                                             
evalExprAux (Compr (ident, expr, Just (ArrayForCompr a)) expr2) env = do
                                                                        v <- evalExpr expr
                                                                        case v of 
                                                                            StringVal s -> stringCase2 env expr2 ident s a
                                                                            ArrayVal lVal -> arrayCase2 env expr2 ident lVal a 
                                                                            _ -> fail $ "Expression " ++ show expr ++ " not valid in Array syntax"

evalExprAux (Compr (ident, expr, Just (ArrayIf expr' Nothing) ) expr2 ) env = do
                                                                                v <- evalExpr expr
                                                                                case v of 
                                                                                    StringVal s -> stringCase3 env expr2 ident s expr'
                                                                                    ArrayVal lVal -> arrayCase3 env expr2 ident lVal expr'
                                                                                    _ -> fail $ "Expression " ++ show expr ++ " not valid in Array syntax"
                                                                                 
--evalExprAux (Compr (ident, expr, Just (ArrayIf expr' (Just arrComp) )) expr2) env = undefined
-- Rather than leaving the previous function as undefined I prefered to leave it as impossible to interpret (Assuming that I restricted the SubScript language)
evalExprAux expr env = fail $ "The expression " ++ show expr ++ " couldn't be interpreted with the following environment " ++ show env



-- In this case I have an extra parameter, namely, the expression used for the if
arrayCase3 :: Env -> Expr -> Ident -> [Value] -> Expr -> SubsM Value
arrayCase3 env expr2 ident lVal expr' = do
                                         v <- mapEvalExprAux2 ident lVal expr' expr2
                                         putEnv env
                                         return v
                                         

stringCase3 :: Env -> Expr -> Ident -> String -> Expr -> SubsM Value
stringCase3 env expr2 ident s expr' = do 
                                        v <- mapEvalExprAux2 ident (toListStringVal s) expr' expr2
                                        putEnv env
                                        return v




mapEvalExprAux2 :: Ident -> [Value] -> Expr -> Expr -> SubsM Value
mapEvalExprAux2 _ [] _ _ = return $ ArrayVal []
mapEvalExprAux2 ident (x:xs) expr' expr2 = do
                                    modify (insertEnv ident x)
                                    e <- evalExpr expr'
                                    case e of
                                      TrueVal -> trueIf expr' expr2 ident xs 
                                      FalseVal -> falseIf expr' expr2 ident xs 
                                      _ -> fail $ "Expression " ++ show expr' ++ " not valid in Array syntax"
                                      
-- If the evaluation of the if was true, then the expression is evaluated and returned. Then, I keep on mapping environments                                  
trueIf :: Expr -> Expr -> Ident -> [Value] ->  SubsM Value
trueIf expr' expr2 ident xs  = do 
                     e' <- evalExpr expr2  
                     ArrayVal l2 <- mapEvalExprAux2 ident xs expr' expr2  
                     return $ ArrayVal $ [e'] ++ l2

-- If the evaluation of the if was false, then the expression is not evaluated but we keep on mapping environments                         
falseIf :: Expr -> Expr -> Ident -> [Value] -> SubsM Value 
falseIf expr' expr2 ident xs = do
                     ArrayVal l2 <- mapEvalExprAux2 ident xs expr' expr2  
                     return $ ArrayVal l2                                       
                                      
                                             



                       
-- These cases are pretty similar to the previous one (Terminal "for") but here I had to add an ArrayFor as a parameter                                  
arrayCase2 :: Env -> Expr -> Ident -> [Value] -> ArrayFor -> SubsM Value
arrayCase2 env expr2 ident lVal a = do 
                                      mapEvalExprAux ( mapEnv env (map (insertEnv ident) lVal) ) expr2 a env 

                                      
stringCase2 :: Env -> Expr -> Ident -> String -> ArrayFor -> SubsM Value
stringCase2 env expr2 ident s a = do 
                                      mapEvalExprAux  ( mapEnv env (map (insertEnv ident) (toListStringVal s)) ) expr2 a env 


-- See how in the first case I restore the previous environment                                       
mapEvalExprAux :: [Env] -> Expr ->  ArrayFor -> Env -> SubsM Value
mapEvalExprAux [] _ _ env = do
                             putEnv env
                             return $ ArrayVal []
                             
-- In these case I glue the arrays calling evalExprAux (which could have been called before) and mapEval recursively                            
mapEvalExprAux (x:xs) expr2 a env= do
                          putEnv x 
                          ArrayVal l1 <- evalExprAux (Compr a expr2) x
                          ArrayVal l2 <- mapEvalExprAux xs expr2 a env
                          return $ ArrayVal $ l1++l2 

                          

-- If there is a terminal "for" expression then depending of the value to iterate (either string or array) I'll have to insert 
-- different values in the "temporary environment" used in "appendValues". Eventually, the previous environment is restored   
arrayCase :: Env -> Expr -> Ident -> [Value] -> SubsM Value                          
arrayCase env expr2 ident lVal = do
                              l1 <- appendValues ident lVal expr2 
                              putEnv env 
                              return $ ArrayVal l1
                              
stringCase :: Env -> Expr -> Ident -> String -> SubsM Value
stringCase env expr2 ident s = do
                              l1 <- appendValues ident (toListStringVal s) expr2
                              putEnv env
                              return $ ArrayVal l1

-- Glue together the values returned after evaluating the expression with the temporary environments                                
appendValues :: Ident -> [Value] -> Expr -> SubsM [Value]
appendValues _ [] _ = return $ []
appendValues ident (x:xs) expr = do
                                modify (insertEnv ident x)
                                v <- evalExpr expr
                                lVal <- appendValues ident xs expr
                                return $ [v]++lVal
                                

                            
                            
-- Convert a list of chars (String) in a list of characters seen as Subs values (These characters are in fact a list of strings formed by that only character)                           
toListStringVal :: String -> [Value]
toListStringVal [] = []
toListStringVal (x:xs) = [StringVal [x]] ++ toListStringVal xs

-- Create the temporary environments after the ident took the different values of the string or the array
mapEnv :: Env -> [(Env -> Env)] -> [Env]
mapEnv _ [] = []
mapEnv env (x:xs) = [(x env)] ++ (mapEnv env xs) 
                            

