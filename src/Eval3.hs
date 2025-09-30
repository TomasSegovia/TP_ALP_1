module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, [])

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (s, _) = case M.lookup v s of
                    Just n -> Right n
                    Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v n (s, trace) = addTrace ("Let " ++ v ++ " " ++ (show n) ++ " ") ((M.insert v n s), trace)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace str (s, trace) = (s, trace ++ str)

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Skip) s = Right (Skip :!: s)
stepComm (Let v e) s = case evalExp e s of 
                        Right (res :!: news) -> Right (Skip :!: update v res news)
                        Left err -> Left err
stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s = case stepComm c1 s of
                        Right (res :!: news) -> Right (Seq res c2 :!: news)
                        Left err -> Left err
stepComm (IfThenElse b c1 c2) s = case evalExp b s of
                        Right (res :!: news) -> if res then Right (c1 :!: news) else Right (c2 :!: news)
                        Left err -> Left err
stepComm (RepeatUntil c b) s = Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var v) s = case lookfor v s of
                          Left err -> Left err
                          Right n -> Right (n :!: s)
evalExp (UMinus e) s = case evalExp e s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (-res :!: news) 
evalExp (Plus e1 e2) s = case evalBinOp e1 e2 (+) s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (Minus e1 e2) s =  case evalBinOp e1 e2 (-) s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (Times e1 e2) s =  case evalBinOp e1 e2 (*) s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (Div e1 e2) s =  case evalDiv e1 e2 s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (VarInc v) s =  case lookfor v s of
                          Left err -> Left err
                          Right res -> let news = update v (res + 1) s in
                            Right (res + 1 :!: news)
evalExp BTrue s =  Right (True :!: s)
evalExp BFalse s = Right (False :!: s)
evalExp (Lt e1 e2) s = case evalBinOp e1 e2 (<) s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (Gt e1 e2) s = case evalBinOp e1 e2 (>) s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (And e1 e2) s = case evalBinOp e1 e2 (&&) s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (Or e1 e2) s = case evalBinOp e1 e2 (||) s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (Eq e1 e2) s = case evalBinOp e1 e2 (==) s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (NEq e1 e2) s = case evalBinOp e1 e2 (/=) s of 
                          Left err -> Left err
                          Right (res :!: news) -> Right (res :!: news) 
evalExp (Not b) s = case evalExp b s of
                          Left err -> Left err
                          Right (res :!: news) -> Right (not res :!: news) 

evalBinOp :: Exp a -> Exp a -> (a -> a -> b) -> State -> Either Error (Pair b State)
evalBinOp e1 e2 op s =  case evalExp e1 s of
                            Left err -> Left err
                            Right (res1 :!: mids) -> 
                              case evalExp e2 mids of
                                Left err -> Left err
                                Right (res2 :!: news) -> Right (op res1 res2 :!: news)

evalDiv :: Integral a => Exp a -> Exp a -> State -> Either Error (Pair a State)
evalDiv e1 e2 s =  case evalExp e1 s of
                            Left err -> Left err
                            Right (res1 :!: mids) -> 
                              case evalExp e2 mids of
                                Left err -> Left err
                                Right (0 :!: _) -> Left DivByZero
                                Right (res2 :!: news) -> Right (div res1 res2 :!: news)