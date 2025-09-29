module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = case M.lookup v s of
                    Just n -> n
                    Nothing -> error "Not found"

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm (Skip) s = (Skip :!: s)
stepComm (Let v e) s = let (res :!: news) = evalExp e s in (Skip :!: update v res news)
stepComm (Seq Skip c2) s = (c2 :!: s)
stepComm (Seq c1 c2) s = let (res :!: news) = stepComm c1 s in (Seq res c2 :!: news)
stepComm (IfThenElse b c1 c2) s = let (res :!: news) = evalExp b s in 
                                  if res then (c1 :!: news) else (c2 :!: news)
stepComm (RepeatUntil c b) s = (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)


-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = (n :!: s)
evalExp (Var v) s = ((lookfor v s) :!: s)
evalExp (UMinus e) s = let (res :!: news) = evalExp e s in (-res :!: news) 
evalExp (Plus e1 e2) s = let (res :!: news) = evalBinOp e1 e2 (+) s in
                          (res :!: news) 
evalExp (Minus e1 e2) s =  let (res :!: news) = evalBinOp e1 e2 (-) s in
                          (res :!: news) 
evalExp (Times e1 e2) s =  let (res :!: news) = evalBinOp e1 e2 (*) s in
                          (res :!: news) 
evalExp (Div e1 e2) s =  let (res :!: news) = evalBinOp e1 e2 div s in
                          (res :!: news) 
evalExp (VarInc v) s =  let   res = lookfor v s
                              news = update v (res + 1) s in
                        (res + 1 :!: news)
evalExp BTrue s =  (True :!: s)
evalExp BFalse s = (False :!: s)
evalExp (Lt e1 e2) s = let  (res :!: news) = evalBinOp e1 e2 (<) s in
                            (res :!: news) 
evalExp (Gt e1 e2) s =  let   (res :!: news) = evalBinOp e1 e2 (>) s in
                              (res :!: news) 
evalExp (And e1 e2) s =  let  (res :!: news) = evalBinOp e1 e2 (&&) s in
                              (res :!: news) 
evalExp (Or e1 e2) s =  let (res :!: news) = evalBinOp e1 e2 (||) s in
                            (res :!: news) 
evalExp (Eq e1 e2) s =  let (res :!: news) = evalBinOp e1 e2 (==) s in
                            (res :!: news) 
evalExp (NEq e1 e2) s =  let  (res :!: news) = evalBinOp e1 e2 (/=) s in
                              (res :!: news) 
evalExp (Not b) s = let (res :!: news) = evalExp b s in (not res :!: news) 

evalBinOp :: Exp a -> Exp a -> (a -> a -> b) -> State -> Pair b State
evalBinOp e1 e2 op s =  let   (res1 :!: mids) = evalExp e1 s
                              (res2 :!: news) = evalExp e2 mids in 
                        (op res1 res2 :!: news)