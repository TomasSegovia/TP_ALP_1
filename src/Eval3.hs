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
lookfor v (s, trace) = case M.lookup v s of
                    Just n -> Right n
                    Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v n (s, trace) = (M.insert v n s, trace)

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
stepComm = undefined

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp = undefined
