{-# Language ViewPatterns #-}
module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones, aritméticas y booleanas
data Exp a where
  
  -- Expresiones enteras
  Const  :: Int -> Exp Int
  Var    :: Variable -> Exp Int
  UMinus :: Exp Int -> Exp Int
  Plus   :: Exp Int -> Exp Int -> Exp Int
  Minus  :: Exp Int -> Exp Int -> Exp Int
  Times  :: Exp Int -> Exp Int -> Exp Int
  Div    :: Exp Int -> Exp Int -> Exp Int
  VarInc :: Variable -> Exp Int

  -- Expresiones booleanas
  BTrue  :: Exp Bool
  BFalse :: Exp Bool
  Lt     :: Exp Int -> Exp Int -> Exp Bool
  Gt     :: Exp Int -> Exp Int -> Exp Bool
  And    :: Exp Bool -> Exp Bool -> Exp Bool
  Or     :: Exp Bool -> Exp Bool -> Exp Bool
  Not    :: Exp Bool -> Exp Bool
  Eq     :: Exp Int -> Exp Int -> Exp Bool
  NEq    :: Exp Int -> Exp Int -> Exp Bool  

deriving instance Show (Exp a)
deriving instance Eq (Exp a)

-- Comandos (sentencias)
-- Observar que sólo se permiten variables de tipo entero
data Comm
  = Skip
  | Let Variable (Exp Int)
  | Seq Comm Comm
  | IfThenElse (Exp Bool) Comm Comm
  | RepeatUntil Comm (Exp Bool)
  deriving (Show, Eq)

pattern IfThen :: Exp Bool -> Comm -> Comm
pattern IfThen b c = IfThenElse b c Skip

convertToIf :: [(Exp Bool, Comm)] -> Comm 
convertToIf xs = foldr (\(b, c) r -> IfThenElse b c r) Skip xs

convertToCase :: Comm -> [(Exp Bool, Comm)]
convertToCase (Skip) = []
convertToCase (IfThen b c) = [(b, c)]
convertToCase (IfThenElse b c r) = (b, c):(convertToCase r)

--convertToCase = go
--  where
--    go (IfThenElse b c Skip) = Just [(b,c)]
--    go (IfThenElse b c rest) = do
--      branches <- go rest
--      return ((b, c) : branches)
--    go _ = Nothing

pattern Case :: [(Exp Bool, Comm)] -> Comm
pattern Case xs <- (convertToCase -> xs) where
  Case xs = convertToIf xs

data Error = DivByZero | UndefVar deriving (Eq, Show)