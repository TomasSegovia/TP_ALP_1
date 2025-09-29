module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until", "case"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp =  try addOpParser
          <|> termexp

termexp :: Parser (Exp Int)
termexp = try mulOpParser
          <|> try factorexp

factorexp :: Parser (Exp Int)
factorexp = try parensIntParser <|> try uMinusParser
          <|> try atomexp

atomexp :: Parser (Exp Int)
atomexp = try varIncParser <|> try natParser <|> try varParser

natParser :: Parser (Exp Int)
natParser = do
          n <- (natural lis)
          return (Const (fromInteger n))

varParser :: Parser (Exp Int)
varParser = do
          v <- identifier lis
          return (Var v)

varIncParser :: Parser (Exp Int)
varIncParser = do
          v <- identifier lis
          reservedOp lis "++"
          return (VarInc v)

uMinusParser :: Parser (Exp Int)
uMinusParser = do
          reservedOp lis "-"
          e <- factorexp
          return (UMinus e)
  
addOpParser :: Parser (Exp Int)
addOpParser = termexp `chainl1` addOp where
  addOp = do
            reservedOp lis "+"
            return (Plus)
          <|> do 
            reservedOp lis "-"
            return (Minus)
  
mulOpParser :: Parser (Exp Int)
mulOpParser = factorexp `chainl1` mulOp where
  mulOp = do
            reservedOp lis "*"
            return (Times)
          <|> do 
            reservedOp lis "/"
            return (Div)

parensIntParser :: Parser (Exp Int)
parensIntParser = do
          e <- parens lis intexp
          return (e)

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp =   try orParser <|>
            boolexp2

boolexp2 :: Parser (Exp Bool)
boolexp2 =  try andParser <|>
            boolterm

boolterm :: Parser (Exp Bool)
boolterm = try negParser <|> try boolfactor

boolfactor :: Parser (Exp Bool)
boolfactor =  try logOpParser <|> try parensBoolParser
              <|> boolatom

boolatom :: Parser (Exp Bool)
boolatom =  do
              reserved lis "true"
              return (BTrue)
            <|> do
              reserved lis "false"
              return (BFalse)

orParser :: Parser (Exp Bool)
orParser = boolexp2 `chainl1` orOp where
  orOp = do
            reservedOp lis "||"
            return (Or)

andParser :: Parser (Exp Bool)
andParser = boolterm `chainl1` andOp where
  andOp = do
            reservedOp lis "&&"
            return (And)

logOpParser :: Parser (Exp Bool)
logOpParser = do 
          e1 <- intexp
          res <- equal e1 <|> inequal e1 <|> less e1 <|> great e1
          return res where
  equal b = do
          reservedOp lis "=="
          e2 <- intexp
          return (Eq b e2)
  inequal b = do
          reservedOp lis "!="
          e2 <- intexp
          return (NEq b e2)
  less b = do
          reservedOp lis "<"
          e2 <- intexp
          return (Lt b e2)
  great b = do
          reservedOp lis ">"
          e2 <- intexp
          return (Gt b e2)

negParser :: Parser (Exp Bool)
negParser = do
          reservedOp lis "!"
          e <- boolfactor
          return (Not e)

parensBoolParser :: Parser (Exp Bool)
parensBoolParser = do
          e <- parens lis boolexp
          return (e)

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm =      try seqParser <|> commterm

commterm :: Parser Comm
commterm =  try asigParser <|> try ifThenElseParser
            <|> try ifThenParser <|> try caseParser <|> try repParser
            <|> skipParser

seqParser :: Parser Comm
seqParser = commterm `chainl1` seqOp where
  seqOp = do
          reservedOp lis ";"
          return (Seq)

asigParser :: Parser Comm
asigParser = do 
        v <- identifier lis
        reservedOp lis "="
        e <- intexp
        return (Let v e)

ifThenParser :: Parser Comm
ifThenParser = do
        reserved lis "if"
        b <- boolexp
        c <- braces lis comm
        return (IfThen b c)

ifThenElseParser :: Parser Comm
ifThenElseParser = do
        reserved lis "if"
        b <- boolexp
        c1 <- braces lis comm
        reserved lis "else"
        c2 <- braces lis comm
        return (IfThenElse b c1 c2)

caseParser :: Parser Comm
caseParser = do
        reserved lis "case"
        c <- braces lis inCase
        return c where
  inCase = do
        b <- boolexp
        reservedOp lis ":"
        c <- braces lis comm
        res <- inCase
        return (IfThenElse b c res)
      <|> 
        return Skip

repParser :: Parser Comm
repParser = do
        reserved lis "repeat"
        c <- braces lis comm
        reserved lis "until"
        b <- boolexp
        return (RepeatUntil c b)

skipParser :: Parser Comm
skipParser = do
              reserved lis "skip"
              return (Skip)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
