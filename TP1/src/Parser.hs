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
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "++"
                        , "--"
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
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = (do {reservedOp "-"; e <- intexp; return (UMinus e)})
         <|> parens intexp
         <|> chainl1 intexp (do {reservedOp lis "+"; return (Plus)})
         <|> chainl1 intexp (do {reservedOp lis "-"; return (Minus)})
         <|> chainl1 intexp (do {reservedOp lis "*"; return (Times)})
         <|> chainl1 intexp (do {reservedOp lis "/"; return (Div)})
         <|> (do {num <- natural lis; return (Const num)})
         <|> (do id <- identifier lis
                 (do reservedOp lis "++"
                     return (VarInc id))
                 <|> (do reservedOp lis "--"
                         return (VarDec id))
                 <|> return (Var id))


------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = (do {reserved "true"; return (BTrue)})
          <|> (do {reserved "false"; return (BFalse)})
          <|> (do reservedOp lis "!"
                  e <- boolexp
                  return (Not e))
          <|> parens boolexp
          <|> chainl1 boolexp (do {reservedOp "&&"; return (And)}) 
          <|> chainl1 boolexp (do {reservedOp "||"; return (Or)})
          <|> chainl1 intexp (do {reservedOp "=="; return (Eq)})
          <|> chainl1 intexp (do {reservedOp "!="; return (NEq)})
          <|> chainl1 intexp (do {reservedOp "<"; return (Lt)})
          <|> chainl1 intexp (do {reservedOp ">"; return (Gt)})

-----------------------------------
--- Parser de comandos
-----------------------------------

-- chainl1 comm (do {reservedOp lis ";"; return (Seq)}) == Seq comm comm
comm :: Parser Comm
comm = chainl1 comm (do {reservedOp lis ";"; return (Seq)}) <|> try parse_skip <|> try parse_if <|> try parse_repeat <|> parse_identifier

parse_skip = do reserved lis "skip"
                return Skip

parse_if = do reserved lis "if"
              b <- boolexp
              c1 <- braces lis comm
              try (do reserved lis "else"
                      c2 <- braces lis comm
                      return (IfThenElse b c1 c2))
              <|> return (IfThen b c1)

parse_repeat = do reserved lis "repeat"
                  c <- braces lis comm
                  reserved lis "until"
                  b <- boolexp
                  return (RepeatUntil c b)

parse_identifier = do d <- identifier lis
                      reservedOp lis "="
                      e <- intexp
                      return (Let d e)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
