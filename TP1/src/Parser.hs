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
intexp = (do {reservedOp lis "-"; e <- intexp; return (UMinus e)})
         <|> parens lis intexp
         <|> chainl1 intexp (do {reservedOp lis "+"; return (Plus)})
         <|> chainl1 intexp (do {reservedOp lis "-"; return (Minus)})
         <|> chainl1 intexp (do {reservedOp lis "*"; return (Times)})
         <|> chainl1 intexp (do {reservedOp lis "/"; return (Div)})
         <|> (do {num <- natural lis; return (Const $ fromIntegral num)})
         <|> try (do i <- identifier lis
                     reservedOp lis "++"
                     return (VarInc i))
         <|> try (do i <- identifier lis
                     reservedOp lis "--"
                     return (VarDec i))
         <|> try (do i <- identifier lis
                     return (Var i))


------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = (do {reserved lis "true"; return (BTrue)})
          <|> (do {reserved lis "false"; return (BFalse)})
          <|> (do reservedOp lis "!"
                  e <- boolexp
                  return (Not e))
          <|> parens lis boolexp
          <|> chainl1 boolexp (do {reservedOp lis "&&"; return (And)}) 
          <|> chainl1 boolexp (do {reservedOp lis "||"; return (Or)})
          <|> do { ei <-intexp; reservedOp lis "=="; ed <- intexp; return (Eq ei ed)}
          <|> do { ei <-intexp; reservedOp lis "!="; ed <- intexp; return (NEq ei ed)}
          <|> do { ei <-intexp; reservedOp lis "<";  ed <- intexp; return (Lt ei ed)}
          <|> do { ei <-intexp; reservedOp lis ">";  ed <- intexp; return (Gt ei ed)}

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
