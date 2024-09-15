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
intexp = chainl1 term sumOps

term = chainl1 factor mulOps

factor = (do {reservedOp lis "-"; e <- intexp; return (UMinus e)})
         <|> (do {num <- natural lis; return (Const $ fromIntegral num)})
         <|> (do {i <- identifier lis;
                  (do reservedOp lis "++"
                      return (VarInc i))
                  <|> (do reservedOp lis "--"
                          return (VarDec i))
                  <|> return (Var i)})
         <|> parens lis intexp

sumOps = (do {reservedOp lis "+"; return (Plus)})
         <|> (do {reservedOp lis "-"; return (Minus)})
         
mulOps = (do {reservedOp lis "*"; return (Times)})
         <|> (do {reservedOp lis "/"; return (Div)})
------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolexp' (do {reservedOp lis "&&"; return (And)}
                            <|> do {reservedOp lis "||"; return (Or)})

boolexp' :: Parser (Exp Bool)
boolexp' = do {reservedOp lis "!"; e <- boolexp; return (Not e)}
           <|> do {reserved lis "false"; return BFalse}
           <|> do {reserved lis "true"; return BTrue}
           <|> parens lis boolexp
           <|> (do {int1 <- intexp;
                    (do {reservedOp lis "=="; int2 <- intexp; return (Eq int1 int2)})
                    <|> (do {reservedOp lis "<"; int2 <- intexp; return (Lt int1 int2)})
                    <|> (do {reservedOp lis ">"; int2 <- intexp; return (Gt int1 int2)})
                    <|> (do {reservedOp lis "!="; int2 <- intexp; return (NEq int1 int2)})}
               )

-----------------------------------
--- Parser de comandos
-----------------------------------

-- chainl1 comm (do {reservedOp lis ";"; return (Seq)}) == Seq comm comm
comm :: Parser Comm
comm = chainl1 comm' (do {reservedOp lis ";"; return (Seq)}) 

comm' = try parse_skip <|> try parse_if <|> try parse_repeat <|> parse_identifier

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
