module Eval2
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
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case  M.lookup v s of 
              Just n -> Right n
              Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

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
stepComm Skip s = Right (Skip :!: s)
stepComm (Let v e) s = case evalExp e s of
                       Right (n :!: s') -> let s'' = update v n s'
                                           in Right (Skip :!: s'')
                       Left error -> Left error
stepComm (Seq Skip c) s = stepComm c s
stepComm (Seq c1 c2) s = case stepComm c1 s of
                         Right (c' :!: s') -> stepComm (Seq c' c2) s'
                         error-> error
stepComm (IfThenElse b c1 c2) s = case evalExp b s of
                                  Right (bool :!: s') -> if bool
                                                         then stepComm c1 s
                                                         else stepComm c2 s
                                  Left error -> Left error
stepComm (RepeatUntil c b) s = stepComm (Seq c (IfThenElse b Skip (RepeatUntil c b))) s


-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var x) s = case lookfor x s of
                    Left error -> Left error
                    Right n  -> Right (n :!: s)
evalExp (UMinus e) s = case evalExp e s of 
                       Right (n :!: s') ->  Right (-n :!: s')
                       Left error -> Left error
evalExp (Plus e0 e1) s = case evalExp e0 s of
                         Right (n0 :!: s') ->  case evalExp e1 s' of
                                               Right (n1 :!: s'') -> Right (n0 + n1 :!: s'')
                                               Left error -> Left error
                         Left error -> Left error
evalExp (Minus e0 e1) s = case evalExp e0 s of
                          Right (n0 :!: s') -> 
                           case evalExp e1 s' of
                           Right (n1 :!: s'') -> Right (n0 - n1 :!: s'')
                           Left error -> Left error
                          Left error -> Left error
evalExp (Times e0 e1) s = case evalExp e0 s of
                          Right (n0 :!: s') -> 
                           case evalExp e1 s' of
                           Right (n1 :!: s'') -> Right (n0 * n1 :!: s'')
                           Left error -> Left error
                          Left error -> Left error
evalExp (Div e0 e1) s = case evalExp e0 s of
                        Right (n0 :!: s') -> 
                         case evalExp e1 s' of
                         Right (n1 :!: s'') -> if n1 == 0 
                                               then Left DivByZero
                                               else Right ((div n0 n1) :!: s'')
                         Left error -> Left error
                        Left error -> Left error
evalExp (VarInc x) s = case lookfor x s of
                       Left error -> Left error
                       Right n  -> Right (n+1 :!: update x (n+1) s)
evalExp (VarDec x) s = case lookfor x s of
                       Left error -> Left error
                       Right n  -> Right (n-1 :!: update x (n-1) s)
evalExp BTrue s = Right (True :!: s)     
evalExp BFalse s = Right (False :!: s)    
evalExp (Lt e0 e1) s = case evalExp e0 s of
                       Right (b0 :!: s') -> 
                        case evalExp e1 s' of
                        Right (b1 :!: s'') -> Right ((b0 < b1) :!: s'')
                        Left error -> Left error
                       Left error -> Left error
evalExp (Gt e0 e1) s = case evalExp e0 s of
                       Right (b0 :!: s') -> 
                        case evalExp e1 s' of
                        Right (b1 :!: s'') -> Right ((b0 > b1) :!: s'')
                        Left error -> Left error
                       Left error -> Left error
evalExp (And p0 p1) s = case evalExp p0 s of
                        Right (b0 :!: s') -> 
                         case evalExp p1 s' of
                         Right (b1 :!: s'') -> Right ((b0 && b1) :!: s'')
                         Left error -> Left error
                        Left error -> Left error
evalExp (Or p0 p1) s = case evalExp p0 s of
                       Right (b0 :!: s') -> 
                        case evalExp p1 s' of
                        Right (b1 :!: s'') -> Right ((b0 || b1) :!: s'')
                        Left error -> Left error
                       Left error -> Left error
evalExp (Not p) s = case evalExp p s of 
                    Right (b :!: s') -> Right (not b :!: s')
                    Left error -> Left error 
evalExp (Eq e0 e1) s = case evalExp e0 s of
                       Right (b0 :!: s') -> 
                        case evalExp e1 s' of
                        Right (b1 :!: s'') -> Right ((b0 == b1) :!: s'')
                        Left error -> Left error
                       Left error -> Left error
evalExp (NEq e0 e1) s = case evalExp e0 s of
                        Right (b0 :!: s') -> 
                         case evalExp e1 s' of
                         Right (b1 :!: s'') -> Right ((b0 /= b1) :!: s'')
                         Left error -> Left error
                        Left error -> Left error