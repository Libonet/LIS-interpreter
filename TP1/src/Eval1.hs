module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple as T




import Data.Maybe -- NO SE SI DEJAR ESTO





-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = fromJust $ M.lookup v s

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
stepCommStar c    s = T.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = Skip :!: s
stepComm (Let v e) s = let n :!: s' = evalExp e s
                           s'' = update v n s'
                       in Skip :!: s''
stepComm (Seq Skip c) s = stepComm c s
stepComm (Seq c1 c2) s = let c' :!: s' = stepComm c1 s
                         in  stepComm (Seq c' c2) s'
stepComm (IfThenElse b c1 c2) s = let bool :!: s' = evalExp b s 
                                  in if bool 
                                     then stepComm c1 s'
                                     else stepComm c2 s'
stepComm (RepeatUntil c b) s = stepComm (Seq c (IfThenElse b Skip (RepeatUntil c b))) s

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = n :!: s
evalExp (Var x) s = lookfor x s :!: s
evalExp (UMinus e) s = let n :!: s' = evalExp e s
                       in -n :!: s'
evalExp (Plus e0 e1) s = let n0 :!: s' = evalExp e0 s
                             n1 :!: s'' = evalExp e1 s'
                         in n0 + n1 :!: s''
evalExp (Minus e0 e1) s = let n0 :!: s' = evalExp e0 s
                              n1 :!: s'' = evalExp e1 s'
                          in n0 - n1 :!: s''
evalExp (Times e0 e1) s = let n0 :!: s' = evalExp e0 s
                              n1 :!: s'' = evalExp e1 s'
                         in n0 * n1 :!: s''
evalExp (Div e0 e1) s = let n0 :!: s' = evalExp e0 s
                            n1 :!: s'' = evalExp e1 s'
                        in div n0 n1 :!: s''
evalExp (VarInc x) s = let n = lookfor x s + 1
                       in n :!: update x n s
evalExp (VarDec x) s = let n = lookfor x s - 1
                       in n :!:update x n s
evalExp BTrue s = True :!: s     
evalExp BFalse s = False :!: s    
evalExp (Lt e0 e1) s = let b0 :!: s' = evalExp e0 s
                           b1 :!: s'' = evalExp e1 s'
                       in b0 < b1 :!: s''
evalExp (Gt e0 e1) s = let b0 :!: s' = evalExp e0 s
                           b1 :!: s'' = evalExp e1 s'
                       in b0 > b1 :!: s''
evalExp (And p0 p1) s = let b0 :!: s' = evalExp p0 s
                            b1 :!: s'' = evalExp p1 s'
                       in b0 && b1 :!: s''
evalExp (Or p0 p1) s = let b0 :!: s' = evalExp p0 s
                           b1 :!: s'' = evalExp p1 s'
                       in (b0 || b1) :!: s''
evalExp (Not p) s = let b :!: s' = evalExp p s
                    in not b :!: s'
evalExp (Eq e0 e1) s = let b0 :!: s' = evalExp e0 s
                           b1 :!: s'' = evalExp e1 s'
                       in b0 == b1 :!: s''
evalExp (NEq e0 e1) s = let b0 :!: s' = evalExp e0 s
                            b1 :!: s'' = evalExp e1 s'
                       in b0 /= b1 :!: s''