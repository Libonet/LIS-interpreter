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
lookfor = M.lookup

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
stepComm Skip s = (Skip, s)
stepComm (Let v e) s = (Skip ,update v (evalExp i s) s)
stepComm (Seq Skip c) s = stepComm c s
stepComm (Seq c1 c2) s = let (c', s') = stepComm c1 s
                         in  stepComm (Seq c' c2) s
stepComm (IfThenElse b c1 c2) s = if evalExp b s 
                                  then stepComm c1 s
                                  else stepComm c2 s
stepComm (RepeatUntil c b)@r = if evalExp b s
                               then stepComm (Seq c r) s
                               else stepComm Skip s

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp = undefined
