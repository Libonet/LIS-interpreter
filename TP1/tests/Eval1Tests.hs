module Eval1Tests where

import Test.HUnit

import AST
import Parser
import Eval1
import qualified Data.Map.Strict               as M

-- | Tests cases templates

okTest :: FilePath -> String -> String -> Eval1.State -> Test
okTest filePath cont msg expected =
  TestCase $ assertEqual msg expected (eval parsedProg)
    where
      parsedProg = case parseComm filePath cont of
        Right ret -> ret
        Left _ -> Skip

-- | Tests cases definition

testSkip :: FilePath -> String -> Test
testSkip filePath cont = okTest filePath cont "Error on skip" M.empty

testSqrt :: FilePath -> String -> Test
testSqrt filePath cont = okTest filePath cont msg prog
  where
    msg = "Error on eval for sqrt program"
    prog = M.fromList [("i",5),("n",25),("t",25)]

testRepeat2 :: FilePath -> String -> Test
testRepeat2 filePath cont = okTest filePath cont msg prog
  where
    msg = "Error on eval for repeat2 program"
    prog = M.fromList [("x",5)]

testIf :: FilePath -> String -> Test
testIf filePath cont = okTest filePath cont msg prog
  where
    msg = "Error on eval for if program"
    prog = M.fromList [("x",4),("y",2)]

testMinusYDiv :: FilePath -> String -> Test
testMinusYDiv filePath cont = okTest filePath cont msg prog
  where
    msg = "Error on eval for MinusYDiv program"
    prog = M.fromList [("x",9),("y",9),("z",5)]

-- | Tests cases

tests :: [(FilePath -> String -> Test, FilePath)]
tests =
    [
      (testSkip, "ejemplos/skip.lis")
    , (testSqrt, "ejemplos/sqrt.lis")
    , (testIf, "ejemplos/if.lis")
    , (testRepeat2, "ejemplos/repeat2.lis")
    , (testMinusYDiv, "ejemplos/MinusYDiv.lis")
    ]

-- | Run tests

eval1Tests :: IO Counts
eval1Tests = do
    -- Add tests
    contents <- mapM (readFile . snd) tests
    runTestTT $ TestList (apply tests contents)

apply :: [(a -> b -> c, a)] -> [b] -> [c]
apply [] _ = []
apply ((f, x): ts) (y:ys) = f x y : apply ts ys
apply _ _ = error "Unexpected error for apply"
