module L2Tests() where

import L2
import TestUtils

allL2Tests = do
  testTypeChecking

testTypeChecking =
  testFunction typeCheck typeCheckingCases

typeCheckingCases =
  [(expr (var "a") (ar (tv "a") (tv "b")),
    False),
   (expr (lam "y" (tv "a") (var "y")) (ar (tv "a") (tv "a")),
    True)]