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
    True),
   (expr (lam "y" (tv "a") (var "y")) (tv "a"),
    False),
   (expr (lam "y" (ar (tv "a") (tv "b")) (var "y")) (ar (tv "c") (tv "b")),
    False),
   (expr
    (lam "x" (tv "a") (ap (lam "y" (tv "a") (var "y")) (var "x") (tv "a"))) (ar (tv "a") (tv "a")), True),
   (expr
    (lam "x" (tv "a") (ap (lam "y" (tv "b") (var "y")) (var "x") (tv "a"))) (ar (tv "a") (tv "a")), False),
    (expr
     (tlam "a" (lam "x" (tv "a") (var "x"))) (pit "a" (ar (tv "a") (tv "a"))),
     True),
    (expr
     (tlam "a" (lam "x" (tv "a") (var "x"))) (pit "b" (ar (tv "a") (tv "a"))),
     False),
    (expr
     (tlam "a" (tap (tlam "b" (lam "y" (tv "b") (var "y"))) (tv "a")))
     (pit "a" (ar (tv "a") (tv "a"))),
     True)]
