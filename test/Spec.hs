import Test.Hspec

import Data.Set ((\\))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (forM_)  

import Lib

testPrinciplePair :: Spec
testPrinciplePair =
  describe "Principle pair searching" $ do
    forM_ tests $ \(test, ans) ->
      it ("test " ++ show test) $ do
        let r = principlePair test
        r `shouldBe` ans
 where
  tests = [
      ( -- x
        Var "x",
        Right (Env [("x",TVar "y0")], TVar "y0")
      )
    , ( -- f x
        Var "f" :@ Var "x",
        Right (Env [("f", TVar "y1" :-> TVar "init_type"), ("x", TVar "y1")], TVar "init_type"))
    , ( -- \x y. y
        Lam "x" $ Lam "y" $ Var "y",
        Right (Env [], TVar "x0" :-> (TVar "x2" :-> TVar "x2"))
      )
    , ( -- x x
        Var "x" :@ Var "x",
        Left "Can't unify (TVar \"x0\") with (TVar \"x0\" :-> TVar \"init_type\")!")
    , ( -- \x. y (x z) z
        Lam "x" $ (Var "y" :@ (Var "x" :@ Var "z")) :@ Var "z",
        Right (Env [("y",TVar "x3" :-> (TVar "y1" :-> TVar "x1")),("z",TVar "y1")],(TVar "y1" :-> TVar "x3") :-> TVar "x1")
      )
    , ( -- \f g. (\t. (\a r. a) t (g t t)) (f (\y. g y y))
        Lam "f" $ Lam "g" $ (Lam "t" $ (Lam "a" $ Lam "r" $ Var "a") :@ Var "t" :@ (Var "g" :@ Var "t" :@ Var "t")) :@ (Var "f" :@ (Lam "y" (Var "g" :@ Var "y" :@ Var "y"))),
        Right (Env [],((TVar "x6" :-> TVar "x12") :-> TVar "x6") :-> ((TVar "x6" :-> (TVar "x6" :-> TVar "x12")) :-> TVar "x6"))
      )
    ]

main :: IO ()
main = hspec $ do
  testPrinciplePair