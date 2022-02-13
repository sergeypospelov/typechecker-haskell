import Test.Hspec

import Lib

testPrinciplePair :: Spec
testModel2Spec =
  describe "Principle pair searching" $ do
    forM_ fs $ \f ->
     it ("sat in w1: " ++ show f) $ do
       f `shouldSatisfy` (sat testModel2 1)
         where fs = [EX $ Atom "phi",
                     EG $ Atom "psi",
                     AF $ Atom "psi",
                     AU (Atom "psi" `Or` Atom "phi") (Atom "psi"),
                     AX $ AU (Atom "phi") (Atom "psi")]

main :: IO ()
main = hspec $ do
  testPrinciplePair