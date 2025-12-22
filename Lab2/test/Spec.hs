import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        let poly1 = P [1,2,3]
        let poly2 = P [1,2,-3]
        it "applyPoly" $ do
            applyPoly poly1 1 `shouldBe` 6
            applyPoly poly1 2 `shouldBe` 17
            applyPoly poly1 0 `shouldBe` 1
            applyPoly poly1 (-1) `shouldBe` 2
        it "eqPoly" $ do
            poly1 == (P [1,2,3]) `shouldBe` True
            poly1 == (P [1,2,3,0,0]) `shouldBe` True
            poly1 == (P [1,2,3,0,1]) `shouldBe` False
        it "showPoly" $ do
            show poly1 `shouldBe` "1 + 2x + 3x^2"
            show poly2 `shouldBe` "1 + 2x + (-3)x^2"
        it "opsPoly" $ do
            poly1 + poly2 `shouldBe` P [2,4]
            poly1 + poly2 `shouldBe` poly2 + poly1
            poly1 - poly2 `shouldBe` P [0,0,6]
            poly1 * poly2 `shouldBe` P [1,4,4,0,-9]
            poly1 * poly2 `shouldBe` poly2 * poly1
            -poly1 `shouldBe` P [-1,-2,-3]
        it "differPoly" $ do
            deriv poly1 `shouldBe` P [2, 6]
            nderiv 2 poly1 `shouldBe` P [6]
            nderiv 3 poly1 `shouldBe` P [0]
            --законы проивзодной суммы и произведения
            deriv (poly1 + poly2) `shouldBe` (deriv poly1) + (deriv poly2)
            deriv (poly1 * poly2) `shouldBe` (deriv poly1) * poly2 + (deriv poly2) * poly1
    describe "simpleLang" $ do
        -- включите тесты на работу 
        it "desugar" $ pending
