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
        it "Var" $ do
            let state = extend empty "x" 5
            eval state (Var "x") `shouldBe` 5
            eval state (Var "y") `shouldBe` 0

        it "Val" $ do
            eval empty (Val 42) `shouldBe` 42
            eval empty (Val 0) `shouldBe` 0

        describe "binary operations" $ do
            let state = extend empty "a" 10
            let state' = extend state "b" 5

            it "Plus" $ do
                eval state' (Op (Var "a") Plus (Var "b")) `shouldBe` 15
                eval state' (Op (Val 3) Plus (Val 4)) `shouldBe` 7

            it "Minus" $ do
                eval state' (Op (Var "a") Minus (Var "b")) `shouldBe` 5
                eval state' (Op (Val 3) Minus (Val 4)) `shouldBe` (-1)

            it "Times" $ do
                eval state' (Op (Var "a") Times (Var "b")) `shouldBe` 50
                eval state' (Op (Val 3) Times (Val 4)) `shouldBe` 12

            it "Divide" $ do
                eval state' (Op (Var "a") Divide (Var "b")) `shouldBe` 2
                eval state' (Op (Val 10) Divide (Val 3)) `shouldBe` 3

            it "Gt (>)" $ do
                eval state' (Op (Var "a") Gt (Var "b")) `shouldBe` 1  -- true
                eval state' (Op (Var "b") Gt (Var "a")) `shouldBe` 0  -- false
                eval state' (Op (Val 5) Gt (Val 5)) `shouldBe` 0      -- false

            it "Ge (>=)" $ do
                eval state' (Op (Var "a") Ge (Var "b")) `shouldBe` 1  -- true
                eval state' (Op (Val 5) Ge (Val 5)) `shouldBe` 1      -- true
                eval state' (Op (Val 3) Ge (Val 5)) `shouldBe` 0      -- false

            it "Lt (<)" $ do
                eval state' (Op (Var "b") Lt (Var "a")) `shouldBe` 1  -- true
                eval state' (Op (Val 5) Lt (Val 3)) `shouldBe` 0      -- false

            it "Le (<=)" $ do
                eval state' (Op (Var "b") Le (Var "a")) `shouldBe` 1  -- true
                eval state' (Op (Val 5) Le (Val 5)) `shouldBe` 1      -- true
                eval state' (Op (Val 7) Le (Val 5)) `shouldBe` 0      -- false

            it "Eql (==)" $ do
                eval state' (Op (Var "a") Eql (Var "a")) `shouldBe` 1  -- true
                eval state' (Op (Var "a") Eql (Var "b")) `shouldBe` 0  -- false
                eval state' (Op (Val 5) Eql (Val 5)) `shouldBe` 1      -- true

        describe "desugar" $ do
            it "Assign" $ do
                desugar (Assign "x" (Val 5)) `shouldBe` DAssign "x" (Val 5)

            it "Incr" $ do
                desugar (Incr "x") `shouldBe` DAssign "x" (Op (Var "x") Plus (Val 1))

            it "If" $ do
                let stmt = If (Val 1) (Assign "x" (Val 1)) (Assign "x" (Val 0))
                let expected = DIf (Val 1) (DAssign "x" (Val 1)) (DAssign "x" (Val 0))
                desugar stmt `shouldBe` expected

            it "While" $ do
                let stmt = While (Val 1) (Incr "x")
                let expected = DWhile (Val 1) (DAssign "x" (Op (Var "x") Plus (Val 1)))
                desugar stmt `shouldBe` expected

            it "For" $ do
                let stmt = For (Assign "i" (Val 0))
                            (Op (Var "i") Lt (Val 10))
                            (Incr "i")
                            (Incr "x")
                let expectedInit = DAssign "i" (Val 0)
                let expectedBody = DSequence (DAssign "x" (Op (Var "x") Plus (Val 1)))
                                            (DAssign "i" (Op (Var "i") Plus (Val 1)))
                let expectedWhile = DWhile (Op (Var "i") Lt (Val 10)) expectedBody
                desugar stmt `shouldBe` DSequence expectedInit expectedWhile

            it "Block (empty)" $ do
                desugar (Block []) `shouldBe` DSequence DSkip DSkip

            it "Block (non-empty)" $ do
                let stmt = Block [Assign "x" (Val 1), Incr "x", Assign "y" (Val 2)]
                let expected = DSequence (DAssign "x" (Val 1))
                                (DSequence (DAssign "x" (Op (Var "x") Plus (Val 1)))
                                        (DSequence (DAssign "y" (Val 2))
                                                    (DSequence DSkip DSkip)))
                desugar stmt `shouldBe` expected

            it "Skip" $ do
                desugar Skip `shouldBe` DSkip

        describe "run" $ do
            describe "factorial" $ do
                it "0!" $ do
                    let state = extend empty "In" 0
                    run state factorial "Out" `shouldBe` 1

                it "1!" $ do
                    let state = extend empty "In" 1
                    run state factorial "Out" `shouldBe` 1

                it "5!" $ do
                    let state = extend empty "In" 5
                    run state factorial "Out" `shouldBe` 120

            describe "squareRoot" $ do
                it "sqrt(0)" $ do
                    let state = extend empty "In" 0
                    run state squareRoot "Out" `shouldBe` 0

                it "sqrt(1)" $ do
                    let state = extend empty "In" 1
                    run state squareRoot "Out" `shouldBe` 1

                it "sqrt(4)" $ do
                    let state = extend empty "In" 4
                    run state squareRoot "Out" `shouldBe` 2

                it "sqrt(9)" $ do
                    let state = extend empty "In" 9
                    run state squareRoot "Out" `shouldBe` 3

                it "sqrt(10)" $ do
                    let state = extend empty "In" 10
                    run state squareRoot "Out" `shouldBe` 3

            describe "fibonacci" $ do
                it "fib(0)" $ do
                    let state = extend empty "In" 0
                    run state fibonacci "Out" `shouldBe` 1

                it "fib(1)" $ do
                    let state = extend empty "In" 1
                    run state fibonacci "Out" `shouldBe` 1

                it "fib(2)" $ do
                    let state = extend empty "In" 2
                    run state fibonacci "Out" `shouldBe` 2

                it "fib(3)" $ do
                    let state = extend empty "In" 3
                    run state fibonacci "Out" `shouldBe` 3

                it "fib(5)" $ do
                    let state = extend empty "In" 5
                    run state fibonacci "Out" `shouldBe` 8

                it "fib(7)" $ do
                    let state = extend empty "In" 7
                    run state fibonacci "Out" `shouldBe` 21

        describe "runSimpler" $ do
            it "DAssign" $ do
                let state = empty
                runSimpler state (DAssign "x" (Val 5)) "x" `shouldBe` 5

            it "DIf (true)" $ do
                let state = empty
                let stmt = DIf (Val 1) (DAssign "x" (Val 10)) (DAssign "x" (Val 20))
                runSimpler state stmt "x" `shouldBe` 10

            it "DIf (false)" $ do
                let state = empty
                let stmt = DIf (Val 0) (DAssign "x" (Val 10)) (DAssign "x" (Val 20))
                runSimpler state stmt "x" `shouldBe` 20

            it "DSequence" $ do
                let state = empty
                let stmt = DSequence (DAssign "x" (Val 5)) (DAssign "y" (Op (Var "x") Plus (Val 3)))
                let finalState = runSimpler state stmt
                finalState "x" `shouldBe` 5
                finalState "y" `shouldBe` 8

            it "DWhile (executes 3 times)" $ do
                let state = extend empty "counter" 0
                let body = DAssign "counter" (Op (Var "counter") Plus (Val 1))
                let while = DWhile (Op (Var "counter") Lt (Val 3)) body
                runSimpler state while "counter" `shouldBe` 3

            it "DSkip" $ do
                let state = extend empty "x" 42
                runSimpler state DSkip "x" `shouldBe` 42