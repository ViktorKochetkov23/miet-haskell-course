{-# LANGUAGE ScopedTypeVariables #-}
import FunctorsMonads
import Streams hiding (main)
import Test.Hspec
-- Раскомментируйте QuickCheck или Hegdehog, в зависимости от того, что будете использовать
-- Документация https://hspec.github.io/quickcheck.html
import Test.Hspec.QuickCheck
-- Документация в https://github.com/parsonsmatt/hspec-hedgehog#readme
-- import Test.Hspec.Hedgehog

-- Добавьте минимум 5 тестов свойств для функций из первых 2 лабораторных (скопируйте определения тестируемых функций сюда).

main :: IO ()
main = hspec $ do
    describe "functors and monads" $ do
        let list_ctor = flip (:) []
        describe "liftA2' tests" $ do
            describe "with Maybe" $ do
                prop "plus" $ do
                    \(x::Int) (y::Int) -> liftA2' (+) (Just x) (Just y) `shouldBe` Just (x + y)
                prop "multi" $ do
                    \(x::Int) (y::Int) -> liftA2' (*) (Just x) (Just y) `shouldBe` Just (x*y)
                it "Nothing first" $ do
                    liftA2' (+) Nothing (Just 1) `shouldBe` Nothing
                it "Nothing second" $ do
                    liftA2' (+) (Just 1) Nothing `shouldBe` Nothing
            describe "with []" $ do
                prop "plus" $ do
                    \(x::Int) (y::Int) -> liftA2' (+) [x, y] [x, y] `shouldBe` [2*x, x+y, x+y, 2*y]
                it "[] first" $ do
                    liftA2' (+) [] [1,2,3] `shouldBe` []
                it "[] second" $ do
                    liftA2' (+) [1,2,3] [] `shouldBe` []
        describe "seqA tests" $ do
            it "with Maybe" $ do
                seqA [Just 1, Just 2] `shouldBe` Just [1, 2]
                seqA [Just 1, Just 2, Nothing] `shouldBe` Nothing
            it "with []" $ do
                seqA [[1,2], [1,2]] `shouldBe` [[1,1], [1,2], [2,1], [2,2]]
                seqA [[1,2], [1,2,3]] `shouldBe` [[1,1], [1,2], [1,3], [2,1], [2,2], [2,3]]
        describe "traverseA tests" $ do
            it "with Maybe" $ do
                traverseA Just [1, 2] `shouldBe` Just [1, 2]
                traverseA (\a -> if a > 2 then Just a else Nothing) [1, 3] `shouldBe` Nothing
            it "with []" $ do
                traverseA list_ctor [1, 2] `shouldBe` [[1, 2]]
                traverseA (\a -> if a > 2 then [a] else []) [1, 3] `shouldBe` []
        describe "filterA tests" $ do
            it "with Maybe" $ do
                filterA (\a -> if a > 10 then Nothing else Just (a > 0)) [-1, -2, 1, 2] `shouldBe` Just [1, 2]
                filterA (\a -> if a < 0 then Nothing else Just (a > 1)) [-1, -2, 1, 2] `shouldBe` Nothing
            it "with []" $ do
                filterA (\a -> if a > 1 then [True] else [a < -1]) [-1, -2, 1, 2] `shouldBe` [[-2, 2]]
        describe "composeM tests" $ do
            prop "with Maybe" $ do
                \(x::Int) -> (composeM Just Just) x `shouldBe` Just x
            prop "with []" $ do
                \(x::Int) -> (composeM list_ctor list_ctor x) `shouldBe` [x]
        describe "Functor laws" $ do
            describe "with Either" $ do
                prop "identity Left" $ do
                    \x -> id <$$> Left x `shouldBe` (id Left x :: Either Int Int)
                prop "identity Right" $ do
                    \x -> id <$$> Right x `shouldBe` (id Right x :: Either Int Int)
                prop "composition Left" $ do
                    \x -> ((+) 1 . (+) 2) <$$> Left x `shouldBe` (((+) 1) <$$> (((+) 2) <$$> Left x) :: Either Int Int)
                prop "composition Right" $ do
                    \x -> ((+) 1 . (+) 2) <$$> Right x `shouldBe` (((+) 1) <$$> (((+) 2) <$$> Right x) :: Either Int Int)
            describe "with (->) t" $ do
                let incr x = x + 1
                prop "identity" $ do
                    \(x::Int) -> (id <$$> incr) x `shouldBe` id incr x
                prop "composition" $ do
                    \x -> (((+) 1 . (+) 2) <$$> incr) x `shouldBe` ((((+) 1) <$$> ((+) 2) <$$> incr) x :: Int)
        describe "Applicative laws" $ do
            describe "with Either" $ do
                prop "identity Left" $ do
                    \x -> (Right id) <**> Left x `shouldBe` (Left x :: Either Int Int)
                prop "identity Right" $ do
                    \x -> (Right id) <**> Right x `shouldBe` (Right x :: Either Int Int)
                prop "homomorphism" $ do
                    let multi2 = (*2)
                    \x -> Right multi2 <**> Right x `shouldBe` (Right (multi2 x) :: Either Int Int)
                prop "interchange" $ do
                    let add2 = (+2)
                    \x -> let u = Right add2 in u <**> Right x `shouldBe` (Right ($ x) <**> u :: Either Int Int)
            describe "with (->) t" $ do
                prop "identity" $ do
                    let incr = (+1)
                    \(x::Int) -> ((pure' id) <**> incr) x `shouldBe` incr x
                prop "homomorphism" $ do
                    let multi2 = (*2)
                    \(x::Int) (y::Int) -> ((pure' multi2) <**> (pure' x)) y `shouldBe` (pure' (multi2 x)) y
                prop "interchange" $ do
                    let add2 = (+2)
                    \(x::Int) (y::Int)-> let u = pure' add2 in (u <**> pure' x) y `shouldBe` (pure' ($ x) <**> u) y
    describe "streams" $ do
        it "streamToList & sTake" $ do
            sTake 10000 ruler `shouldBe` take 10000 (streamToList ruler)
        prop "sRepeat" $ do
            \(x::Int) -> sTake 100 (sRepeat x) `shouldBe` replicate 100 x
        prop "sCycle" $ do
            \(xs::[Int]) -> sTake (2 * length xs) (sCycle xs) `shouldBe` xs ++ xs
        prop "sIterate" $ do
            \(x::Int) -> sTake 100 (sIterate (+1) x) `shouldBe` take 100 (iterate (+1) x)
        it "sInterleave" $ do
            let zeros = sRepeat 0
            let checkInterleave s1 (x :> s2) = x : sTake 100 (sInterleave s1 s2) `shouldBe` sTake 101 (sInterleave (x:>s2) s1)
            checkInterleave zeros nats
            checkInterleave zeros ruler
            checkInterleave nats ruler
        describe "minMax functions" $ do
            prop "minMaxSlow & minMax" $ do
                \(xs::[Int]) -> minMaxSlow xs `shouldBe` minMax xs
            prop "minMax & minMaxBang" $ do
                \(xs::[Int]) -> minMax xs `shouldBe` minMaxBang xs