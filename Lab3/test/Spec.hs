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
        describe "liftA2' tests" $ do
            describe "with Maybe" $ do
                prop "plus" $ do
                    (\x y -> liftA2' (+) (Just x) (Just y) `shouldBe` (Just (x + y) :: Maybe Int))
                prop "multi" $ do
                    (\x y -> liftA2' (*) (Just x) (Just y) `shouldBe` (Just (x*y) :: Maybe Int))
                it "Nothing first" $ do
                    liftA2' (+) Nothing (Just 1) `shouldBe` Nothing
                it "Nothing second" $ do
                    liftA2' (+) (Just 1) Nothing `shouldBe` Nothing
    describe "streams" $ do
        it "" $ pending
