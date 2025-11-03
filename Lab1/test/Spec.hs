import FirstSteps
import Lists
import Luhn
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
        -- Можно вложить глубже: describe "xor" do $ ... чтобы дать названия отдельным тестам
        it "xor" $ do
            xor True True `shouldBe` False
            xor True False `shouldBe` True
            xor False True `shouldBe` True
            xor False False `shouldBe` False
        it "max3" $ do
            max3 1 3 2 `shouldBe` 3
            max3 5 2 5 `shouldBe` 5
        it "min3" $ do
            min3 1 3 2 `shouldBe` 1
            min3 5 2 5 `shouldBe` 2
        it "median3" $ do
            median3 1 3 2 `shouldBe` 2
            median3 5 2 5 `shouldBe` 5
        it "min3d" $ do
            min3d 1.0 3.0 2.0 `shouldBe` 1.0
            min3d 5.0 2.0 5.0 `shouldBe` 2.0
        it "rbgToCmyk" $ do
            rbgToCmyk (RGB 255 255 255) `shouldBe` CMYK 0 0 0 0
            rbgToCmyk (RGB 255 0 0)     `shouldBe` CMYK 0 1 1 0
            rbgToCmyk (RGB 0 255 0)     `shouldBe` CMYK 1 0 1 0
            rbgToCmyk (RGB 0 0 255)     `shouldBe` CMYK 1 1 0 0
        it "geomProgression" $ do
            geomProgression 1.0 2.0 0  `shouldBe` 1.0
            geomProgression 1.0 2.0 10 `shouldBe` 1024.0
            geomProgression 2.0 3.0 4  `shouldBe` 162.0
        it "coprime" $ do
            coprime 10 15 `shouldBe` False
            coprime 12 35 `shouldBe` True
    describe "lists" $ do
        it "distance" pending
        it "intersect" pending
        it "zipN" pending
        it "find" pending
        it "findLast" pending
        it "mapFuncs" pending
        it "tailNel" pending
        it "lastNel" pending
        it "zipNel" pending
        it "listToNel" pending
        it "nelToList" pending
    describe "luhn" $ it "" pending
