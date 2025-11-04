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
        let nel1 = NEL 1 [2, 3]
        let nel2 = NEL 2 [4, 5]
        it "distance" $ do
            distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) `shouldBe` sqrt 2.0
            distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) `shouldBe` 1.0
        it "intersect" $ do
            intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] `shouldSatisfy` (\x -> x == [2, 4] || x == [4, 2])
            intersect [1, 2, 4, 6] [3, 5, 7]       `shouldBe` []
        it "zipN" $ do
            zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
            zipN [[1, 2, 3], [4, 5], [6]]          `shouldBe` [[1, 4, 6], [2, 5], [3]]
        it "find" $ do
            find (> 0) [-1, 2, -3, 4] `shouldBe` Just 2
            find (> 0) [-1, -2, -3]   `shouldBe` Nothing
        it "findWithFilter" $ do
            findWithFilter (> 0) [-1, 2, -3, 4] `shouldBe` Just 2
            findWithFilter (> 0) [-1, -2, -3]   `shouldBe` Nothing
        it "findLast" $ do
            findLast (> 0) [-1, 2, -3, 4] `shouldBe` Just 4
        it "mapFuncs" $ do
            mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 `shouldBe` [9, 4, 0]
        it "satisfiesAll" $ do
            satisfiesAll [even, \x -> x `rem` 5 == 0] 10 `shouldBe` True
            satisfiesAll [] 4                          `shouldBe` True
            satisfiesAll [(> 2), (< 4)] 5              `shouldBe` False
        it "tailNel" $ do
            tailNel nel1 `shouldBe` [2, 3]
            tailNel nel2 `shouldBe` [4, 5]
        it "lastNel" $ do
            lastNel nel1 `shouldBe` 3
            lastNel nel2 `shouldBe` 5
        it "zipNel" $ do
            zipNel nel1 nel2 `shouldBe` NEL (1, 2) [(2, 4), (3, 5)]
        it "listToNel" $ do
            listToNel [1,2,3] `shouldBe` nel1
        it "nelToList" $ do
            nelToList nel1 `shouldBe` [1,2,3]
    describe "luhn" $ it "" pending
