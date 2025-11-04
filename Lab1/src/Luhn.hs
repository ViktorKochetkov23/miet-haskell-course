module Luhn where

-- Проверка корректности номера банковской карты алгоритмом Луна https://ru.wikipedia.org/wiki/Алгоритм_Луна.
-- Алгоритм:
-- 1. Все цифры, стоящие на чётных местах (считая с конца), удваиваются. Если при этом получается число, большее 9, то из него вычитается 9. Цифры, стояшие на нечётных местах, не изменяются.
-- То есть: последняя цифра не меняется; предпоследнее удваивается; 3-е с конца (предпредпоследнее) не меняется; 4-е с конца удваивается и т.д.
-- 2. Все полученные числа складываются.
-- 3. Если полученная сумма кратна 10, то исходный список корректен.

-- Не пытайтесь собрать всё в одну функцию, используйте вспомогательные.
-- Например: разбить число на цифры (возможно, сразу в обратном порядке).
-- Не забудьте добавить тесты, в том числе для вспомогательных функций!
isLuhnValid :: Int -> Bool
isLuhnValid cardNumber = sum (doDoublEven (digitsReverseList cardNumber)) `mod` 10 == 0

digitsReverseList :: Int -> [Int]
digitsReverseList 0 = []
digitsReverseList n = n `mod` 10 : digitsReverseList (n `div` 10)

doubleEven :: Bool -> Int -> Int
doubleEven False dig = dig
doubleEven True dig = let z = dig * 2 in (if z > 9 then z - 9 else z)

mapMany :: [a -> b] -> [a] -> [b] 
mapMany [] [] = []
mapMany fs xs
    | length fs /= length xs = error "Functors and args amounts are different"
    | otherwise = head fs (head xs) : mapMany (tail fs) (tail xs)

generateDoubleEvens :: Int -> Bool -> [Int -> Int]
generateDoubleEvens 1 _even = [doubleEven _even]
generateDoubleEvens n _even
    | n > 1 = doubleEven _even : generateDoubleEvens (n - 1) (not _even)
    | otherwise = error "Amount of functions cant be less than 1"

doDoublEven :: [Int] -> [Int]
doDoublEven xs = mapMany (generateDoubleEvens (length xs) True) xs
