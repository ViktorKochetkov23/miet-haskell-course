-- Не забудьте добавить тесты.

module Poly where

zipN :: [[a]] -> [[a]]
zipN xss
    | all null xss = []
    | otherwise = map head nonEmpty : zipN (map tail nonEmpty)
    where
        nonEmpty = filter (not . null) xss

-- Многочлены
-- a -- тип коэффициентов, список начинается со свободного члена.
-- Бонус: при решении следующих заданий подумайте, какие стали бы проще или
-- сложнее при обратном порядке коэффициентов (и добавьте комментарий).
newtype Poly a = P [a]

-- Задание 1 -----------------------------------------

-- Определите многочлен $x$.
x :: Num a => Poly a
x = P [0, 1]

-- Задание 2 -----------------------------------------

-- Функция, считающая значение многочлена в точке
applyPoly :: Num a => Poly a -> a -> a
applyPoly (P []) _ = 0;
applyPoly (P (coef: coefs)) x = coef + x * applyPoly (P coefs) x

-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.

-- Ответ: потому что первые список коэффициентов может заканчиваться нулями, т.е.
-- [1,2,3] и [1,2,3,0,0] - один и тот же полином
foldLastZeros :: (Num a, Eq a) => [a] -> [a]
foldLastZeros [] = []
foldLastZeros [x] = [x | x /= 0]
foldLastZeros (x: xs)
    | null residue = foldLastZeros [x]
    | otherwise = x: residue
    where residue = foldLastZeros xs

instance (Num a, Eq a) => Eq (Poly a) where
    (P x) == (P y) = foldLastZeros x == foldLastZeros y

-- Задание 4 -----------------------------------------

-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.)

-- Функция отображения переменной в степени
showPowerOfX :: Int -> String
showPowerOfX p
    | p == 0 = ""
    | p == 1 = "x"
    | otherwise = if p > 0 then "x^" ++ show p else "x^" ++ "(" ++ show p ++ ")"

-- Функция отображения одночлена
showMonomial :: (Num a, Ord a, Eq a, Show a) => (Int, a) -> String
showMonomial (p, coef)
    | coef == 0 = "0"
    | otherwise = (if coef > 0 then show coef else "(" ++ show coef ++ ")") ++ showPowerOfX p

-- Функция нумерования элементов списка
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- Функция отображения суммых списка слогаемых
showSumOf:: [String] -> String
showSumOf [] = ""
showSumOf [x] = x
showSumOf (x: xs) = x ++ " + " ++ showSumOf xs

instance (Num a, Eq a, Ord a, Show a) => Show (Poly a) where
    show (P xs) = showSumOf (map showMonomial (filter (\(p, coef) -> coef /= 0) (enumerate xs)))

-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P (map sum (zipN [xs, ys]))

-- Задание 6 -----------------------------------------

-- Определите умножение многочленов
times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = P [sum [a * b | (i, a) <- enumerate xs, 
                                        (j, b) <- enumerate ys, 
                                        i + j == k]
                            | k <- [0..(length xs + length ys - 2)]]
-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P (map negate xs)  
    fromInteger x = P [fromInteger x]
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------

-- Реализуйте nderiv через deriv
class Num a => Differentiable a where
    -- взятие производной
    deriv  :: a -> a
    -- взятие n-ной производной
    nderiv :: Int -> a -> a
    nderiv n x
        | n < 0 = error "Negative order derivative is not defined"
        | n == 0 = x
        | n > 0 = deriv (nderiv (n - 1) x)
-- Задание 9 -----------------------------------------

-- Определите экземпляр класса типов
instance Num a => Differentiable (Poly a) where
    deriv (P coefs)
        | null new_coefs = P [0]
        | otherwise = P new_coefs
        where new_coefs = drop 1 (map (\(order, coef) -> fromIntegral order * coef) (enumerate coefs))