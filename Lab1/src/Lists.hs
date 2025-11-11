module Lists where
import Data.Maybe (isNothing)

-- вектор задаётся списком координат
newtype Point = Point [Double] deriving (Eq, Show, Read)

-- distance x y находит расстояние между двумя точками в n-мерном
-- пространстве. Если число координат точек разное, сообщите об ошибке.
-- distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) == sqrt 2.0
-- distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) == 1.0

-- используйте рекурсию и сопоставление с образцом
sqDistance, distance :: Point -> Point -> Double
-- disntance (Point []) (Point []) = 0
sqDistance (Point []) (Point []) = 0
sqDistance (Point (x1: xs1)) (Point (x2: xs2)) = (x1 - x2) ^ 2 + sqDistance (Point xs1) (Point xs2)
sqDistance _ _ = error "Dimensions are not equal"

distance p1 p2 = sqrt (sqDistance p1 p2)
-- intersect xs ys возвращает список, содержащий общие элементы двух списков.
-- intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4] (или [4, 2]!)
-- intersect [1, 2, 4, 6] [3, 5, 7] == []

-- используйте рекурсию и сопоставление с образцом
contains:: Integer -> [Integer] -> Bool
contains _ [] = False
contains x (a:as) 
  | x == a    = True
  | otherwise = contains x as

intersect :: [Integer] -> [Integer] -> [Integer]
intersect [] _ = []
intersect _ []  = []
intersect xs (y:ys)
  | contains y xs = y : intersect xs ys
  | otherwise = intersect xs ys


-- zipN принимает список списков и возвращает список, который состоит из
-- списка их первых элементов, списка их вторых элементов, и так далее.
-- zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
-- zipN [[1, 2, 3], [4, 5], [6]] == [[1, 4, 6], [2, 5], [3]]
zipN :: [[a]] -> [[a]]
zipN xss
    | all null xss = []
    | otherwise = map head nonEmpty : zipN (map tail nonEmpty)
    where
        nonEmpty = filter (not . null) xss

-- Нижеперечисленные функции можно реализовать или рекурсивно, или с помощью
-- стандартных функций для работы со списками (map, filter и т.д.)
-- Попробуйте оба подхода! Хотя бы одну функцию реализуйте обоими способами.

-- Если в списке xs есть такие элементы x, для которых f x == True, то
-- find f xs возвращает Just (первый x), а findLast f xs -- Just (последний x).
-- Если таких нет, то обе функции возвращают Nothing
-- find (> 0) [-1, 2, -3, 4] == Just 2
-- findLast (> 0) [-1, 2, -3, 4] == Just 4
-- find (> 0) [-1, -2, -3] == Nothing
find, findLast, findWithFilter :: (a -> Bool) -> [a] -> Maybe a

find f [] = Nothing
find f (x: xs)
    | f x = Just x
    | otherwise = find f xs

-- То же самое, но с filter
findWithFilter f xs
    | null filteredXs = Nothing
    | otherwise = Just (head filteredXs)
    where filteredXs = filter f xs

findLast f [] = Nothing
findLast f [x]
    | f x = Just x
    | otherwise = Nothing
findLast f (x: xs)
    | isNothing residue = findLast f [x]
    | otherwise = residue
    where residue = findLast f xs

-- mapFuncs принимает список функций fs и возвращает список результатов 
-- применения всех функций из fs к x.
-- mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]
mapFuncs :: [a -> b] -> a -> [b]

mapFuncs fs x = map (\f -> f x) fs

-- satisfiesAll принимает список предикатов (функций, возвращающих Bool) preds
-- и возвращает True, если все они выполняются (т.е. возвращают True) для x.
-- Полезные стандартные функции: and, all.
-- satisfiesAll [even, \x -> x rem 5 == 0] 10 == True
-- satisfiesAll [] 4 == True (кстати, почему?)
satisfiesAll :: [a -> Bool] -> a -> Bool

satisfiesAll [] x = True
satisfiesAll (f: fs) x
    | f x = satisfiesAll fs x
    | otherwise = False

-- Непустой список состоит из первого элемента (головы)
-- и обычного списка остальных элементов
-- Например, NEL 1 [2, 3] соотвествует списку [1, 2, 3], а NEL 1 [] -- списку [1].
data NEL a = NEL a [a] deriving (Eq, Show, Read)

-- Запишите правильный тип (т.е. такой, чтобы функция имела результат для любых аргументов
-- без вызовов error) и реализуйте функции на NEL, аналогичные tail, last и zip
tailNel :: NEL a -> [a]
tailNel (NEL x xs)
    | null xs = [x]
    | otherwise = xs

lastNel :: NEL a -> a
lastNel (NEL x xs)
    | null xs = x
    | otherwise = last xs

zipNel :: NEL a -> NEL b -> NEL (a, b)
zipNel (NEL x xs) (NEL y ys) = NEL (x, y) (zip xs ys)

listToNel :: [a] -> Maybe (NEL a)
listToNel [] = Nothing
listToNel (x: xs) = Just (NEL x xs)

nelToList :: NEL a -> [a]
nelToList (NEL x xs) = x: xs
