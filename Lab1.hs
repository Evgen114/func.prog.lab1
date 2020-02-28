-- В сдаваемой версии не должно быть предупреждений
-- (в исходной они есть, поскольку аргументы функций не используются).
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

-- Для первых упражнений есть тесты в Lab1_Test.hs. Добавьте свои!

-- Не забывайте, что можно добавлять вспомогательные
-- функции и переменные. Старайтесь, чтобы код был
-- читаемым. Рекомендации по оформлению кода:
-- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
-- Как компилятор понимает отступы: https://en.wikibooks.org/wiki/Haskell/Indentation

-- Одно из заданий вынесено в файл Luhn.hs для отделения его вспомогательных
-- функций. Рекомендуемая очерёдность выполнения: после distance или после intersect.
module Lab1 where

-- xor x y находит "исключающее или" x и y
-- xor True False == True
-- xor True True == False

-- используйте сопоставление с образцом
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- max3 x y z находит максимум из x, y и z
-- max3 1 3 2 == 3
-- max3 5 2 5 == 5
-- median3 x y z находит второе по величине число (медиану)
-- median3 1 3 2 == 2
-- median3 5 2 5 == 5
max3, median3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = max (max x y) z

median3 x y z = min (max (min x y) z) (max x y)

-- Типы данных, описывающие цвета в моделях
-- RGB (https://ru.wikipedia.org/wiki/RGB), компоненты от 0 до 255
-- и CMYK (https://ru.wikipedia.org/wiki/CMYK), компоненты от 0.0 до 1.0
data RGB = RGB { red :: Int, green :: Int, blue :: Int } deriving (Eq, Show, Read)
data CMYK = CMYK { cyan :: Double, magenta :: Double, yellow :: Double, black :: Double } deriving (Eq, Show, Read)
-- Задайте функцию для их преобразования
-- (формулы из http://www.codeproject.com/Articles/4488/XCmyk-CMYK-to-RGB-Calculator-with-source-code):
-- Black   = min(1-Red, 1-Green, 1-Blue)
-- Cyan    = (1-Red-Black) / (1-Black)
-- Magenta = (1-Green-Black) / (1-Black)
-- Yellow  = (1-Blue-Black) / (1-Black)
-- где значения Red, Green и Blue нормализованы от 0 до 1).

-- Заметьте, что (/) для Int не работает, и неявного преобразования Int в Double нет.
-- Это преобразование производится с помощью функции fromIntegral.

min3 :: Ord a => a -> a -> a -> a
min3 x y z = min (min x y) z

rgbToCmyk :: RGB -> CMYK
rgbToCmyk (RGB red green blue) = let r = fromIntegral red / 255.0
                                     g = fromIntegral green / 255.0
                                     b = fromIntegral blue / 255.0
                                     k = (min3 (1-r) (1-g) (1-b))
                                     c = (1-r-k) / (1-k)
                                     m = (1-g-k) / (1-k)
                                     y = (1-b-k) / (1-k)
                                     in CMYK c m y k

class Approx_eq a where
    (~=) :: a -> a -> Bool

instance Approx_eq Double where
    (~=) x y = abs (x - y) <= 0.0001

instance Approx_eq CMYK where
    (~=) (CMYK c m y b) (CMYK c' m' y' b') = all (\p -> fst p ~= snd p) [(c, c'), (m, m'), (b, b'), (y,y'), (b,b')]

-- geomProgression b q n находит n-й (считая с 0) член
-- геометрической прогрессии, нулевой член которой -- b,
-- а знаменатель -- q.
-- geomProgression 3.0 2.0 2 == 12.0

-- используйте рекурсию
-- не забудьте случаи n < 0 и n == 0.
geomProgression :: Double -> Double -> Integer -> Double
geomProgression b _ 0 = b
geomProgression b q n = if n > 0 then geomProgression (b * q) q (n - 1) else error "Negative n"

-- coprime a b определяет, являются ли a и b взаимно простыми
-- (определение: Целые числа называются взаимно простыми,
-- если они не имеют никаких общих делителей, кроме +/-1)
-- coprime 10 15 == False
-- coprime 12 35 == True

-- используйте рекурсию
-- есть ли важные пограничные случаи?

-- полезные функции в Prelude (автоматически загруженной
-- части стандартной библиотеки): quot, rem, quotRem
-- (или div, mod, divMod в зависимости от того, как
-- обрабатываете отрицательные числа)
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html
coprime :: Integer -> Integer -> Bool
coprime a b = my_gcd (abs a) (abs b) == 1 where
    my_gcd a 0 = a
    my_gcd a b = my_gcd b (a `rem` b)

-- вектор задаётся списком координат
data Point = Point [Double] deriving (Eq, Show, Read)

-- distance x y находит расстояние между двумя точками в n-мерном
-- пространстве. Если число координат точек разное, сообщите об ошибке.
-- distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) == sqrt 2.0
-- distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) == 1.0

-- используйте рекурсию и сопоставление с образцом
distance :: Point -> Point -> Double
distance (Point x) (Point y) | length x /= length y || length x == 0 = error "Bad length"
                             | otherwise = sqrt (dist x y) where
                                dist [] _ = 0.0
                                dist _ [] = 0.0
                                dist (x:xs) (y:ys) = (x - y) ^ (2 :: Integer) + dist xs ys

-- intersect xs ys возвращает список, содержащий общие элементы двух списков.
-- intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4] (или [4, 2]!)
-- intersect [1, 2, 4, 6] [3, 5, 7] == []

-- используйте рекурсию и сопоставление с образцом
intersect :: [Integer] -> [Integer] -> [Integer]
intersect [] _ = []
intersect (x:xs) y | elem' x y = x : intersect xs y
                   | otherwise = intersect xs y
                        where
                            elem' _ [] = False
                            elem' x y = any (== x) y

-- zipN принимает список списков и возвращает список, который состоит из
-- списка их первых элементов, списка их вторых элементов, и так далее.
-- zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
zipN :: [[a]] -> [[a]]
zipN xss | any null xss = []
         | otherwise = map head xss : zipN (map tail xss)

-- Нижеперечисленные функции можно реализовать или рекурсивно, или с помощью
-- стандартных функций для работы со списками (map, filter и т.д.)
-- Попробуйте оба подхода! Хотя бы одну функцию реализуйте обоими способами.

-- Если в списке xs есть такие элементы x, для которых f x == True, то
-- find f xs возвращает Just (первый x), а findLast f xs -- Just (последний x).
-- Если таких нет, то обе функции возвращают Nothing
-- find (> 0) [-1, 2, -3, 4] == Just 2
-- findLast (> 0) [-1, 2, -3, 4] == Just 4
-- find (> 0) [-1, -2, -3] == Nothing
find, find_with_filter, findLast :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs) = if f x then Just x else find f xs

-- Версия с использование filter
find_with_filter f xs = let arr = filter f xs
                        in if null arr then Nothing else Just $ head arr

findLast f xs = helper f xs Nothing where
    helper _ [] ans = ans
    helper f (x:xs) ans = if f x then helper f xs (Just x) else helper f xs ans

-- mapFuncs принимает список функций fs и возвращает список результатов
-- применения всех функций из fs к x.
-- mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]
mapFuncs :: [a -> b] -> a -> [b]
mapFuncs [] _ = []
mapFuncs (f:fs) x = f x : mapFuncs fs x

-- satisfiesAll принимает список предикатов (функций, возвращающих Bool) preds
-- и возвращает True, если все они выполняются
-- (т.е. возвращают True) для x. Полезные стандартные функции: and, all.
-- satisfiesAll [even, \x -> x rem 5 == 0] 10 == True
-- satisfiesAll [] 4 == True (кстати, почему?)
satisfiesAll :: [a -> Bool] -> a -> Bool
satisfiesAll [] _ = True
satisfiesAll preds x = all (== True) (mapFuncs preds x)

-- непустой список состоит из первого элемента (головы)
-- и обычного списка остальных элементов
data NEL a = NEL a [a] deriving (Eq, Show, Read)

-- запишите правильный тип (т.е. такой, чтобы функция имела результат для любых аргументов)
-- и реализуйте функции на NEL, аналогичные tail, last и zip
-- tailNel :: NEL a -> ???
-- lastNel :: NEL a -> ???
-- zipNel :: NEL a -> NEL b -> ???
-- listToNel :: [a] -> ???
-- nelToList :: NEL a -> ???
tailNel :: NEL a -> [a]
tailNel (NEL _ x) = x

lastNel :: NEL a -> a
lastNel (NEL a []) = a
lastNel (NEL _ x) = last x

zipNel :: NEL a -> NEL b -> [(a, b)]
zipNel (NEL x xs) (NEL y ys) = (x, y) : zip xs ys

listToNel :: [a] -> NEL a
listToNel [] = error "empty list"
listToNel (x:xs) = NEL x xs

nelToList :: NEL a -> [a]
nelToList (NEL a xs) = a : xs
