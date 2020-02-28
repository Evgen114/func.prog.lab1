{-# LANGUAGE OverloadedStrings #-}

module Lab1_Test where

import EasyTest
import Control.Applicative

import Lab1
import Luhn
-- Для запуска сделайте двойной щелчок по этому файлу или зайдите в директорию и запустите
-- ghci Lab1_Test из командной строки.
-- Для перезагрузки после изменений (в Lab1 или Lab1_Test) используйте команду :reload (сокращённо :r) внутри GHCi.
-- Не забудьте попробовать :help (:h)!

-- документация EasyTest в EasyTest.html (http://hackage.haskell.org/package/easytest-0.2.1/docs/EasyTest.html)
-- там используются понятия и синтаксис, которые нам пока незнакомы, не беспокойтесь об этом
-- к концу курса вы должны быть способны понять большую часть EasyTest :)
-- <|> комбинирует тесты

-- обратите внимание на оформление списка: так удобнее добавлять, удалять и комментировать элементы в конце
allTests = tests
  -- scope даёт название тесту (и может быть вложено)
  [ scope "xor" $ tests
      -- expect проверяет условие (безусловный успех: ok, безусловная ошибка: crash "причина")
      [ expect $ xor True True == False
      , expect $ xor True False == True
      , expect $ xor False True == True
      , expect $ xor False False == False
      ]
    ,
    scope "max3" $ tests
      [ expect $ max3 1 3 2 == 3
      , expect $ max3 5 2 5 == 5
      , expect $ max3 1 2 3 == 3
      , expect $ max3 3 2 1 == 3
      ]
    ,
    scope "median3" $ tests
      [ expect $ median3 1 2 3 == 2
      , expect $ median3 1 3 2 == 2
      , expect $ median3 3 2 1 == 2
      , expect $ median3 3 1 2 == 2
      , expect $ median3 4 1 4 == 4
      ]
    ,
    scope "approx double equal" $ tests
      [ expect $ (~=) (3.4 ::Double) (3.40001 :: Double) == True
      , expect $ (~=) (3.4 ::Double) (3.4002 :: Double) == False
      ]
    ,
    scope "geomProgression" $ tests
      [ expect $ geomProgression 3.0 2 2 ~= 12.0
      , expect $ geomProgression 3.0 2 1 ~= 6.0
      , expect $ geomProgression 3.0 2 0 ~= 3.0
      , expect $ geomProgression 3.0 (1 / 3) 3 ~= (1 / 9)
      ]
    ,
    scope "rgbToCmyk" $ tests
      [ expect $ rgbToCmyk (RGB 41 5 73) ~= (CMYK 0.4384 0.9315 0.0 0.7137)
      , expect $ rgbToCmyk (RGB 0 255 0) ~= (CMYK 1.0 0.0 1.0 0.0)
      ]
    ,
    scope "coprime" $ tests
      [ expect $ coprime 10 15 == False
      , expect $ coprime 14 31 == True
      , expect $ coprime (-10) 15 == False
      , expect $ coprime 10 (-15) == False
      ]
    ,
    scope "distance" $ tests
      [ expect $ distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) ~= sqrt 2.0
      , expect $ distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) ~= 1.0
      , expect $ distance (Point [1.0, 5.0, 2.3]) (Point [-1.0, 3.5, 0.3]) ~= sqrt 10.25
      ]
    ,
    scope "intersect" $ tests
      [ expect $ intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4]
      , expect $ intersect [5, 4, 2, 5, 7] [1, 2, 4, 6] == [4, 2]
      , expect $ intersect [1, 2, 4, 6] [3, 5, 7] == []
      ]
    ,
    scope "zipN" $ tests
      [ expect $ zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
      , expect $ zipN [[1, 2], [3]] == [[1, 3]]
      , expect $ zipN [[1], [3, 2]] == [[1, 3]]
      , expect $ zipN ([[], [], []] :: [[Int]]) == []
      , expect $ zipN [[1, 1, 1], [2, 2], [3]] == [[1, 2, 3]]
      , expect $ zipN [[1, 1, 1], [2], [3, 4]] == [[1, 2, 3]]
      , expect $ zipN [[1, 2, 3, 4], [1, 2, 3], [1, 2]] == [[1, 1, 1], [2, 2, 2]]
      , expect $ zipN [[1], [1, 2], [1, 2, 3], [1, 2, 3, 4]] == [[1, 1, 1, 1]]
      ]
    ,
    scope "mapFuncs" $ tests
      [ expect $ mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]
      , expect $ mapFuncs ([]:: [Integer -> Integer]) 3 == []
      ]
    ,
    scope "satisfiesAll" $ tests
      [ expect $ satisfiesAll [even, \x -> x `rem` 5 == 0] 10 == True
      , expect $ satisfiesAll [] 4 == True
      ]
    ,
    scope "find" $ tests
      [ expect $ find (> 0) [-1, 2, -3, 4] == Just 2
      , expect $ find (> 0) [-1, -2, -3] == Nothing
      , expect $ find (> 0) [] == Nothing

      , expect $ find_with_filter (> 0) [-1, 2, -3, 4] == Just 2
      , expect $ find_with_filter (> 0) [-1, -2, -3] == Nothing
      , expect $ find_with_filter (> 0) [] == Nothing
      ]
    ,
    scope "findLast" $ tests
      [ expect $ findLast (> 0) [-1, 2, -3, 4] == Just 4
      , expect $ findLast (> 0) [-1, -2, -3] == Nothing
      , expect $ findLast (> 0) [] == Nothing
      ]
    ,
    scope "NEL" $ tests
      [ expect $ tailNel (NEL 5 []) == []
      , expect $ tailNel (NEL 5 [8, 9]) == [8, 9]
      , expect $ tailNel (NEL [5, 4] [[1, 1], [2, 2]]) == [[1, 1], [2, 2]]

      , expect $ lastNel (NEL 5 []) == 5
      , expect $ lastNel (NEL 5 [1, 2, 3]) == 3

      , expect $ zipNel (NEL 5 []) (NEL 9 []) == [(5, 9)]
      , expect $ zipNel (NEL 5 [4, 1]) (NEL 9 [10, 8]) == [(5, 9), (4, 10), (1, 8)]
      , expect $ zipNel (NEL 5 [4, 1, 8, 10]) (NEL 9 [10, 8, 9]) == [(5, 9), (4, 10), (1, 8), (8, 9)]

      , expect $ listToNel [5] == NEL 5 []
      , expect $ listToNel [5, 4, 10] == NEL 5 [4, 10]
      , expect $ listToNel [[5, 4], [10, 1], [0, 4]] == NEL [5, 4] [[10, 1], [0, 4]]

      , expect $ nelToList (NEL 5 []) == [5]
      , expect $ nelToList (NEL 5 [4, 10]) == [5, 4, 10]
      , expect $ nelToList (NEL [5, 4] [[10, 1], [0, 4]]) == [[5, 4], [10, 1], [0, 4]]
      ]
    ,
    -- Алгоритм Луна
    scope "calcEven" $ tests
      [ expect $ calcEven 0 == 0
      , expect $ calcEven 4 == 8
      , expect $ calcEven 5 == 1
      ]
    ,
    scope "calcSum" $ tests
      [ expect $ calcSum [1, 2, 3] == 4 + 4
      , expect $ calcSum [1, 6, 3] == 4 + 3
      , expect $ calcSum [1, 4] == 1 + 8
      , expect $ calcSum [1] == 1
      ]
    ,
    scope "isLuhnValid" $ tests
      [
        -- 2 + 2 + 6 + 4 + 9 + 8 + 5 + 6 = 42
        expect $ isLuhnValid [1, 2, 3, 4, 9, 8, 7, 6] == False
        ,
        -- 2 + 2 + 6 + 4 + 9 + 8 + 5 + 4 = 40
        expect $ isLuhnValid [1, 2, 3, 4, 9, 8, 7, 4] == True
      ]
  ]

main = run allTests -- runOnly "xor" allTests
                    -- rerun XXXX (или rerunOnly XXXX "xor") для повтора случайных тестов
