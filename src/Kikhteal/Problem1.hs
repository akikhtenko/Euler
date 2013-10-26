module Kikhteal.Problem1 where

multiplesSum::Integer -> [Integer] -> Integer
multiplesSum lim mul = sum $ filter (isMultiple mul) [1..lim]
        where isMultiple ys x = any (\y -> x `mod` y == 0) ys