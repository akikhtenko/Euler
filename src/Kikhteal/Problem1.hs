module Kikhteal.Problem1 where

multiplesSum::Integer -> [Integer] -> Maybe Integer
multiplesSum lim mul
        | lim < 1 || null mul = Nothing
        | otherwise  = Just $ sum $ filter (isMultiple mul) [1..lim]
        where isMultiple ys x = any (\y -> x `mod` y == 0) ys