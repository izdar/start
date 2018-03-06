
print':: [Int] -> [Int]
print' [] = []
print' (x:xs) = print' xs ++ [x]