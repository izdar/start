-- ========================================================================================================================== --


--
--                                                          ASSIGNMENT 1
--
--      A common type of text alignment in print media is "justification", where the spaces between words, are stretched or
--      compressed to align both the left and right ends of each line of text. In this problem we'll be implementing a text
--      justification function for a monospaced terminal output (i.e. fixed width font where every letter has the same width).
--
--      Alignment is achieved by inserting blanks and hyphenating the words. For example, given a text:
--
--              "He who controls the past controls the future. He who controls the present controls the past."
--
--      we want to be able to align it like this (to a width of say, 15 columns):
--
--              He who controls
--              the  past cont-
--              rols  the futu-
--              re. He  who co-
--              ntrols the pre-
--              sent   controls
--              the past.
--


-- ========================================================================================================================== --


import Data.List
import Data.Char

text1 = "He who controls the past controls the future. He who controls the present controls the past."
text2 = "A creative man is motivated by the desire to achieve, not by the desire to beat others."


-- ========================================================================================================================== --







-- ========================================================= PART 1 ========================================================= --


--
-- Define a function that splits a list of words into two lists, such that the first list does not exceed a given line width.
-- The function should take an integer and a list of words as input, and return a pair of lists.
-- Make sure that spaces between words are counted in the line width.
--
-- Example:
--    splitLine ["A", "creative", "man"] 12   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 11   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 10   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 9    ==>   (["A"], ["creative", "man"])
--


splitLine :: [String] -> Int -> ([String], [String])
-- Function definition here
lenn [] = 0
lenn (x:xs) = 1 + length x + lenn xs

helper [] y n = ([],[])
helper (x:xs) y c    | c == 0 && lenn (x:xs) == y = ((x:xs),[])
					 | (length x) < y = (x:fst (helper xs (y-(1+length x)) 1),snd (helper xs (y-(1+length x)) 1))
					 | otherwise = ([],(x:xs))

splitLine x y = helper x y 0


-- ========================================================= PART 2 ========================================================= --



-- To be able to align the lines nicely. we have to be able to hyphenate long words. Although there are rules for hyphenation
-- for each language, we will take a simpler approach here and assume that there is a list of words and their proper hyphenation.
-- For example:

enHyp = [("creative", ["cr","ea","ti","ve"]), ("controls", ["co","nt","ro","ls"]), ("achieve", ["ach","ie","ve"]), ("future", ["fu","tu","re"]), ("present", ["pre","se","nt"]), ("motivated", ["mot","iv","at","ed"]), ("desire", ["de","si","re"]), ("others", ["ot","he","rs"])]


--
-- Define a function that splits a list of words into two lists in different ways. The first list should not exceed a given
-- line width, and may include a hyphenated part of a word at the end. You can use the splitLine function and then attempt
-- to breakup the next word with a given list of hyphenation rules. Include a breakup option in the output only if the line
-- width constraint is satisfied.
-- The function should take a hyphenation map, an integer line width and a list of words as input. Return pairs of lists as
-- in part 1.
--
-- Example:
--    lineBreaks enHyp 12 ["He", "who", "controls."]   ==>   [(["He","who"], ["controls."]), (["He","who","co-"], ["ntrols."]), (["He","who","cont-"], ["rols."])]
--
-- Make sure that words from the list are hyphenated even when they have a trailing punctuation (e.g. "controls.")
--
-- You might find 'map', 'find', 'isAlpha' and 'filter' useful.
--


lineBreaks :: [(String, [String])] -> Int -> [String] -> [([String], [String])]
--Function definition here

find' :: [(String, [String])] -> String -> ([String])
find' x [] = []
find' [] y = []
find' (x:xs) y  | (fst x) == y = snd x
				| otherwise = find' xs y

getWord :: [String] -> Int -> String
getWord [] n = []
getWord x n | head (snd (splitLine x n)) /= [] = head (snd (splitLine x n))
			| otherwise = []

joinAll [] n = []
joinAll (x:xs) n | n > 0 = joinAll xs (n-(length x))
				 | otherwise = x ++ (joinAll xs (n-(length x)))

joinSome [] n = []
joinSome (x:xs) y | y <= 0 = []
			      | otherwise = x ++ (joinSome xs (y-(length x)))

hyp :: [String] -> [String] -> Int -> ([String],[String])
hyp x [] n = ([],[])
hyp (x:xs) (y:ys) n | (length x+1) < n = (x:fst (hyp xs (y:ys) (n-(1+length x))),snd (hyp xs (y:ys) (n-(1+length x))))
					| joinSome (y:ys) (n-(length y)) /=[] && xs ==[] = ([joinSome (y:ys) (n-(length y)) ++ "-"],[drop (length (joinSome (y:ys) (n-(length y)))) x])
					-- | (n-(length y)) > 0 && joinSome (y:ys) (n-(length y)) /=[] && xs ==[] = ([joinSome (y:ys) (n-(length y)) ++ "-"],[drop (length (joinSome (y:ys) (n-(length y)))) x])
					| joinSome (y:ys) (n-(length y)) /=[] = ([joinSome (y:ys) (n-(length y)) ++ "-"],[drop (length (joinSome (y:ys) (n-(length y)))) x] ++ xs)
					| otherwise = ([],(x:xs))

sndSize [] = 0
sndSize (x:xs) = 1 + (length x) + sndSize xs

fin x (y:ys) n | sndSize (fst (splitLine x n)) < n = [(hyp x (y:ys) n)] ++ (fin x (y:ys) (n-1)) 
		       | sndSize (fst (splitLine x n)) == n = [splitLine x n]


lineBreaks enHyp n x | (find' enHyp (filter isAlpha (getWord x (n)))) == [] = [splitLine x n]
					 | otherwise = nub (fin x (find' enHyp (filter isAlpha (getWord x (n)) )) n)






-- ========================================================= PART 3 ========================================================= --


--
-- Define a function that inserts a given number of blanks (spaces) into a list of strings and outputs a list of all possible
-- insertions. Only insert blanks between strings and not at the beginning or end of the list (if there are less than two
-- strings in the list then return nothing). Remove duplicate lists from the output.
-- The function should take the number of blanks and the the list of strings as input and return a lists of strings.
--
-- Example:
--    blankInsertions 2 ["A", "creative", "man"]   ==>   [["A", " ", " ", "creative", "man"], ["A", " ", "creative", " ", "man"], ["A", "creative", " ", " ", "man"]]
--
-- Use let/in/where to make the code readable
--



blankInsertions :: Int -> [String] -> [[String]]
addSpace y 0 n = y
addSpace (y:ys) x n | x == n = y:" ":addSpace ys (x-1) n
				    | x < n = " ":addSpace (y:ys) (x-1) n
				    | otherwise = (y:ys)

order x [] = True
order [] x = True
order (x:xs) (y:ys) | y == " " = order (x:xs) ys
                    | x == y && y == last (y:ys) = True
                    | x == y = order xs ys
                    | otherwise = False

permute 0 y = [y]
permute x y | length y > 1 && x > 0 = (permutations (addSpace y x x))
			| x <= 0 = [y]

almost 0 y = [y]
almost x y | x > 0 = filter (order y) (permute x y)
		   | otherwise = [y]

orderFix [] = []
orderFix (x:xs) | head x == " " || last x == " " = orderFix xs
				| otherwise = x:orderFix xs 

blankInsertions x y | x <= 0 = [y]
blankInsertions x y | x > 0 = nub (orderFix (almost x y))
					| otherwise = [y]


-- ========================================================= PART 4 ========================================================= --



-- Define a function to score a list of strings based on four factors:

--    blankCost: The cost of introducing each blank in the list
--    blankProxCost: The cost of having blanks close to each other
--    blankUnevenCost: The cost of having blanks spread unevenly
--    hypCost: The cost of hyphenating the last word in the list

-- The cost of a list of strings is computed simply as the weighted sum of the individual costs. The blankProxCost weight equals
-- the length of the list minus the average distance between blanks (0 if there are no blanks). The blankUnevenCost weight is
-- the variance of the distances between blanks.

-- The function should take a list of strings and return the line cost as a double

-- Example:
--    lineCost ["He", " ", " ", "who", "controls"]
--        ==>   blankCost * 2.0 + blankProxCost * (5 - average(1, 0, 2)) + blankUnevenCost * variance(1, 0, 2) + hypCost * 0.0
--        ==>   blankCost * 2.0 + blankProxCost * 4.0 + blankUnevenCost * 0.666...

-- Use let/in/where to make the code readable



---- Do not modify these in the submission ----
blankCost = 1.0
blankProxCost = 1.0
blankUnevenCost = 1.0
hypCost = 1.0
-----------------------------------------------


lineCost :: [String] -> Double
-- Function definition here

firstSpace [] = 0
firstSpace (x:xs) | x /= " " = 1 + firstSpace xs
				  | otherwise = 0

lastSpace [] count = count
lastSpace (x:xs) count | x == " " = lastSpace xs 0
					   | otherwise = lastSpace xs (count + 1)

isolate [] count = []
isolate (x:xs) count   | x /= " " && count == 0 = isolate xs count
			           | x == " " && count == 0 = x:isolate xs 1
			           | otherwise = x:isolate xs 1

no_spaces [] = 0
no_spaces (x:xs) | x == " " = 1 + no_spaces xs
				 | otherwise = no_spaces xs

length' [] = 0
length' (x:xs) = 1 + length' xs

inBetween x = length' (filter (/= " ") (isolate (reverse (isolate x 0)) 0))

avg x y z = (x + y + z)/3

var x y z = (((x - (avg x y z ))*(x - (avg x y z )))+((y - (avg x y z ))*(y - (avg x y z )))+((z - (avg x y z ))*(z - (avg x y z ))))/3

isHyp:: [String] -> Double
isHyp [] = 0
isHyp x | last (last x) == '-' = 1
		| otherwise = 0

lineCost x = (blankCost * (no_spaces x)) + (blankProxCost * ((length' x) - (avg (firstSpace x) (inBetween x) (lastSpace x 0)))) + (blankUnevenCost * (var (firstSpace x) (inBetween x) (lastSpace x 0))) + (hypCost * (isHyp x))





-- ========================================================= PART 5 ========================================================= --


--
-- Define a function that returns the best line break in a list of words given a cost function, a hyphenation map and the maximum
-- line width (the best line break is the one that minimizes the line cost of the broken list).
-- The function should take a cost function, a hyphenation map, the maximum line width and the list of strings to split and return
-- a pair of lists of strings as in part 1.
--
-- Example:
--    bestLineBreak lineCost enHyp 12 ["He", "who", "controls"]   ==>   (["He", "who", "cont-"], ["rols"])
--
-- Use let/in/where to make the code readable
--


bestLineBreak :: ([String] -> Double) -> [(String, [String])] -> Int -> [String] -> ([String], [String])
-- Function definition here

strLen [] = 0
strLen (x:xs) = 1 + length x + strLen xs

blanks [] n = []
blanks (x:xs) n  = (blankInsertions (n - ((strLen (fst x)))) (fst x)) ++ blanks xs n

minBlankCost f [] = []
minBlankCost f (x:xs) = f x:minBlankCost f xs

bestBlankCost f x = minimum (minBlankCost f x)

minCostList f [] n =  []
minCostList f (x:xs) n | f x == n = x
			    	   | otherwise = minCostList f xs n

finalList [] y = (y,[])
finalList (x:xs) y  | last (fst x) == (last y) = (y,snd x)
					| otherwise = finalList xs y

bestLineBreak f enHyp n x | n >= strLen x = (x,[]) 
						  | otherwise = finalList (lineBreaks enHyp n x) (minCostList f (blanks (lineBreaks enHyp n x) n) (bestBlankCost f (blanks (lineBreaks enHyp n x) n)))


--
-- Finally define a function that justifies a given text into a list of lines satisfying a given width constraint.
-- The function should take a cost function, hyphenation map, maximum line width, and a text string as input and return a list of
-- strings.
--
-- 'justifyText lineCost enHyp 15 text1' should give you the example at the start of the assignment.
--
-- You might find the words and unwords functions useful.
--


justifyText :: ([String] -> Double) -> [(String, [String])] -> Int -> String -> [String]
-- Function definition here

itEndsNow f enHyp n [] = []
itEndsNow f enHyp n x = [unwords ((fst (bestLineBreak f enHyp n x)))] ++ (itEndsNow f enHyp n ( (snd (bestLineBreak f enHyp n x))))

justifyText f enHyp n x = (itEndsNow f enHyp n (words x))







