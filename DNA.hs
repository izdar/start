-- ---------------------------------------------------------------------
-- DNA Analysis 
-- CS300 Spring 2018
-- Due: 24 Feb 2018 @9pm
-- ------------------------------------Assignment 2------------------------------------
--
-- >>> YOU ARE NOT ALLOWED TO IMPORT ANY LIBRARY
-- Functions available without import are okay
-- Making new helper functions is okay
--
-- ---------------------------------------------------------------------
--
-- DNA can be thought of as a sequence of nucleotides. Each nucleotide is 
-- adenine, cytosine, guanine, or thymine. These are abbreviated as A, C, 
-- G, and T.
--
type DNA = [Char]
type RNA = [Char]
type Codon = [Char]
type AminoAcid = Maybe String

-- ------------------------------------------------------------------------
-- 				PART 1
-- ------------------------------------------------------------------------				

-- We want to calculate how alike are two DNA strands. We will try to 
-- align the DNA strands. An aligned nucleotide gets 3 points, a misaligned
-- gets 2 points, and inserting a gap in one of the strands gets 1 point. 
-- Since we are not sure how the next two characters should be aligned, we
-- try three approaches and pick the one that gets us the maximum score.
-- 1) Align or misalign the next nucleotide from both strands
-- 2) Align the next nucleotide from first strand with a gap in the second     
-- 3) Align the next nucleotide from second strand with a gap in the first    
-- In all three cases, we calculate score of leftover strands from recursive 
-- call and add the appropriate penalty.                                    
maxOf3:: Int -> Int -> Int -> Int
maxOf3 x y z = max (max x y) z

inital x y = maxOf3 (sco x y) (sco (' ':x) y) (sco x (' ':y))

sco :: DNA -> DNA -> Int
sco [] [] = 0
sco [] x = 0
sco x [] = 0
sco (x:xs) (y:ys) | x ==' '|| y==' ' = 1 + maxOf3 (sco xs ys) (sco (' ':xs) ys) (sco xs (' ':ys))
				  | x == y = 3 + maxOf3 (sco xs ys) (sco (' ':xs) ys) (sco xs (' ':ys))
				  | otherwise = 2 + maxOf3 (sco xs ys) (sco (' ':xs) ys) (sco xs (' ':ys))

score :: DNA -> DNA -> Int
score x y = inital x y

-- -------------------------------------------------------------------------
--				PART 2
-- -------------------------------------------------------------------------
-- Write a function that takes a list of DNA strands and returns a DNA tree. 
-- For each DNA strand, make a separate node with zero score 
-- in the Int field. Then keep merging the trees. The merging policy is:
-- 	1) Merge two trees with highest score. Merging is done by making new
--	node with the smaller DNA (min), and the two trees as subtrees of this
--	tree
--	2) Goto step 1 :)
--
data DNATree = Node DNA Int DNATree DNATree | Nil deriving (Ord, Show, Eq)

treeList::[DNA] -> [DNATree]
treeList [] = []
treeList (x:xs) = Node x 0 Nil Nil:treeList xs

returnInt::DNATree -> Int
returnInt (Node dna x l r) = x

returnDNA:: DNATree -> DNA
returnDNA (Node dna x l r) = dna

minn:: DNATree -> DNATree ->DNATree
minn (Node dna1 x left1 right1) (Node dna2 y left2 right2) | min x y == x = (Node dna1 x left1 right1)
														   | otherwise = (Node dna2 y left2 right2)

maxTreeList:: DNATree -> [DNATree] -> [(DNATree, Int)]
maxTreeList _ [] = []
maxTreeList y (x:xs) | returnDNA x /= returnDNA y && minn y x == y = (Node (min (returnDNA x) (returnDNA y)) (score (returnDNA x) (returnDNA y)) y x,(score (returnDNA x) (returnDNA y))):maxTreeList y xs
					 | returnDNA x /= returnDNA y && minn y x == x = (Node (min (returnDNA x) (returnDNA y)) (score (returnDNA x) (returnDNA y)) x y,(score (returnDNA x) (returnDNA y))):maxTreeList y xs
					 | otherwise = maxTreeList y xs

treeTraversal:: DNATree -> [DNATree]
treeTraversal Nil = []
treeTraversal (Node a x l r) = (Node a x l r):treeTraversal l ++ treeTraversal r

listFilter:: [DNATree] -> [DNATree] -> [DNATree]
listFilter x y = filter (`notElem` x) y

maxTree:: [(DNATree,Int)] -> DNATree -> DNATree
maxTree [] c = c
maxTree (x:xs) c | snd x > returnInt c = maxTree xs (fst x)
				 | otherwise = maxTree xs c

makeTreeList:: [DNATree] -> [(DNATree, Int)]
makeTreeList [] = []
makeTreeList (x:xs) = maxTreeList x xs ++ makeTreeList xs

removeMaxTree::[(DNATree,Int)] -> DNATree -> [(DNATree,Int)]
removeMaxTree [] _ = []
removeMaxTree (x:xs) y | fst x /= y = x:removeMaxTree xs y
					   | otherwise = removeMaxTree xs y

gameOver:: [DNATree] -> [DNATree] 
gameOver x = listFilter (treeTraversal (maxTree (makeTreeList x) (Node "ASA" 0 Nil Nil))) x

theEnd:: [DNATree] -> [DNATree]
theEnd x | length x > 1 = theEnd ((maxTree (makeTreeList x) (Node "ASA" 0 Nil Nil)):(gameOver x))
		 | otherwise = x

makeDNATree :: [DNA] -> DNATree
makeDNATree x = head (theEnd (treeList x))

-- -------------------------------------------------------------------------
--				PART 3
-- -------------------------------------------------------------------------

-- Even you would have realized it is hard to debug and figure out the tree
-- in the form in which it currently is displayed. Lets try to neatly print 
-- the DNATree. Each internal node should show the 
-- match score while leaves should show the DNA strand. In case the DNA strand 
-- is more than 10 characters, show only the first seven followed by "..." 
-- The tree should show like this for an evolution tree of
-- ["AACCTTGG","ACTGCATG", "ACTACACC", "ATATTATA"]
--
-- 20
-- +---ATATTATA
-- +---21
--     +---21
--     |   +---ACTGCATG
--     |   +---ACTACACC
--     +---AACCTTGG
--
-- Make helper functions as needed. It is a bit tricky to get it right. One
-- hint is to pass two extra string, one showing what to prepend to next 
-- level e.g. "+---" and another to prepend to level further deep e.g. "|   "

getLeft Nil = Nil
getLeft (Node a b l r) = l

getRight Nil = Nil
getRight (Node a b l r) = r

treeHeight:: DNATree -> Int
treeHeight Nil = 0
treeHeight x = 1 + max (treeHeight (getLeft x)) (treeHeight (getRight x))

level:: DNATree -> Int -> [(DNATree,Int)]
level Nil n = []
level x n = [(x,n)] ++ (level (getLeft x) (n+1)) ++ (level (getRight x) (n+1))

levelOrder:: [(DNATree,Int)] -> Int -> [DNATree]
levelOrder [] n = []
levelOrder (x:xs) n | snd x == n = fst x:levelOrder xs n
					| otherwise = levelOrder xs n

spaces:: Int -> [Char]
spaces 0 = []
spaces n | n > 0 = ' ':spaces (n-1)
		 | otherwise = []

levelDenoter 0 = []
levelDenoter n | n > 0 = " |" ++ levelDenoter (n-1)
			   | otherwise = []

printLevel:: [DNATree] -> Int -> [Char]
printLevel [] _ = []
printLevel (x:xs) n | returnInt x /= 0 && n == 0 = show (returnInt x) ++ "\n" ++ printLevel xs n
					| returnInt x == 0 = spaces (n-1) ++ levelDenoter (n-1) ++ "+---" ++ returnDNA x ++ "\n" ++ printLevel xs n
					| otherwise = spaces (n-1) ++ levelDenoter (n-1) ++ "+---" ++ show (returnInt x) ++ "\n" ++ printLevel xs n

draw_almost :: DNATree -> Int -> Int -> [Char]
draw_almost Nil _ _ = []
draw_almost x n height | n < height = (printLevel (levelOrder (level x 0) n) n) ++ (draw_almost x (n+1) height)
					   | otherwise = []

draw:: DNATree -> [Char]
draw x = (draw_almost x 0 (treeHeight x))

-- ---------------------------------------------------------------------------
--				PART 4
-- ---------------------------------------------------------------------------
--
--
-- Our score function is inefficient due to repeated calls for the same 
-- suffixes. Lets make a dictionary to remember previous results. First you
-- will consider the dictionary as a list of tuples and write a lookup
-- function. Return Nothing if the element is not found. Also write the 
-- insert function. You can assume that the key is not already there.
type Dict a b = [(a,b)]

lookupDict :: (Eq a) => a -> Dict a b -> Maybe b
lookupDict a [] = Nothing
lookupDict a (b:bs) | a == fst b = Just (snd b)
					| otherwise = lookupDict a bs

insertDict :: (Eq a) => a -> b -> (Dict a b)-> (Dict a b)
insertDict a b c = c ++ [(a,b)]

-- We will improve the score function to also return the alignment along
-- with the score. The aligned DNA strands will have gaps inserted. You
-- can represent a gap with "-". You will need multiple let expressions 
-- to destructure the tuples returned by recursive calls.

-- findScore:: Int -> [((String,String),Int)] -> ((String,String),Int)
-- findScore x [] = (([],[]),0)
-- findScore c (x:xs)| snd x == c = x
-- 				  | otherwise = findScore c xs

alignment :: String -> String -> ((String, String), Int)
alignment = undefined

-- We will now pass a dictionary to remember previously calculated scores 
-- and return the updated dictionary along with the result. Use let 
-- expressions like the last part and pass the dictionary from each call
-- to the next. Also write logic to skip the entire calculation if the 
-- score is found in the dictionary. You need just one call to insert.

type ScoreDict = Dict (DNA,DNA) Int

scoreMemo :: (DNA,DNA) -> ScoreDict -> (ScoreDict,Int)
scoreMemo = undefined
-- In this part, we will use an alternate representation for the 
-- dictionary and rewrite the scoreMemo function using this new format.
-- The dictionary will be just the lookup function so the dictionary 
-- can be invoked as a function to lookup an element. To insert an
-- element you return a new function that checks for the inserted
-- element and returns the old dictionary otherwise. You will have to
-- think a bit on how this will work. An empty dictionary in this 
-- format is (\_->Nothing)

type Dict2 a b = a->Maybe b 
insertDict2 :: (Eq a) => a -> b -> (Dict2 a b)-> (Dict2 a b)
insertDict2 = undefined

type ScoreDict2 = Dict2 (DNA,DNA) Int

scoreMemo2 :: (DNA,DNA) -> ScoreDict2 -> (ScoreDict2,Int)
scoreMemo2 = undefined

-- ---------------------------------------------------------------------------
-- 				PART 5
-- ---------------------------------------------------------------------------

-- Now, we will try to find the mutationDistance between two DNA sequences.
-- You have to calculate the number of mutations it takes to convert one 
-- (start sequence) to (end sequence). You will also be given a bank of 
-- sequences. However, there are a couple of constraints, these are as follows:

-- 1) The DNA sequences are of length 8
-- 2) For a sequence to be a part of the mutation distance, it must contain 
-- "all but one" of the neuclotide bases as its preceding sequence in the same 
-- order AND be present in the bank of valid sequences
-- 'AATTGGCC' -> 'AATTGGCA' is valid only if 'AATTGGCA' is present in the bank
-- 3) Assume that the bank will contain valid sequences and the start sequence
-- may or may not be a part of the bank.
-- 4) Return -1 if a mutation is not possible

	
-- mutationDistance "AATTGGCC" "TTTTGGCA" ["AATTGGAC", "TTTTGGCA", "AAATGGCC" "TATTGGCC", "TTTTGGCC"] == 3
-- mutationDistance "AAAAAAAA" "AAAAAATT" ["AAAAAAAA", "AAAAAAAT", "AAAAAATT", "AAAAATTT"] == 2

allButOne:: DNA -> DNA -> Int -> Bool
allButOne [] [] n | n == 1 = True
				  | otherwise = False
allButOne (x:xs) (y:ys) n | x == y = allButOne xs ys n
						  | x /= y && n == 1 = False
						  | x /= y = allButOne (y:xs) (y:ys) (n+1)

findMut::  DNA -> [DNA] -> [DNA]
findMut x [] = []
findMut [] x = []
findMut x (y:ys) | allButOne x y 0 && x /= y = y:findMut x ys
				 | otherwise = findMut x ys

mutAll:: [DNA] -> [DNA] -> DNA -> Int -> ([DNA],Int)
mutAll [] y z c = ([],c)
mutAll (x:xs) y z c | c == 1 && length (filter (==z) (findMut x y)) /= 0 = ((findMut x y),c+1)
					| length (filter (==z) (findMut x y)) == 0 = mutAll (findMut x y ++ fst (mutAll xs y z (c+1))) (filter (`notElem` (findMut x y)) y) z (c+1)
				    | otherwise = ((findMut x y),c)

mutationDistance :: DNA -> DNA -> [DNA] -> Int
mutationDistance x y z | snd (mutAll (findMut x z) z y 1) == 1000 && snd (mutAll (findMut y z) z x 1) == 1000 = -1
					   | snd (mutAll (findMut x z) z y 1) == 1000 = snd (mutAll (findMut y z) z x 1)
					   | snd (mutAll (findMut y z) z x 1) == 1000 = snd (mutAll (findMut x z) z y 1)
					   | otherwise = min (snd (mutAll (findMut y z) z x 1)) (snd (mutAll (findMut x z) z y 1))


-- ---------------------------------------------------------------------------
-- 				PART 6
-- ---------------------------------------------------------------------------
--
-- Now, we will write a function to transcribe DNA to RNA. 
-- The difference between DNA and RNA is of just one base i.e.
-- instead of Thymine it contains Uracil. (U)
--
transcribeDNA :: DNA -> RNA
transcribeDNA [] = []
transcribeDNA (x:xs) | x == 'T' = 'U':transcribeDNA xs
				     | otherwise = x:transcribeDNA xs

-- Next, we will translate RNA into proteins. A codon is a group of 3 neuclotides 
-- and forms an aminoacid. A protein is made up of various amino acids bonded 
-- together. Translation starts at a START codon and ends at a STOP codon. The most
-- common start codon is AUG and the three STOP codons are UAA, UAG and UGA.
-- makeAminoAcid should return Nothing in case of a STOP codon.
-- Your translateRNA function should return a list of proteins present in the input
-- sequence. 
-- Please note that the return type of translateRNA is [String], you should convert
-- the abstract type into a concrete one.
-- You might wanna use the RNA codon table from 
-- https://www.news-medical.net/life-sciences/RNA-Codons-and-DNA-Codons.aspx
-- 
--

proteinList = [("UUU","Phe"),("UCU","Ser"),("UAU","Tyr"),("UGU","Cys"),("UUC","Phe"),("UCC","Ser"),("UAC","Tyr"),("UGC","Cys"),("UUA","Leu"),("UCA","Ser"),("UAA","STOP"),("UGA","STOP"),("UUG", "Leu"),("UCG","Ser"),("UAG","STOP"),("UGG","Trp"),("CUU","Leu"),("CCU","Pro"),("CAU","His"),("CGU", "Arg"),("CUC","Leu"),("CCC","Pro"),("CAC","His"),("CGC", "Arg"),("CUA","Leu"),("CCA", "Pro"),("CAA","Gln"),("CGA","Arg"), ("CUG","Leu"),("CCG","Pro"),("CAG","Gln"),("CGG","Arg"),("AUU","Ile"),("ACU","Thr"),("AAU","Asn"),("AGU","Ser"),("AUC","Ile"),("ACC","Thr"),("AAC","Asn"),("AGC","Ser"),("AUA","Ile"),("ACA", "Thr"),("AAA","Lys"),("AGA","Arg"),("AUG", "Met"),("AUG","START"),("ACG","Thr"), ("AAG","Lys"),("AGG","Arg"),("GUU", "Val"),("GCU", "Ala"),("GAU","Asp"), ("GGU", "Gly"), ("GUC", "Val"),("GCC", "Ala"), ("GAC","Asp"),("GGC", "Gly"),("GUA","Val"),("GCA", "Ala"), ("GAA", "Glu"), ("GGA", "Gly"), ("GUG", "Val"), ("GCG", "Ala"), ("GAG", "Glu"), ("GGG", "Gly")]

makeAminoAcidMyFunct x [] = Nothing
makeAminoAcidMyFunct x (y:ys) | x == fst y = Just (snd y)
					   | otherwise = makeAminoAcidMyFunct x ys

makeAminoAcid :: Codon -> AminoAcid
makeAminoAcid x = makeAminoAcidMyFunct x proteinList

translate:: RNA -> Int -> RNA
translate [] n = []
translate (x:xs) n | n `mod` 3 /= 0 = x:translate xs (n+1)
				   | otherwise = x:[]

translateList:: RNA -> Int -> [RNA]
translateList [] n = []
translateList (x:xs) n | n `mod` 3 /= 0 = (translate (x:xs) n):translateList xs (n+1)
				   	   | otherwise = translateList xs (n+1)

filterList:: [RNA] -> [RNA]
filterList [] = []
filterList (x:xs) | length x == 3 = x:filterList xs
				  | otherwise = filterList xs

findProt _ [] = []
findProt x (y:ys) | x == fst y = snd y
				  | otherwise = findProt x ys

aminoConvert:: [RNA] -> [(RNA,String)] -> [String]
aminoConvert [] y = []
aminoConvert (x:xs) y = (findProt x y):aminoConvert xs y

readAmino:: [String] -> Int -> [AminoAcid]
readAmino [] n = []
readAmino (x:xs) n | x == "Met" = Just x:readAmino xs 1
				   | n == 1 && x /= "STOP" = Just x:readAmino xs 1
				   | n == 1  && x == "STOP" = Just "123":readAmino xs 0
				   | otherwise = readAmino xs 0

finalWork:: [AminoAcid] -> String
finalWork [] = [] 
finalWork (x:xs) | (\(Just x) -> x) x == "123" = [] 
				 | otherwise = (\(Just x) -> x) x ++ finalWork xs

count123 [] = 0
count123 (x:xs) | x /= "123" = 1 + count123 xs
				| otherwise = 0

work:: [String] -> [String]
work [] = []
work x | filter (=="123") x /= [] = finalWork x:work (drop ((count123 x)+1) x)
	   | otherwise = []

translateRNA :: RNA -> [String]
translateRNA x = work (readAmino (aminoConvert (filterList (translateList x 1)) proteinList) 0)









