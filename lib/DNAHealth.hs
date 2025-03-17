{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}

module DNAHealth where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!))
import Control.Monad.State (MonadState(..), execState)
import Control.Monad (replicateM)
import qualified Data.Vector as V

newtype DNA = DNA {inner :: String}
    deriving stock (Show)

data Gene = Gene
    { str :: String
    , health :: Int
    }
    deriving stock (Show)

type Health = Int

validChars :: [Char]
validChars = ['a' .. 'z']

mkDNA :: String -> Maybe DNA
mkDNA "" = Nothing
mkDNA cs
    | all (`elem` validChars) cs = Just $ DNA cs
    | otherwise = Nothing

mkGene :: String -> Int -> Maybe Gene
mkGene "" _ = Nothing
mkGene cs h
    | all (`elem` validChars) cs =
        Just $ Gene cs h
    | otherwise = Nothing

data Strand = Strand
    { start :: Int
    , end :: Int
    , d :: DNA
    }
    deriving stock (Show)

mkStrand :: Int -> Int -> String -> Maybe Strand
mkStrand s e d = Strand s e <$> mkDNA d

-- Genes in the health value list are
-- not guaranteed to be unique so we
-- can't use a HashMap. We also seem
-- to reference them by index anyway
-- so a Vector of (Gene, Health) seems best.
--
--
-- Part 1: Calculate health for each strand
-- Some sort of substring problem
--
-- Part 2: Min/max of set
--
-- Can do in at least as good as O(n) since we
-- can just iterate through all the elements.
--
-- Can amortise it away completely by keeping
-- track of the min/max during part 1.

selectGenes :: Vector Gene -> Strand -> [Gene]
selectGenes gs strand =
    [gs ! n | n <- [strand.start .. strand.end]]

-- We need to recursively find the longest
-- substring of strand.d that is a member
-- of gs.
--
-- Assuming that the longest string won't
-- leave us with some "hanging" genes at
-- the end which should've been included
-- had we chosen a non-longest string.
--
-- It seems, after looking at the examples,
-- that we've made a series of wrong assumptions.
--
-- In fact, even after selecting the healthy
-- genes for a given strand, they're still
-- not guaranteed to be unique. On the plus
-- side, we can put these into a map and sum
-- any duplicates since adding both health
-- values is the desired behaviour. This
-- gets us O(1) amortised lookup in a HashMap.
--
-- The strange thing from the examples is that,
-- once we find a substring, we don't remove it.
-- The sequence `aaa` is not `aa` and `a` but instead
-- the middle character is used twice and we get
-- `aa` and `aa`. So we seem to only drop one character
-- at a time.
--
-- That dropping of 1 character at a time resolves
-- our above concern about taking the longest
-- substring and leaving "dangling" characters
-- at the end.
--
-- We now wish to take all longest prefixes of
-- strand.d, dropping the first character each
-- time, which are members of gs.keys and
-- sum their corresponding value in gs.
--
-- This seems very expensive, perhaps we should
-- view the problem differently...
--
-- Invert, always invert. One of Charlie Munger's
-- "mental models".
--
-- Rather than searching for prefixes of the
-- strand in genes. Let's look for instances
-- of genes in the strand.
--
-- Now our problem becomes a simple search
-- for all occurences of each substring within
-- the strand. There are a plethora of algorithms
-- for this, depending on various constraints.
--
-- While the examples are short, no such constraint
-- formally guarantees the genes will be short and
-- a constraint explicitly gives an upper bound of
-- 2x10^6 for the sum of all strand lengths.
--
-- We are also given a constraint that n <= 10^5.
--
-- If we assume that the worst case of 2000000
-- being the sum of strand lengths corresponds to
-- the constraint of 1000000 strands. We can assume
-- an approximate limit of around 200 for the length
-- of strands, this is computationally quite small.
--
-- That said, we will be querying the same string
-- for multiple genes so a brute force algorithm
-- is likely inappropriate.
--
-- Likewise, KMP probably isn't appropriate since,
-- based on the examples, a mismatch doesn't let
-- us jump forward very far.
--
-- We also expect to find our patterns multiple
-- times within the given text.
--
-- This observations lead towards constructing
-- some sort of data structure, to aid in multiple
-- queries.
--
-- A suffix tree seems to be appropriate.

type Edge = (String, SuffixTree)

data SuffixTree
    = Node [Edge]
    | Leaf
    deriving stock (Show)

suffixes :: String -> [String]
suffixes "" = []
suffixes str@(_ : cs) = str : suffixes cs

longestCommonPrefix :: String -> String -> String
longestCommonPrefix "" _ = ""
longestCommonPrefix _ "" = ""
longestCommonPrefix (x : xs) (y : ys)
    | x == y = x : longestCommonPrefix xs ys
    | otherwise = ""

unsafeStripPrefix :: String -> String -> String
unsafeStripPrefix pref str = fromJust $ stripPrefix pref str

edges :: SuffixTree -> [Edge]
edges Leaf = []
edges (Node es) = es

-- There are O(n) algorithms to construct
-- a suffix tree, but they are very complex.
insertSuffix :: String -> SuffixTree -> SuffixTree
insertSuffix s Leaf = Node [("", Leaf), (s, Leaf)]
insertSuffix s (Node []) = Node [(s, Leaf)]
insertSuffix sx0 (Node (e@(sy0, t) : es0))
    | pref == "" =
        let es = edges $ insertSuffix sx0 (Node es0)
         in Node (e : es)
    | otherwise =
        let sx = unsafeStripPrefix pref sx0
            sy = unsafeStripPrefix pref sy0
            prefEdge =
                if sy == ""
                    -- sy0 is completely a prefix of sx0
                    -- then we want to insert sx into
                    -- the edges from sx0/pref.
                    then (pref, insertSuffix sx t)
                    -- sy0 and sx0 shared a partial prefix,
                    -- we want to create a new edge with the
                    -- shared prefix, place sy with its original
                    -- children as an edge and then insert sx
                    -- as an additiona "leaf edge"
                    else (pref, Node [(sx, Leaf), (sy, t)])
         in Node (prefEdge : es0)
    where
        pref = longestCommonPrefix sy0 sx0

suffixTree :: String -> SuffixTree
suffixTree = foldr insertSuffix (Node []) . suffixes

-- Now we can easily determine the number of times a
-- substring occurs by searching for it in our trNode [("a",Node [("",Leaf),("na",Node [("",Leaf),("na",Leaf)])]),("na",Node [("",Leaf),("na",Leaf)]),("banana",Leaf)]ee
-- and counting the child nodes once we find it.

size :: SuffixTree -> Int
size Leaf = 1
size (Node es) = sum $ map (size . snd) es

count :: String -> SuffixTree -> Int
count _ Leaf = 0
count _ (Node []) = 0
count str (Node ((pref, t) : es))
    -- if our search str is equal to the
    -- current edge, the occurences are simply
    -- the size of the sub-tree the edge leads to
    | pref == str = size t
    -- if the edge is a prefix of our query
    -- string, we remove the prefix from the
    -- query string and search for the remainder
    -- in the sub-tree
    --
    -- not checking pref /= "" causes a
    -- nasty bug
    | pref /= "" && pref `isPrefixOf` str =
        count (unsafeStripPrefix pref str) t
    -- if our query string is a prefix of
    -- the edge, but not equal to it, then
    -- it only occurs in that case.
    | str `isPrefixOf` pref = 1
    -- otherwise we had no match and continue
    -- searching the remaining edges
    | otherwise = count str (Node es)

strandHealth :: Vector Gene -> Strand -> Int
strandHealth gs0 strand =
    foldl' (\acc g -> acc + countGene g) 0 gs
    where
        gs = selectGenes gs0 strand
        t = suffixTree strand.d.inner
        countGene g = g.health * count g.str t

minMax :: Ord b => (a -> b) -> [a] -> (b, b)
minMax _ [] = error "can't minMax empty list"
minMax f (x:xs) = execState (go xs) initState
    where
        initState = (fx, fx)
        fx = f x
        go [] = pure () 
        go (y:ys) = do
            (mn0, mx0) <- get
            let res = f y
                mn1 = min mn0 res
                mx1 = max mx0 res
            put (mn1, mx1)
            go ys

minMaxHealths :: [Strand] -> Vector Gene -> (Int, Int)
minMaxHealths strands genes = minMax (strandHealth genes) strands

main :: IO ()
main = do
    _n <- getLine
    gs <- words <$> getLine
    hs <- (map read . words) <$> getLine

    let genes0 = [fromJust $ mkGene g h | (g, h) <- zip gs hs]
        genes = V.fromList genes0

    nstrands <- read <$> getLine
    strands <- replicateM nstrands do
        [n, m, str] <- words <$> getLine
        pure $ fromJust $ mkStrand (read n) (read m) str

    let (mn, mx) = minMaxHealths strands genes
        output = show mn <> " " <> show mx

    putStrLn output
