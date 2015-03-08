{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split
import Data.String.Utils(strip, join)

import qualified Data.Map.Strict as Map

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Maybe

data Nucleotide = A | C | G | T | None deriving (Show, Eq)
type BasePair = (Nucleotide, Nucleotide)

data SNP = SNP {
  rsid :: String,
  chromosome :: String,
  position :: Int,
  genotype :: BasePair
} deriving (Show)

main = do
  snps <- readFile "ManuSporny-genome.txt"
  raw <- readFile "rsids.json"
  let filtered = filterLines $ lines snps
      parsed = filter valid $ map processLine filtered
      db = loadSNPedia raw
      matches = withSNPedia parsed db
      featured = filter popular matches
      summary = "Matched " ++ show (length matches) ++ " of " ++ show (length parsed) ++ " SNP's (" ++ show (percent (length matches) (length parsed)) ++ "%)"
  putStrLn summary
  putStrLn $ map (\b -> show b ++ " = " ++ countNucleotides b parsed) [A G C T]
  print $ map toUrl featured

countNucleotides :: Nucleotide -> [SNP] -> Int
countNucleotides n snps = length $ filter (nucleotideMatch n) snps

nucleotideMatch :: Nucleotide -> SNP -> Bool
nucleotideMatch n snp = left == n || right == n
                        where (left, right) = genotype snp

-- Next:
-- * Filter matches by confidence?
-- * Table UI for interesting stats
-- * Better invalid nucleotide error handling

filterLines :: [String] -> [String]
filterLines lines = filter (\x -> head x /= '#') lines

toBasePair :: [Char] -> BasePair
toBasePair [l, r] = (toNucleotide l, toNucleotide r)
toBasePair [l] = (toNucleotide l, None)
toBasePair _ = (None, None)

toUrl :: SNP -> String
toUrl bp = "http://snpedia.com/index.php/" ++ rsid bp
           ++ "(" ++ show left ++ ";" ++ show right ++ ")"
           where (left, right) = genotype bp

toNucleotide 'A' = A
toNucleotide 'C' = C
toNucleotide 'G' = G
toNucleotide 'T' = T
toNucleotide _ = None

processLine line = SNP rsid chromosome position genotype
  where
    parts = splitOn "\t" line
    rsid = parts !! 0
    chromosome = parts !! 1
    position = read $ parts !! 2 :: Int
    genotype = toBasePair $ strip $ parts !! 3

valid x =
  left /= None || right /= None
  where (left, right) = genotype x

-- SNPedia

type DB = Map.Map String String
loadSNPedia raw = fromJust hash
  where bytestring = BS.pack raw
        hash = decode bytestring :: Maybe DB

withSNPedia :: [SNP] -> DB -> [SNP]
withSNPedia snps db = filter (\x -> Map.member (rsid x) db) snps

popular :: SNP -> Bool
popular snp = elem (rsid snp) pop
        where pop = ["rs10"]

-- Utility

percent :: Int -> Int -> Float
percent x y = 100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float
