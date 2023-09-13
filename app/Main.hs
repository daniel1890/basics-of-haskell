module Main where

import System.IO
import Data.List

main :: IO ()
main = do
    let resultQuadruple = quadruple 5
    print resultQuadruple

    let resultFactorial = factorial 5
    print resultFactorial

    let resultAverage = average [2, 2, 4, 4]
    print resultAverage

    let resultAddOneMap = map (+1) [1, 2, 3, 4]
    print resultAddOneMap

    let oddEvenString = oddEven [1..10]
    print oddEvenString

    linesArray <- readFileToArray "app/day1.txt"
    putStrLn "Contents of the file:"
    mapM_ putStrLn linesArray


-- Simpel voorbeeld van functie definitie syntax
double x = x + x
quadruple x = double (double x)

-- Definitie van een functie die een factorial van een nummer geeft
-- [1..n] syntax is een voorbeeld van de Sequence operator
factorial n = product[1..n]

-- Met de sequence operator kunnen lijsten met een minimum en limiet gemakkelijk met 2 getallen 
-- geinitialiseerd worden
-- Een array van 1 tm 10
--[1..10]
-- Een array van 2 tm 20 met stappen van 2
--[2, 20]

-- Berekenen van een gemiddelde in een gegeven lijst
-- xs staat voor meerdere x's, de input is in dit geval een lijst
average xs = div (sum xs) (length xs)

-- Wanneer een functie een functie als argument heeft, 
-- of als een functie als returnwaarde wordt geleverd
-- spreken we over een "higher order function".

-- Voorbeeld van de map functie
-- Dit is een higher order function die op elk element van de lijst
-- een functie toepast
--map :: (a -> b) -> [a] -> [b]

-- Voorbeeld van list comprehension
-- Wanneer alles binnen een functie binnen blokhaken staat oddEven xs = [...]
-- Dan is dit een list comprehension, alles wat voor de verticale | bar staat is de input
-- wat erna staat is de conditionele expressie
-- In dit voorbeeld wordt bij elk getal tussen 1 tm 10 uitgedrukt of dit een even of oneven getal is
oddEven xs = [show x ++ " = " ++ if odd x then "odd" else "even" | x <- xs]

------------------------------------------------------------------------------ADVENT_OF_CODE_2022_DAY_1----------------------------------------------------------------
-- Helper functie om een file in te lezen waarbij elke line in een array cell gestopt wordt
readFileToArray :: FilePath -> IO [String]
readFileToArray filePath = do
    contents <- readFile filePath
    let linesArray = lines contents
    return linesArray

readUntilNewline :: [String] -> [String] -> [String]
readUntilNewline [] _ = []
readUntilNewline (x:xs) accumulator
    | x == "\n" = accumulator
    | otherwise = readUntilNewline xs (accumulator ++ [x])

--calcSumOffBiggestElf xs = [  | x < xs]