--  File     : Proj2.hs
--  Author   : Duc Hang Giang Vu <duchanggiang@student.unimelb.edu.au>
--  Purpose  : Project 2 - functions for battleship-style guessing game


-- | File-level Documentation:
-- This module assumes the board is 4x8 ('A'-'H') - (1-4). There are 3 ships
-- on the board at 3 distinct Locations. Each guess consists of 3 Locations.
--
-- The goal of this module is to find the guess that hits all 3 ships
-- in the least amount of guesses possible.
--
-- This module export the following functions for the game:
-- 
--    >> toLocation: convert a String to a Location
--    >> fromLocation: convert a Location to a String
--    >> feedback: give feedback on how accurate the guess
--                 is based on the Locations of the ships
--    >> initialGuess: return the first guess
--    >> nextGuess: make next guess given feedback from previous 
--                  guess and GameState
--    >> GameState: store and update the list of potential Locations
--                  of the ships

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char (digitToInt, ord, intToDigit, isAlpha, isDigit)
import Data.List (group, sort )


-- | Location represents a location on the game board,
-- it consists of a Char and an Int.
data Location = Loc Char Int
    deriving (Eq, Show)


-- | GameState is a list of tuples of:
--    >> 1 list of 3 potential ship Locations (target)
--    >> 1 corresponding feedback using previous guess
type GameState = [([Location], (Int, Int, Int))]


--------------------------------- toLocation ---------------------------------

-- | Convert a String to a Maybe Location.
-- If both Char representing the row and column are valid,
-- return a Just Location.
--
-- Otherwise, return Nothing
toLocation :: String -> Maybe Location
toLocation [a, b]
    | validColumn a && validRow b = Just (Loc a (digitToInt b))
toLocation _ = Nothing

-- | Given a Char representing the column of a Location,
-- verify that it is an alphabetical character and is 
-- within range ('A'-'H').
validColumn :: Char -> Bool
validColumn c = isAlpha c && c >= 'A' && c <= 'H'

-- | Given a Char representing the row of a Location,
-- verify that it is a digit character and is within
-- range (1-4).
validRow :: Char -> Bool
validRow n = isDigit n && num >= 1 && num <= 4
    where
        num = digitToInt n


-------------------------------- fromLocation --------------------------------

-- | Convert a Location to a String
fromLocation :: Location -> String
fromLocation (Loc c n) = [c, intToDigit n]


---------------------------------- feedback ----------------------------------

-- | Take a list of 3 ship locations and a guess and return a tuple of 3 Int:
--    >> The number of correctly guessed ship
--    >> The number of guess Location that is 1 away from a ship
--    >> The number of guess Location that is 2 away from a ship
--
-- Note: a guess consists of 3 Location,
--       only the closest ship to any guess Location counts.
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback ships guess = fbHelper ships guess (0,0,0)

-- | Recursive helper function to calculate the minimum distance between each 
-- guess Location and the 3 ships Location, update the resulting feedback 
-- accordingly.
--
-- The feedback is a tuple of 3 Int.
fbHelper :: [Location] -> [Location] -> (Int, Int, Int) -> (Int, Int, Int)
fbHelper _ [] result = result
fbHelper ships (g:gs) (x1, x2, x3)
    | minDistance ships g == 0 = fbHelper ships gs (x1 + 1, x2, x3)
    | minDistance ships g == 1 = fbHelper ships gs (x1, x2 + 1, x3)
    | minDistance ships g == 2 = fbHelper ships gs (x1, x2, x3 + 1)
    | otherwise = fbHelper ships gs (x1, x2, x3)

-- | Find the distance between the guess Location and each of the 3 ship 
-- Location and return the smallest one.
minDistance :: [Location] -> Location -> Int
minDistance ships guess = minimum (map (distance guess) ships)

-- | Return the distance between 2 Location
-- The distance is calculated as specified by the specification.
distance :: Location -> Location -> Int
distance (Loc c1 n1) (Loc c2 n2) = max columnDist rowDist
    where
        columnDist = abs (ord c1 - ord c2)
        rowDist = abs (n1 - n2)



-------------------------------- initialGuess --------------------------------

-- | Return a tuple consisting of the first guess and a GameState.
-- A guess is a list of 3 Location
initialGuess :: ([Location], GameState)
initialGuess = (bestInitialGuess, initialGameState)

-- | Return a hardcoded initial guess that gives the best result.
-- This is just an arbitrary Location. It allow the program to
-- run at a decent speed.
--
-- This might be because the 3 Location here are close together, which means
-- that after the first guess, roughly half of the board is guaranteed
-- to be eliminated.
bestInitialGuess :: [Location]
bestInitialGuess = [Loc 'A' 1, Loc 'B' 1, Loc 'A' 2]

-- | Compute and return the initial GameState based on the first guess.
--
-- GameState is list of a tuple of 2 things:
--    >> a potential target (a target is a list of 3 ship Location)
--    >> a corresponding potential feedback 
--        (calculated using initalGuess)
initialGameState :: GameState
initialGameState = zip potentialTargets potentialFeedbacks
    where
        potentialTargets = squares `choose` 3
        potentialFeedbacks = getPotentialFeedbacks potentialTargets 
                                            bestInitialGuess

-- | Given 1 guess and a list of potential targets, return a list of 
-- potential feedbacks.
getPotentialFeedbacks :: [[Location]] -> [Location] -> [(Int, Int, Int)]
getPotentialFeedbacks targets guess = map (flip feedback guess) targets


-- | List of all the Location on the board
squares :: [Location]
squares =
    [Loc 'A' 1, Loc 'B' 1, Loc 'C' 1, Loc 'D' 1, 
     Loc 'E' 1, Loc 'F' 1, Loc 'G' 1, Loc 'H' 1,
     Loc 'A' 2, Loc 'B' 2, Loc 'C' 2, Loc 'D' 2, 
     Loc 'E' 2, Loc 'F' 2, Loc 'G' 2, Loc 'H' 2,
     Loc 'A' 3, Loc 'B' 3, Loc 'C' 3, Loc 'D' 3, 
     Loc 'E' 3, Loc 'F' 3, Loc 'G' 3, Loc 'H' 3,
     Loc 'A' 4, Loc 'B' 4, Loc 'C' 4, Loc 'D' 4, 
     Loc 'E' 4, Loc 'F' 4, Loc 'G' 4, Loc 'H' 4]


-- | Return all possible ways to choose n elements from a list
choose :: [a] -> Int -> [[a]]
choose _ 0 = [[]]
choose (x:xs) n
    | n > length (x:xs) = [[]]
    | n == length (x:xs) = [x:xs]
    | otherwise = map (x:) (choose xs (n-1)) ++ choose xs n



--------------------------------- nextGuess ----------------------------------


-- | Given a tuple of (previous guess, previous GameState), and a feedback 
-- tuple of 3 Int, return a new tuple of (next guess, next GameState).
--
-- 1. Using the feedback & GameState, eliminate potential targets that has
--    inconsistent feedback with the expected feedback given previous guess.
-- 2. For each of the new guesses, find the expected number of potential 
--    targets remaining. 
-- 3. Pick the guess with the lowest expected value as the next guess.
-- 4. Compute the new list of potential feedbacks for this guess.
-- 5. Pair the new list of feedbacks with the new list of targets as the new
--    GameState. 
-- 6. Return next guess and next GameState.
nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> 
                                            ([Location], GameState)

nextGuess (prevGuess, gameState) actualFeedback = (nextGuess, nextGameState)
    where
        nextGameState = zip newTargets newFeedbacks
        newFeedbacks = getPotentialFeedbacks newTargets nextGuess
        nextGuess = snd (getMinTuple (zip expected newGuesses))
        expected = map (getExpectedValue 
                        . getPotentialFeedbacks newTargets) newGuesses
        newGuesses = newTargets
        newTargets = map fst hasConsistentFeedback
        hasConsistentFeedback = filter (\pair -> snd pair == actualFeedback 
                                        && fst pair /= prevGuess) gameState


-- | Given a list of feedbacks, return the expected number of 
-- remaining potential candidates.
getExpectedValue :: [(Int, Int, Int)] -> Float
getExpectedValue [] = 0
getExpectedValue xs = foldr (\x acc -> x*x / totalLength + acc) 0 countUnique
    where
        countUnique = map (fromIntegral . length) (group (sort xs))
        totalLength = fromIntegral (length xs)


-- | Given list of tuples of (score, guess), return the tuple 
-- with the minimum score.
getMinTuple :: [(Float, [Location])] -> (Float, [Location])
getMinTuple = foldr1 (\x acc -> if fst x < fst acc then x else acc)

