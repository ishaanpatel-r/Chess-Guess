
------------------------------------------------------------------------------
-- Author:   Ishaan Rajeshkumar Patel
--
-- Purpose:  ChessGuess,
--           We implement a guesser-part of a guessing game where,
--           Upto N chess pieces are selected by the tester and
--           The algorithm has to guess them correctly with least guesses,
--           In 10 seconds.
-------------------------------------------------------------------------------


module ChessGuess (initialGuess, nextGuess, GameState) where
-- the module contains 2 main functions: initialGuess, nextGuess.
--             and 10 utility functions: gameZeroState, gameFirstState,
--                   gameRestState, baseGameState, guessResults, 
--                   totalStrength, combinations, similarOverlaps,
--                   baseFirstGuess, padThis.
    

    -- basic imports
    import Data.List
    import Data.Ord
    import qualified Data.Set as Set


    -- PieceSet stores a list of target pieces
    type PieceSet = [String]
    kinds = ["P", "P", "P", "P", "P", "P", "P", "P", 
            "R", "R", "B", "B", "N", "N", "K", "Q"]
    colors = ["B", "W"]
    -- Sample PieceSet : ["BK", "WQ", "WP"]
    --
    --
    -- GameState stores a list of (target guess set, total of guess result)
    type GameState = [(PieceSet, Int)]
    -- Sample GameState :  [(["BK", "WQ", "WP"], 3), ...]
    --
    --
    -- GuessResult is number of correct (Pieces, Kinds, Colors)
    type GuessResult = (Int, Int, Int)
    -- Sample GuessResult for Target : ["BK", "WQ", "WP"]
    --                        Guess : ["BP", "WQ", "WP"]
    --                    => (2, 0, 1)





    --
    -------------------------------
    -- MAIN FUNCTIONS --
    --------------------
    --
    -- initialGuess :: INPUT    => size (of game)
    --              :: OUTPUT   => PieceSet (initial guess), 
    --                            GameState (all possible guesses)
    initialGuess :: Int -> (PieceSet, GameState)
    initialGuess size | size <= 7 = ( -- <= 7 :: for cases where computing is easy
                        --
                        baseFirstGuess size, 
                        -- takes 'size' number of elements from a list of 
                        -- unique (nub) single-colored ("B") chess pieces (++ kind)
                        --
                        baseGameState size
                        -- generates all possible target guesses for 'size' game size
                        -- (refer baseGameState utility-function for more insight)
                        --
                        )
                     | size > 7 = (baseFirstGuess size, [])
                     -- > 7 :: for cases where computing becomes taxing

    -- nextGuess :: INPUT    => previous guess,
    --                          previous list of all possible guesses,
    --                          result of previous guess.
    --          :: OUTPUT   => next best guess,
    --                         cherry-picked possible guesses.
    nextGuess :: (PieceSet, GameState) -> GuessResult -> (PieceSet, GameState)
    nextGuess (pG, 
        pGState) pGResult | length(pGState) > 1 = gameRestState (pG, 
                                pGState) pGResult -- recursive guess function (for all sizes)
                          | length(pGState) == 1 =  gameFirstState (pG, 
                                pGState) pGResult -- generate new gamestate (for size > 7)
                           | length(pGState) == 0 = gameZeroState (pG,
                                pGState) pGResult -- second guess (for size > 7)







    --
    ----------------------------------
    -- UTILITY FUNCTIONS --
    -----------------------




    -- gameZeroState :: INPUT   => initial guess, 
    --                          gamestate,
    --                          initial guess' score.
    --              :: OUTPUT   => returns all whites as second guess,
    --                         and number of blacks in target guess.
    gameZeroState :: (PieceSet, GameState) -> GuessResult -> (PieceSet, GameState)
    gameZeroState (firstGuess, globalGameState) firstGuessResult = (secondGuess, globalGameState)
                    where
                        secondGuess = padThis "W" size [] 
                        -- second guess is all white pieces
                        correctPieces = let (x, _, _) = firstGuessResult in x
                        correctKinds = let (_, y, _) = firstGuessResult in y
                        correctColors = let (_, _, z) = firstGuessResult in z
                        globalGameState = [(firstGuess, 
                            blackCount)] -- pass number of black pieces
                        guessScore = correctPieces + correctKinds + correctColors
                        size = length(firstGuess)
                        blackCount = correctPieces + correctColors




    -- gameFirstState :: INPUT  => second guess set, 
    --                          gamestate,
    --                          second guess' score.
    --              :: OUTPUT   => returns a set of possible guesses,
    --                          constructed on basis of how many blacks
    --                          and whites exist in target set.
    gameFirstState :: (PieceSet, GameState) -> GuessResult -> (PieceSet, GameState)
    gameFirstState (secondGuess, globalGameState) secondGuessResult = (finalFixedGuess, finalGameState)
                    where
                        correctPieces = let (x, _, _) = secondGuessResult in x
                        correctKinds = let (_, y, _) = secondGuessResult in y
                        correctColors = let (_, _, z) = secondGuessResult in z
                        whiteCount = correctPieces + correctColors
                        blackCount = let (_, sameColor) = globalGameState !! 0 in sameColor
                        finalGameState = (Set.toList . Set.fromList) [(k, 0) | -- attach score
                                                     k <- [concat ([b ++ w]) | -- flatten combinations
                                   b <- combinations blackCount ["B" ++ kind | -- generate fixed template
                                   kind <- kinds], w <- combinations whiteCount ["W" ++ kind |
                                   kind <- kinds]]] -- generates fixed length combinations
                                   -- considering black-counts and white-counts determined
                        finalFixedGuess = let (firstPick, _) = finalGameState !! 0 in firstPick
                                    -- provide first guess for recursive guess function




    -- gameRestState :: INPUT    => constructed guess sets, 
    --                          gamestate,
    --                          guess score.
    --                          (iterative now)
    --              :: OUTPUT   => returns a set of possible guesses,
    --                          constructed on basis of how many blacks
    --                          and whites exist in target set.
    gameRestState :: (PieceSet, GameState) -> GuessResult -> (PieceSet, GameState)
    gameRestState (prevGuess, prevGuessState) prevGuessResult = (newGuess, newGameState)
      where newGuess      = fst (maximumBy (comparing snd) [((fst x), (totalStrength 
                            (fst x) newGameState)) | x <- newGameState])
                            -- calculate score equivalence
            newGameState  = [((fst gus), initialScore) | 
                            gus <- prevGuessState, (guessResults prevGuess 
                            (fst gus)) == prevGuessResult]
                            -- cherry-pick according to score equivalence
            initialScore  = 0




    -- guessResults :: INPUT    => target set, guess set
    --              :: OUTPUT   => returns (p, k, c) where 
    --                      p   = number of correct pieces
    --                      k   = number of correct kinds
    --                      c   = number of correct colors
    guessResults :: PieceSet -> PieceSet -> GuessResult
    guessResults target guess = let 
			correctPieces   = length(similarOverlaps guess target)
			correctKinds    = length(similarOverlaps [[i!!1] | 
                                i <- guess] [[j!!1] | 
                                j <- target]) - correctPieces
                            -- all correct kinds `minus` correct pieces
			correctColors   = length(similarOverlaps [[i!!0] | 
                                i <- guess] [[j!!0] | 
                                j <- target]) - correctPieces
                            -- all correct colors `minus` correct pieces
         in (correctPieces, correctKinds, correctColors)




    -- totalStrength  :: INPUT    => guess, gamestate
    --                :: OUTPUT   => first cherry-picked score
    totalStrength :: PieceSet -> GameState -> Int
    totalStrength guess [] = 0
    totalStrength guess (x:xs) = totalScore + (totalStrength guess xs)
      where totalScore = correctPiece + correctKind + correctColor
            (correctPiece,
             correctKind,
             correctColor) = guessResults guess (fst x)




    -- similarOverlaps  :: INPUT   => two lists (x:xs & ys)
    --                  :: OUTPUT  => list containing their intersection
    similarOverlaps :: [String] -> [String] -> [String]
    similarOverlaps [] _ = []
    similarOverlaps _ [] = []
    similarOverlaps (x:xs) ys = if x `elem` ys
                     then x:similarOverlaps xs (delete x ys)
                     else similarOverlaps xs ys




    -- baseGameState  :: INPUT   => game (size)
    --                :: OUTPUT  => all possible combinations 
    --                              fit into gamestate upto game size
    baseGameState :: Int -> GameState
    baseGameState size = (Set.toList . Set.fromList) ([(pieceSet, 0) | 
                            pieceSet <- concat [combinations s [c++k |
                            c <- colors, k <- kinds] | 
                            s <- [0..size]]])




    -- baseGameState  :: INPUT   => game (size)
    --                :: OUTPUT  => all possible combinations 
    --                              fit into gamestate upto game size
    baseFirstGuess :: Int -> PieceSet
    baseFirstGuess size | size <= 7 = take size ["B" ++ kind | 
                                        kind <- nub kinds]
                        | size > 7 = padThis "B" size []




    -- combinations   :: INPUT   => number of combinations to make (n),
    --                              out of the list to make (xs)
    --                :: OUTPUT  => list of all combinations for
    --                              size n
    combinations :: Int -> [a] -> [[a]]
    combinations 0 _ = [[]]
    combinations n xs = [xs !! index : x | 
                            index <- [0..(length xs)-1], 
                            x <- combinations (n - 1) (drop (index + 1) xs) ]




    -- padThis   :: INPUT   => character to pad (col), 
    --                         number of pads (m),
    --                         list to pad (xs).
    --           :: OUTPUT  => padded list.
    padThis :: String -> Int -> [String] -> [String]
    padThis col m xs = ys ++ take (m - length ys) [col ++ k |
                             k <- kinds] where ys = take m xs