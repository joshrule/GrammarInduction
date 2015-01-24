
module Data where

import Data.List (intercalate, permutations)
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle 

import Data.Map (Map, (!))
import qualified Data.Map as Map

import LPN.Number

counts_first_10 = [40000, 10000, 4444, 2500, 1600, 1111, 816, 625, 493, 400]

-- Phase 1: Numbers 1 to 10
-- For Phase 1, there is no test set.
mkPhase1_Number :: Int -> [String]
mkPhase1_Number count = do
  (i, w) <- zip [1..] first10
  return $ "phase1_train(count(prove('Number_1'-[[" ++ intercalate ", " w ++
    "]]), " ++ show (counts_first_10 ! i) ++ "))."

mkPhase1_Succ :: Int -> [String]
mkPhase1_Succ count = do
  (i, n) <- zip [1..] [1..9]
  let u = first10 !! (n-1)
      v = first10 !! n
  return $ "phase1_train(count(prove('Succ_2'-[["
    ++ intercalate ", " v ++ "], ["
    ++ intercalate ", " u ++ "]]), " ++ show (counts_first_10 ! i) ++ "))."

mkPhase1_More :: Int -> [String]
mkPhase1_More count = do
  n <- [1..9]
  m <- [(n+1)..9]
  let u = first10 !! (n-1)
      v = first10 !! (m-1)
  return $ "phase1_train(count(prove('More_2'-[["
    ++ intercalate ", " v ++ "], ["
    ++ intercalate ", " u ++ "]]), " ++ show count ++ "))."

mkPhase1 :: Int -> [String]
mkPhase1 count = mkPhase1_Number count ++ mkPhase1_Succ count ++ mkPhase1_More count

writePhase1 :: Int -> FilePath -> IO ()
writePhase1 count path = do
  writeFile path $ unlines (mkPhase1 count)

-- Phase 2: Numbers 1 to 100
phase2_number_held_out :: [[[String]]]
phase2_number_held_out = [
  [["eighty", "eight"]],
  [["twenty", "eight"]],
  [["ninety", "six"]],
  [["eighty"]],
  [["sixty", "five"]],
  [["thirty", "two"]],
  [["twenty", "five"]],
  [["fifty", "seven"]],
  [["sixty", "nine"]],
  [["forty"]],
  [["fifty", "one"]],
  [["twenty", "four"]],
  [["fifty", "six"]],
  [["forty", "five"]],
  [["ninety"]],
  [["twenty", "seven"]],
  [["seventy", "two"]],
  [["sixty", "seven"]],
  [["forty", "seven"]],
  [["thirty", "five"]]
  ]

-- | all length two number words made up of lexicon up to 99 that are
-- not actually number words
phase2_number_negative_examples :: [[[String]]]
phase2_number_negative_examples =
  [[[x, y]] | [x] <- first19, [y] <- first19]
  ++ [[[x, y]] | [x] <- first19, y <- tens]
  ++ [[[x, y]] | x <- tens, y <- tens]

-- | list of number words in phase2 that are not in the held out test set
phase2_number_not_held_out :: [[[String]]]
phase2_number_not_held_out = [ [w] | w <- n1to99, not ([w] `elem` phase2_number_held_out)]

phase2_number_test_examples :: [([[String]], String)]
phase2_number_test_examples = zip phase2_number_held_out (repeat "positive")
                       ++ zip phase2_number_negative_examples (repeat "negative")


mkPhase2_Number :: MonadRandom m =>
                 Int -- ^ total number of examples
                 -> Double -- ^ noise probability (i.e. fractional examples that should be false)
                 -> m [String]
mkPhase2_Number n err = do
  mkTrainTest n "phase2" "Number_1"
    phase2_number_not_held_out
    phase2_number_test_examples
    phase2_number_negative_examples
    err

                      
-- Phase 2 Succ
phase2_succ_held_out :: [[[String]]]
phase2_succ_held_out = map switch [
  [["seventy","four"],["seventy","five"]],
  [["thirty","four"],["thirty","five"]],
  [["thirty","nine"],["forty"]],
  [["eighty","nine"],["ninety"]],
  [["ninety","two"],["ninety","three"]],
  [["fifty","seven"],["fifty","eight"]],
  [["thirty","seven"],["thirty","eight"]],
  [["eighty","three"],["eighty","four"]],
  [["forty","one"],["forty","two"]],
  [["ninety","seven"],["ninety","eight"]],
  [["eighty","four"],["eighty","five"]],
  [["eighty","five"],["eighty","six"]],
  [["sixty","four"],["sixty","five"]],
  [["sixty","three"],["sixty","four"]],
  [["forty","three"],["forty","four"]],
  [["seventy","six"],["seventy","seven"]],
  [["thirty","six"],["thirty","seven"]],
  [["ninety","one"],["ninety","two"]],
  [["thirty"],["thirty","one"]],
  [["sixty","one"],["sixty","two"]]
  ]
  where switch [a, b] = [b, a]

phase2_succ_negative_examples = 
  [[n1to99 !! i, n1to99 !! j] | i <- [0..97], j <- [0..97], not (i == j+1)]

phase2_succ_not_held_out = [ [v, w] | (w, v) <- zip (init n1to99) (tail n1to99),
                             not ([v, w] `elem` phase2_succ_held_out)]

phase2_succ_test_examples :: MonadRandom m =>
                             Int -- ^ number of negative examples
                          -> m [([[String]], String)]
phase2_succ_test_examples nNeg = do
  let pos = zip phase2_succ_held_out (repeat "positive")
  neg' <- shuffleM phase2_succ_negative_examples
  let neg = zip (take nNeg neg') (repeat "negative")
  return $ pos ++ neg

mkPhase2_Succ :: MonadRandom m =>
                 Int -- ^ total number of examples
                 -> Double -- ^ noise probability (i.e. fractional examples that should be false)
                 -> m [String]
mkPhase2_Succ n err = do
  testSet <- phase2_succ_test_examples 20
  mkTrainTest n "phase2" "Succ_2"
                      phase2_succ_not_held_out
                      testSet
                      phase2_succ_negative_examples
                      err

mkPhase2 :: MonadRandom m
            => Int -- ^ number of Number examples
            -> Int -- ^ number of Succ examples
            -> Double -- ^ Error probability
            -> m [String]
mkPhase2 nNumber nSucc err =do
  numbers <- mkPhase2_Number nNumber err
  succs <- mkPhase2_Succ nSucc err
  return $ numbers  ++ succs

writePhase2 :: Int -> Int -> Double -> FilePath -> IO ()
writePhase2 nNumber nSucc err path = do
  xs <- mkPhase2 nNumber nSucc err 
  writeFile path (unlines xs)

----------------------------------------------------------------
-- Phase 3:
phase3_number_held_out :: [[[String]]]
phase3_number_held_out = [
  [["nine","hundred","ninety","three"]],
  [["three","hundred","eight"]],
  [["one","hundred","fifteen"]],
  [["two","hundred","eighty","one"]],
  [["two","hundred","eighty","four"]],
  [["three","hundred","eighty","nine"]],
  [["one","hundred","twenty","four"]],
  [["five","hundred","fifty","three"]],
  [["five","hundred","eighty","eight"]],
  [["nine","hundred","thirty","two"]],
  [["three","hundred"]],
  [["six","hundred","nineteen"]],
  [["one","hundred","forty","one"]],
  [["seven","hundred","one"]],
  [["two","hundred","ninety","seven"]],
  [["six","hundred","eleven"]],
  [["eight","hundred","sixty","two"]],
  [["six","hundred","fifteen"]],
  [["four","hundred","fifty","two"]],
  [["six","hundred","thirty","two"]]
  ]

phase3_number_not_held_out :: [[[String]]]
phase3_number_not_held_out = [[w] | w <- n1to999, not ([w] `elem` phase3_number_held_out)]

phase3_number_all_negative_examples :: [[[String]]]
phase3_number_all_negative_examples = filter (\[x] -> not (x `elem` n1to999)) perms
    where perms = map (\x -> [x]) $ concat $ map (permutations) n1to999


phase3_number_negative_examples :: MonadRandom m =>
                                   Int -- ^ number of examples
                                   -> m [[[String]]]
phase3_number_negative_examples n = shuffleM negs >>= \xs -> return $ take n xs
  where negs = phase3_number_all_negative_examples

phase3_number_test_examples :: MonadRandom m =>
                               Int -- ^ number of negative examples
                            -> m [([[String]], String)]
phase3_number_test_examples n = do
  let pos = zip phase3_number_held_out (repeat "positive")
  neg_exs <- phase3_number_negative_examples n
  let neg = zip neg_exs (repeat "negative")
  return $ pos ++ neg
  

mkPhase3_Number :: MonadRandom m =>
                 Int -- ^ total number of training examples
                 -> Int -- ^ number of negative test examples
                 -> Double -- ^ noise probability (i.e. fractional examples that should be false)
                 -> m [String]
mkPhase3_Number n m err = do
  test_exs <- phase3_number_test_examples m
  out <- mkTrainTest n "phase3" "Number_1"
         phase3_number_not_held_out
         test_exs
         phase3_number_all_negative_examples
         err
  return out
  

mkPhase3 :: MonadRandom m =>
            Int -- ^ nNumber examples
         -> Int -- ^ number of negative Number test examples
         -> Double -- ^ err percentage
         -> m [String]
mkPhase3 nNumber nNumberNegTest err = do
  numbers <- mkPhase3_Number nNumber nNumberNegTest err
  return numbers



writePhase3 :: Int -- ^ nNumber examples
            -> Int -- ^ number of negative Number test examples
            -> Double -- ^ err percentage
            -> FilePath -> IO ()
writePhase3 nNumber nNumberNegTest err path = do
  xs <- mkPhase3 nNumber nNumberNegTest err
  writeFile path (unlines xs)

  


----------------------------------------------------------------
mkPrismData :: Int -- ^ Count
            -> String -- ^ Predicate
            -> [[String]] -- ^ Arguments
            -> String
mkPrismData count pred args = "count(prove('" ++ pred ++ "'-"
                              ++ filter (/= '\"') (show args) ++ "), " ++ show count ++ ")"
                              
mkTrainTest :: MonadRandom m =>
               Int -- ^ total number of training example
            -> String -- ^ phase name
            -> String -- ^ Predicate Name
            -> [[[String]]] -- ^ Positive Training examples
            -> [([[String]], String)] -- ^ TestSet examples
            -> [[[String]]] -- ^ Negative examples
            -> Double -- ^ noise probability (i.e. fractional examples that should be false)
            -> m [String]
mkTrainTest n phaseName pred trainSet testSet negativeSet err = do
  ss <- sampleExs [] 0
  let exWithCounts = count ss
  let train = do
        (ex, c) <- exWithCounts
        return $ phaseName ++ "_train(" ++ mkPrismData c pred ex  ++ ")."
  let test = do
        (ex, label) <- testSet
        return $ phaseName ++ "_test(" ++ mkPrismData 1 pred ex ++ ", " ++ label ++ ")."
  return $ train ++ test
  where 
        sampleExs acc m | m < n = do
          r <- getRandomR (0, 1 :: Double)
          if r < err
            then do ex <- sampleIncorrect
                    sampleExs (ex:acc) (m + 1)
            else do ex <- sampleCorrect
                    sampleExs (ex:acc) (m + 1)
        sampleExs acc _ = return acc
        randomElem xs = do r <- getRandomR (0, length xs-1)
                           return $ xs !! r
        sampleIncorrect :: MonadRandom m => m [[String]]                           
        sampleIncorrect = randomElem negativeSet
        sampleCorrect :: MonadRandom m => m [[String]]
        sampleCorrect = randomElem trainSet

        count :: (Ord a, Eq a) => [a] -> [(a, Int)]
        count xs = go Map.empty xs
           where go mp [] = Map.toList mp
                 go mp (x:xs) = case Map.lookup x mp of
                   Nothing -> go (Map.insert x 1 mp) xs
                   Just y -> go (Map.insert x (y + 1) mp) xs



main = do
  undefined
