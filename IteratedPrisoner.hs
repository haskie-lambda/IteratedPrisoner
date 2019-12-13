{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Random
import Data.List (nubBy, sortBy, intercalate)  
import Data.Bifunctor (bimap)
import Data.Function (on)

main :: IO ()
main =  do
  result <- startSimulation 100 3 100
  print $ show $ stats $ snd result

data Choice = Cooperate | Defect
     deriving (Eq, Show)
type BattleResult = (Choice, Choice)

-------------------------- TYPES  -------------------------- 
type PlayerID = Int
type Payment = Int
type PlayerHist = [((Choice, Payment), (PlayerID, Choice))]
data Player = Player 
                { name :: String -- strategy name 
                , playerID :: PlayerID
                , decide :: PlayerHist -> Choice 
                , getPlayerHist :: PlayerHist
                }

instance Show Player where
  show (Player n p _ o) = 
    "Player { name: '" ++ n ++ "'" ++ 
           ", playerID: " ++ (show p) ++ 
           ", getPlayerHist: " ++ (show o) ++ "'}"

instance Eq Player where
  (Player n _ _ _) == (Player n' _ _ _) = n == n'

instance Eq (Int -> Player) where
  p1 == p2 = (p1 0) == (p2 0)

instance Show (Int -> Player) where
  show p = show $ p 0

type Population      = [Player]

type RandList        = [Int]
type IterationResult = [Player]


-------------------------- DEFINITIONS --------------------------
payment :: BattleResult -> (Int, Int)
payment (Cooperate, Cooperate) = (3,3)
payment (Cooperate, Defect)    = (1,4)
payment (Defect, Cooperate)    = (4,1)
payment (Defect, Defect)       = (2,2)

defector :: Int -> Player
defector n = Player 
                "Defector" 
                n
                (\_ -> Defect)
                []
                
cooperator :: Int -> Player
cooperator n = Player
                "Cooperator"
                n
                (\_ -> Cooperate)
                []


tftDecide :: PlayerHist -> Choice
tftDecide []            = Cooperate
tftDecide ((_,(_,c)):_) = c

tft :: Int -> Player
tft n = Player
                "TFT"
                n
                tftDecide
                []

rageDecide :: PlayerHist -> Choice
rageDecide [] = Cooperate
rageDecide l  = if (elem Defect . map getOpChoice $ l) then Defect else Cooperate 
                  where getOpChoice = snd . snd 

rage :: Int -> Player
rage n = Player
                "Yasha"
                n
                rageDecide
                []

playerTypes :: [Int -> Player]
playerTypes = [defector, cooperator, tft, rage]

generatePopulation :: [(Int->Player, Int)] -> Population
generatePopulation = map (\(i,p) -> p i) . 
                     zip [1..] . 
                     intercalate [] . 
                     map (\(p,n) -> replicate n p)   



-------------------------- GAME LOGIC --------------------------


--              shuffled population  iteration count
runIteration :: Population ->        Int ->           IterationResult 
runIteration p i = undoPairs $ play i (makePairs p)

--      counter   shuffled list of battles         
play :: Int ->    [(Player, Player)] ->    [(Player, Player)]
play 0 h        = h
play i p  
    | i < 0     = p
    | otherwise = play (i-1) $ newPlayers decisions
  where 
    dec p      = decide p $ getPlayerHist p
    decisions  = zip p $ map (bimap dec dec) p   :: [((Player, Player), BattleResult)]
    newPlayers = 
      map (\((p1,p2),cs@(c1,c2)) ->  
             let (a1, a2) = payment cs
             in 
             (p1{getPlayerHist = ((c1, a1),(playerID p2, c2)):(getPlayerHist p1)}
             ,p2{getPlayerHist = ((c2, a2),(playerID p1, c1)):(getPlayerHist p2)}))


--         tournaments  maxIterations  initial Population                      for shuffling   stats for tournaments    with updated histories
runGame :: Int ->       Int ->         ([[(Int->Player, Int)]], Population) -> RandList ->     ([[(Int->Player, Int)]], Population)
runGame _ maxIter res [] = res
runGame 0 maxIter res _  = res
runGame i maxIter res@(hist,ps) rs@(h:t) 
  | i < 0                = res
  | otherwise      = runGame (i-1) maxIter (iterStats:hist, newPopulation) $
                       drop (length iteration) t
  where    
    getPayments = map (snd . fst) . getPlayerHist                                          :: Player -> [Payment]
    iteration   = runIteration (shuffle rs ps) maxIter                                     :: Population
    iterStats   = map (\p -> (p, sum .    
                                 map (sum . getPayments) . 
                                 filter (==(p 0)) $ iteration)
                       ) playerTypes                                                       :: [(Int->Player, Payment)]
    payments    = sum . map snd $ iterStats                                                :: Int
    newPopulationStats = map (\(p, s) -> (p, calcCount s payments (length ps))) iterStats  :: [(Int->Player, Payment)]
    newPopulation      = generatePopulation newPopulationStats                             :: [Player]



startSimulation :: Int -> Int -> Int -> IO ([[(Int->Player, Int)]], Population) 
startSimulation genSize tournaments iterations = do
    g <- getStdGen
    let gen = generatePopulation $ map (\p-> (p, genSize `div` (length playerTypes))) playerTypes
        randList = randoms g
    putStrLn "Simulating Iterated prisoner"
    putStrLn $ "Population " ++ show (stats gen) 
    return $ runGame tournaments iterations ([], gen) randList 
    

-------------------------- AUXILIARY --------------------------


shuffle :: RandList -> [a] -> [a]                                  
shuffle rands xs = let 
  ys = take (length xs) rands
  in
  map fst $ sortBy (compare `on` snd) (zip xs ys)

makePairs :: [a] -> [(a,a)]
makePairs []       = []
makePairs [_]      = []
makePairs (h:h':t) = (h,h'):(makePairs t)

undoPairs :: [(a,a)] -> [a]
undoPairs [] = []
undoPairs ((a,b):t) = [a,b]++(undoPairs t)

stats :: Population -> [(String, Int)]
stats l = map (\p -> (name p, length $ filter (\e->name e == name p) l)) $
                 nubBy (\p1 p2 -> name p1 == name p2) l

-- tries to preserve the calculated amount for each player as close as possible
--           player payout       overall payout    population size
calcCount :: Int ->              Int ->            Int                -> Int
calcCount _ 0 _ = 0
calcCount _ _ 0 = 0
calcCount a g p = let a' = fromIntegral a
                      g' = fromIntegral g
                      p' = fromIntegral p
                      in round $ a'/g'*p' 