{-# LANGUAGE FlexibleInstances #-}
module Lib where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import System.Random

minFingers = 0 :: Int
maxFingers = 5 :: Int
playTill = 3 :: Int

data GameState = 
  GameState { 
    player1 :: Int, 
    player2 :: Int,
    gen :: IO StdGen
  }

putPlayer1 :: Int -> Morra ()
putPlayer1 score = do
  gameState <- get
  put (gameState { player1 = score })

putPlayer2 :: Int -> Morra ()
putPlayer2 score = do
  gameState <- get
  put (gameState { player2 = score })

putGen :: IO StdGen -> Morra ()
putGen someGen = do
  gameState <- get
  put (gameState { gen = someGen })

incrementPlayer1 :: Morra ()
incrementPlayer1 = do
  gameState <- get
  putPlayer1 $ (player1 gameState) + 1

incrementPlayer2 :: Morra ()
incrementPlayer2 = do
  gameState <- get
  putPlayer2 $ (player2 gameState) + 1

type Morra = StateT GameState IO
data Player = Player { nextMove :: Morra Move, name :: String }
data Move = Move { fingers :: Int, guess :: Int }

instance Show GameState where
  show (GameState p1 p2 _) = 
    "Score: \nPlayer 1: " ++ (show p1) ++ "\nPlayer 2: " ++ (show p2)

initGameState :: GameState
initGameState = GameState 0 0 getStdGen

nextAIMove :: Morra Move
nextAIMove = do
  gameState <- get
  g <- liftIO (gen gameState)
  let (fingers, g') = randomR (minFingers, maxFingers) g
  let (guess, g'') = randomR (minFingers, maxFingers) g'
  putGen (return g'')
  return $ Move { fingers = fingers, guess = guess }

nextPlayerMove :: String -> Morra Move
nextPlayerMove name = do
  liftIO $ putStrLn $ name ++ "'s turn:"
  liftIO $ putStrLn "Guess the sum: "
  guess <- fmap (read :: String -> Int) $ liftIO getLine
  liftIO $ putStrLn "How many fingers will you show? (0-5): "
  fingers <- fmap (read :: String -> Int) $ liftIO getLine
  return $ Move { fingers = fingers, guess = guess }

guessedCorrectly :: Move -> [Move] -> Bool
guessedCorrectly (Move _ guess) moves =
  guess == (sum $ fmap fingers moves)

morra' :: Player -> Player -> Morra ()
morra' p1 p2 = do
  gameState <- get
  p1Move <- nextMove p1

  liftIO $ putStrLn $ (name p1) ++ " showed " ++ (show $ fingers p1Move) ++ " and guessed " 
    ++ (show $ guess p1Move) ++ "."

  p2Move <- nextMove p2

  liftIO $ putStrLn $ (name p2) ++ " showed " ++ (show $ fingers p2Move) ++ " and guessed " 
    ++ (show $ guess p2Move) ++ "."

  let moves = [p1Move, p2Move]

  if guessedCorrectly p1Move moves && guessedCorrectly p2Move moves
  then
    liftIO $ putStrLn $ "Both players guessed correctly. No points awarded."
  else if guessedCorrectly p1Move moves
  then do
    liftIO $ putStrLn $ (name p1) ++ " guessed correctly."
    incrementPlayer1
  else if guessedCorrectly p2Move moves
  then do
    liftIO $ putStrLn $ (name p2) ++ " guessed correctly."
    incrementPlayer2
  else
    liftIO $ putStrLn $ "Nobody guessed correctly."

  gameState' <- get 

  if player1 gameState' == playTill
  then
    liftIO $ putStrLn $ "Player 1 wins."
  else if player2 gameState' == playTill
  then
    liftIO $ putStrLn $ "Player 2 wins."
  else do
    liftIO $ putStrLn $ show gameState'
    morra' p1 p2

morra :: Morra ()
morra = do
  liftIO $ putStrLn "Please select a mode:\nPVP (1)\nPVC (2)\nCVC (3)"
  mode <- fmap (read :: String -> Int) $ liftIO getLine
  if mode == 1
  then
    morra' 
      (Player { nextMove = nextPlayerMove "Player 1", name = "Player 1" }) 
      (Player { nextMove = nextAIMove, name = "Your opponent" })
  else if mode == 2
  then
    morra' 
      (Player { nextMove = nextPlayerMove "Player 1", name = "Player 1" }) 
      (Player { nextMove = nextPlayerMove "Player 2", name = "Player 2" })
  else
    morra' 
      (Player { nextMove = nextAIMove, name = "AI Player 1" }) 
      (Player { nextMove = nextAIMove, name = "AI Player 2" })

  

