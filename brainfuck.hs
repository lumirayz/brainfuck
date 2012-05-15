import Control.Monad
import Control.Monad.State

import Data.Char

data Tape = Tape [Int] Int [Int] deriving (Show)

data Brainfuck = Brainfuck
	{ bfTape :: Tape
	, bfBraces :: [String]
	}

type BrainfuckM = StateT Brainfuck IO

getTapeValue :: Tape -> Int
getTapeValue (Tape ls v rs) = v

setTapeValue :: Int -> Tape -> Tape
setTapeValue v (Tape ls _ rs) = Tape ls v rs

modifyTapeValue :: (Int -> Int) -> Tape -> Tape
modifyTapeValue f (Tape ls v rs) = Tape ls (f v) rs

moveRight :: Tape -> Tape
moveRight (Tape ls v (r:rs)) = Tape (v:ls) r rs
moveRight (Tape ls v []) = Tape (v:ls) 0 []

moveLeft :: Tape -> Tape
moveLeft (Tape (l:ls) v rs) = Tape ls l (v:rs)
moveLeft (Tape [] v rs) = Tape [] 0 (v:rs)

currentValue :: BrainfuckM Int
currentValue = get >>= return . getTapeValue . bfTape

modifyTape :: (Tape -> Tape) -> BrainfuckM ()
modifyTape f = modify (\b -> b {bfTape = f (bfTape b)})

runString :: String -> BrainfuckM ()
runString ('>':s) = modifyTape moveRight >> runString s
runString ('<':s) = modifyTape moveLeft >> runString s
runString ('+':s) = modifyTape (modifyTapeValue (+ 1)) >> runString s
runString ('-':s) = modifyTape (modifyTapeValue (subtract 1)) >> runString s
runString ('[':s) = do
	v <- currentValue
	if v /= 0
	then modify (\b -> b {bfBraces = s : bfBraces b}) >> runString s
	else skipToBrace s
runString (']':s) = do
	v <- currentValue
	if v == 0
	then runString s
	else get >>= runString . head . bfBraces
runString ('.':s) = do
	v <- currentValue
	liftIO $ putChar (chr v)
	runString s
runString (',':s) = do
	c <- liftIO getChar
	modifyTape (setTapeValue (ord c))
	runString s
runString (_:s) = runString s
runString "" = return ()

skipToBrace :: String -> BrainfuckM ()
skipToBrace (']':xs) = runString xs
skipToBrace (_:xs) = skipToBrace xs
skipToBrace [] = runString []

run :: String -> IO ()
run s = runStateT (runString s) (Brainfuck (Tape [] 0 []) []) >> return ()
