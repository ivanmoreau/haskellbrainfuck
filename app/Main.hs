module Main (main) where

import Data.Maybe (isJust, fromJust)
import GHC.IO.Handle ( BufferMode(NoBuffering), hSetBuffering )
import System.IO (stdin)
type Tape = ([Int], Int)

(>!) :: [a] -> Int -> Maybe a
xs >! i
    | (i + 1) > length xs || (i < 0) = Nothing
    | otherwise = Just (xs !! i)

data Token =
    TLeft    { times :: Int } |
    TRight   { times :: Int } |
    TPlus    { times :: Int } |
    TMinus   { times :: Int } |
    TPrint   { times :: Int } |
    TRead    { times :: Int } |
    TFirst { invIndex :: Int }|
    TLast  { invIndex :: Int }
    deriving (Eq, Show)

type Instructions = ([Token], Int)

moveRight :: Instructions -> Tape -> Int -> IO String
moveRight x y 0 = runner x y
moveRight x (ys, pos) t
    | isJust (ys >! newIndex) = runner x (ys, newIndex)
    | otherwise = runner x (ys ++ replicate (newIndex - lastIndex) 0, newIndex)
        where   newIndex = pos + t
                lastIndex = length ys - 1

moveLeft :: Instructions -> Tape -> Int -> IO String
moveLeft x y 0 = runner x y
moveLeft x (ys, pos) t
    | isJust (ys >! newIndex) = runner x (ys, newIndex)
    | otherwise = error "index < -1"
        where   newIndex = pos - t

plus :: Instructions -> Tape -> Int -> IO String
plus x y 0 = runner x y
plus x (ys, pos) t
    | isJust result = runner x (take pos ys ++ (fromJust result + t) : drop (pos + 1) ys, pos)
    | otherwise = error "Evil index."
    where result = ys >! pos

minus :: Instructions -> Tape -> Int -> IO String
minus x y 0 = runner x y
minus x (ys, pos) t
    | isJust result = runner x (take pos ys ++ (fromJust result - t) : drop (pos + 1) ys, pos)
    | otherwise = error "Evil index."
    where result = ys >! pos

loopInit :: Instructions -> Tape -> Int -> IO String
loopInit (xs, x) (ys, pos) t
    | isJust result = case fromJust result of
        0 -> runner (xs, t) (ys, pos)
        _ -> runner (xs, x) (ys, pos)
    | otherwise = error "Evil index."
    where result = ys >! pos

loopEnd :: Instructions -> Tape -> Int -> IO String
loopEnd (xs, x) (ys, pos) t
    | isJust result = case fromJust result of
        0 -> runner (xs, x) (ys, pos)
        _ -> runner (xs, t) (ys, pos)
    | otherwise = error "Evil index."
    where result = ys >! pos

printC :: Instructions -> Tape -> Int -> IO String
printC f t 0 = runner f t
printC f (xs, pos) c = do
        putChar (toEnum (xs !! pos) :: Char )
        printC f (xs, pos) (c - 1)

readC :: Instructions -> Tape -> Int -> IO String
readC x y 0 = runner x y
readC x (ys, pos) c = do
    hSetBuffering stdin NoBuffering
    input <- getChar
    let char = fromEnum ( input) in
        if isJust result then
            readC x (take pos ys
            ++ (fromJust result + char) : drop (pos + 1) ys, pos) (c - 1)
        else error "Evil index."
    where result = ys >! pos

isChar :: Char -> (Char -> Bool)
isChar x = f where
    f y = x == y

howChars :: String -> (Char -> Bool) -> (Int, String)
howChars "" _ = (0, "")
howChars (x:xs) f
    | f x = let (a, b) = howChars xs f in (a + 1, b)
    | otherwise = (0, x:xs)

tokenizer :: String -> Int -> [Int] -> ([Token], [Int])
tokenizer ('>':xs) t q = let (tok, str) = howChars ('>':xs) (isChar '>')
    in let (tokens, ind) = tokenizer str (t + 1) q in (TRight tok : tokens, ind)
tokenizer ('<':xs) t q = let (tok, str) = howChars ('<':xs) (isChar '<')
    in let (tokens, ind) = tokenizer str (t + 1) q in (TLeft tok : tokens, ind)
tokenizer ('+':xs) t q = let (tok, str) = howChars ('+':xs) (isChar '+')
    in let (tokens, ind) = tokenizer str (t + 1) q in (TPlus tok : tokens, ind)
tokenizer ('-':xs) t q = let (tok, str) = howChars ('-':xs) (isChar '-')
    in let (tokens, ind) = tokenizer str (t + 1) q in (TMinus tok : tokens, ind)
tokenizer ('.':xs) t q = let (tok, str) = howChars ('.':xs) (isChar '.')
    in let (tokens, ind) = tokenizer str (t + 1) q in (TPrint tok : tokens, ind)
tokenizer (',':xs) t q = let (tok, str) = howChars (',':xs) (isChar ',')
    in let (tokens, ind) = tokenizer str (t + 1) q in (TRead tok : tokens, ind)
tokenizer ('[':xs) t q = let (tokens, ind) = tokenizer xs (t + 1) (t:q)
    in let (i:rest) = case ind of
            [] -> error "Match error"
            x -> x
    in (TFirst i : tokens, rest)
tokenizer (']':xs) t q =
    let (i:rest) = case q of
            [] -> error "Match error"
            x -> x
    in let (tokens, ind) = tokenizer xs (t + 1) rest
    in (TLast i : tokens, t:ind)
tokenizer "" _ _ = ([], [])
tokenizer (_:xs) t q = tokenizer xs t q

runner :: Instructions -> Tape -> IO String
runner (xs, pos) y = if (pos + 1) > length xs
    then return "" else (case (xs !! pos) of
        TLeft n -> moveLeft (xs, pos + 1) y n
        TRight n -> moveRight (xs, pos + 1) y n
        TPlus n -> plus (xs, pos + 1) y n
        TMinus n -> minus (xs, pos + 1) y n
        TPrint n -> printC (xs, pos + 1) y n
        TRead n -> readC (xs, pos + 1) y n
        TFirst t -> loopInit (xs, pos + 1) y t
        TLast t -> loopEnd (xs, pos + 1) y t)

machine :: String -> IO String
machine "" = error "Oh, fiddlesticks! What now?"
machine x = runner (fst (tokenizer x 0 []), 0) ([0], 0)

main :: IO ()
main = do
    str <- getLine
    val <- machine str
    putStrLn val