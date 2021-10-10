module Main (main) where


import Data.Maybe (isJust, fromJust)
import Data.List (elemIndex)
import Control.Exception.Base (tryJust)
type Tape = ([Int], Int)
-- type Instructions = Token

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

moveRight :: Instructions -> Tape -> Int -> String
moveRight x y 0 = runner x y
moveRight x (ys, pos) t
    | newIndex > 6 = error ("Max index should be 6 for Hello World, but we have: " ++ show newIndex)
    | isJust (ys >! newIndex) = runner x (ys, newIndex)
    | otherwise = runner x (ys ++ replicate (newIndex - lastIndex) 0, newIndex)
        where   newIndex = pos + t
                lastIndex = length ys - 1

moveLeft :: Instructions -> Tape -> Int -> String
moveLeft x y 0 = runner x y
moveLeft x (ys, pos) t
    | isJust (ys >! newIndex) = runner x (ys, newIndex)
    | otherwise = error "index < -1"
        where   newIndex = pos - t


plus :: Instructions -> Tape -> Int -> String
plus x y 0 = runner x y
--plus x (y:ys, pos) t = runner x ((y + t):ys, pos)
plus x (ys, pos) t
    | isJust result = runner x (take pos ys ++ (fromJust result + t) : drop (pos + 1) ys, pos)
    | otherwise = error "Evil index."
    where result = ys >! pos
--plus _ ([], _) _ = error "Unreachable case: empty list."

minus :: Instructions -> Tape -> Int -> String
minus x y 0 = runner x y
--minus x (y:ys, pos) t = runner x ((y - t):ys, pos)
minus x (ys, pos) t
    | isJust result = runner x (take pos ys ++ (fromJust result - t) : drop (pos + 1) ys, pos)
    | otherwise = error "Evil index."
    where result = ys >! pos
--minus _ ([], _) _ = error "Unreachable case: empty list."

loopInit :: Instructions -> Tape -> Int -> String
-- loopInit (xs, _) (0 : ys, pos) t = runner (xs, t) (0 : ys, pos)
loopInit (xs, x) (ys, pos) t
    | isJust result = case fromJust result of
        0 -> runner (xs, t) (ys, pos)
        _ -> runner (xs, x) (ys, pos)
    | otherwise = error "Evil index."
    where result = ys >! pos
-- loopInit x y _ = runner x y

loopEnd :: Instructions -> Tape -> Int -> String
-- loopEnd x (0 : ys, pos) _ = runner x (0 : ys, pos)
loopEnd (xs, x) (ys, pos) t
    | isJust result = case fromJust result of
        0 -> runner (xs, x) (ys, pos)
        _ -> runner (xs, t) (ys, pos)
    | otherwise = error "Evil index."
    where result = ys >! pos
-- loopEnd (xs, _) y t = runner (xs, t) y

-- Something isn't working in this two functions

-- [ --------> ]
-- jumpToEndLoop :: Instructions -> Instructions
-- jumpToEndLoop (TLast t:xs, pos) = (TLast t:xs, pos + 1)
-- jumpToEndLoop (TFirst t:xs, pos) = jumpToEndLoop (jumpToEndLoop (TFirst t:xs, pos + 1))
-- jumpToEndLoop (x:xs, pos) = jumpToEndLoop (x:xs, pos + 1)
-- jumpToEndLoop ([], _) = error "EOF: \"]\" was expected."


-- [ <-------- ]
-- jumpToStartLoop :: Instructions -> Instructions
-- jumpToStartLoop (TFirst t:xs, pos) = (TFirst t:xs, pos + 1)
-- jumpToStartLoop (TLast t:xs, pos) = let (a, b) = jumpToStartLoop (TLast t:xs, pos + 1)
--     in jumpToStartLoop (a, b - 2)
-- jumpToStartLoop (x:xs, pos) = jumpToEndLoop (x:xs, pos - 1)
-- jumpToStartLoop ([], _) = error "SOF: \"[\" was expected."

-- printC :: Instructions -> Tape -> String

printC :: Instructions -> Tape -> Int -> String
--printC f ([0], 0) c =  outchars ++ runner f ([0], 0)
--    where outchars = replicate c (toEnum ([0] !! 0) :: Char )
--printC f (xs, pos) c = error (show (xs, pos))
--printC (i, ip) (xs, pos) c = concat (replicate c (" ip: " ++ show (i !! ip) ++ ", " ++ show pos ++ ":" ++ show (xs !! pos) ++ " ")) ++ runner (i, ip) (xs, pos)
printC f (xs, pos) c =  outchars ++ runner f (xs, pos)
    where outchars = replicate c (toEnum (xs !! pos) :: Char )

isChar :: Char -> (Char -> Bool)
isChar x = f where
    f y = x == y

howChars :: String -> (Char -> Bool) -> (Int, String)
howChars "" _ = (0, "")
howChars (x:xs) f
    | f x = let (a, b) = howChars xs f in (a + 1, b)
    | otherwise = (0, x:xs)


-- WORKING, DON'T TOUCH. 🤷🏼‍♀️
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
-- tokenizer _ = error "Oh, fiddlesticks! What now?"

-- Probably working. 🤷🏼‍♀️
runner :: Instructions -> Tape -> String
runner (xs, pos) y = if (pos + 1) > length xs
    then "" else (case (xs !! pos) of
        TLeft n -> moveLeft (xs, pos + 1) y n
        TRight n -> moveRight (xs, pos + 1) y n
        TPlus n -> plus (xs, pos + 1) y n
        TMinus n -> minus (xs, pos + 1) y n
        TPrint n -> printC (xs, pos + 1) y n
        TRead _ -> error "unimplemented"
        TFirst t -> loopInit (xs, pos + 1) y t
        TLast t -> loopEnd (xs, pos + 1) y t)
-- runner _ _ = error "Oh, fiddlesticks! What now?"

-- WORKING, DON'T TOUCH. 🤷🏼‍♀️
machine :: String -> String
machine "" = error "Oh, fiddlesticks! What now?"
machine x = runner (fst (tokenizer x 0 []), 0) ([0], 0)

-- run :: String -> String
-- run (t:ts) = machine ([], t, ts) ([], 0, [])
-- run [] = ""

--debugTokens :: [Token] -> Int -> String
--debugTokens (x:xs) t = (show t) ++ ": " ++ (show x) ++ "     " ++ debugTokens xs (t + 1)
--debugTokens [] _ = "    "

-- WORKING, DON'T TOUCH. 🤷🏼‍♀️
main :: IO ()
main = putStrLn (machine "++++++++.[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")