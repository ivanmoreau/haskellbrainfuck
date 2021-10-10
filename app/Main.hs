module Main (main) where


import Data.Maybe (isJust)
import Data.List (elemIndex)
import Control.Exception.Base (tryJust)
type Tape = ([Int], Int)
-- type Instructions = Token

(>!) :: [a] -> Int -> Maybe a
xs >! i 
    | (length xs < i + 2) || (i < 0) = Nothing 
    | otherwise = Just (xs !! i)

data Token =
    TLeft    { val :: Int } |
    TRight   { val :: Int } |
    TPlus    { val :: Int } |
    TMinus   { val :: Int } |
    TPrint   { val :: Int } |
    TRead    { val :: Int } |
    TFirst                  |
    TLast
    deriving (Eq, Show)

type Instructions = ([Token], Int)

moveRight :: Instructions -> Tape -> Int -> String
moveRight x y 0 = runner x y
moveRight x (ys, pos) t
    | newIndex > 6 = error ("Max index should be 6 for Hello World, but we have: " ++ (show newIndex))
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
plus x (y:ys, pos) t = runner x ((y + t):ys, pos)
plus _ ([], _) _ = error "Unreachable case: empty list."

minus :: Instructions -> Tape -> Int -> String
minus x y 0 = runner x y
minus x (y:ys, pos) t = runner x ((y - t):ys, pos)
minus _ ([], _) _ = error "Unreachable case: empty list."

loopInit :: Instructions -> Tape -> String
loopInit x (0 : ys, pos) = runner (jumpToEndLoop x) (0 : ys, pos)
loopInit x y = runner x y

loopEnd :: Instructions -> Tape -> String
loopEnd x (0 : ys, pos) = runner x (0 : ys, pos)
loopEnd x y = runner (jumpToStartLoop x) y

-- Something isn't working in this two functions

-- [ --------> ]
jumpToEndLoop :: Instructions -> Instructions
jumpToEndLoop (TLast:xs, pos) = (TLast:xs, pos + 1)
jumpToEndLoop (TFirst:xs, pos) = jumpToEndLoop (jumpToEndLoop (TFirst:xs, pos + 1))
jumpToEndLoop (x:xs, pos) = jumpToEndLoop (x:xs, pos + 1)
jumpToEndLoop ([], _) = error "EOF: \"]\" was expected."


-- [ <-------- ]
jumpToStartLoop :: Instructions -> Instructions
jumpToStartLoop (TFirst:xs, pos) = (TFirst:xs, pos + 1)
jumpToStartLoop (TLast:xs, pos) = let (a, b) = jumpToStartLoop (TLast:xs, pos + 1)
    in jumpToStartLoop (a, b - 2)
jumpToStartLoop (x:xs, pos) = jumpToEndLoop (x:xs, pos - 1)
jumpToStartLoop ([], _) = error "SOF: \"[\" was expected."

-- printC :: Instructions -> Tape -> String

printC :: Instructions -> Tape -> Int -> String
--printC f ([0], 0) c =  outchars ++ runner f ([0], 0)
--    where outchars = replicate c (toEnum ([0] !! 0) :: Char )
--printC f (xs, pos) c = error (show (xs, pos))
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


-- WORKING, DON'T TOUCH. 🤷🏼‍♀️ (but no, it doesn't)
tokenizer :: String -> [Token]
tokenizer ('>':xs) = let (tok, str) = howChars ('>':xs) (isChar '>')
    in (TRight tok : tokenizer str)
tokenizer ('<':xs) = let (tok, str) = howChars ('<':xs) (isChar '<')
    in (TLeft tok : tokenizer str)
tokenizer ('+':xs) = let (tok, str) = howChars ('+':xs) (isChar '+')
    in (TPlus tok : tokenizer str)
tokenizer ('-':xs) = let (tok, str) = howChars ('-':xs) (isChar '-')
    in (TMinus tok : tokenizer str)
tokenizer ('.':xs) = let (tok, str) = howChars ('.':xs) (isChar '.')
    in (TPrint tok : tokenizer str)
tokenizer (',':xs) = let (tok, str) = howChars (',':xs) (isChar ',')
    in (TRead tok : tokenizer str)
tokenizer ('[':xs)  = TFirst : (tokenizer xs)
    where f (']':xs)  = TLast : tokenizer xs
tokenizer "" = []
tokenizer (_:xs) = tokenizer xs
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
        TRead n -> error "unimplemented"
        TFirst -> loopInit (xs, pos + 1) y
        TLast -> loopEnd (xs, pos + 1) y)
-- runner _ _ = error "Oh, fiddlesticks! What now?"

-- WORKING, DON'T TOUCH. 🤷🏼‍♀️
machine :: String -> String
machine "" = error "Oh, fiddlesticks! What now?"
machine x = runner (tokenizer x, 0) ([0], 0)

-- run :: String -> String
-- run (t:ts) = machine ([], t, ts) ([], 0, [])
-- run [] = ""

-- WORKING, DON'T TOUCH. 🤷🏼‍♀️
main :: IO ()
main = putStrLn (machine "++++++++.[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")