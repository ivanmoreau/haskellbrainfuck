module Main (main) where


import Data.Maybe (isJust)
import Data.List (elemIndex)
type Tape = ([Int], Int)
-- type Instructions = Token

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
    | isJust (elemIndex newIndex ys) = runner x (ys, newIndex)
    | otherwise = runner x (ys ++ replicate (newIndex - lastIndex) 0, newIndex)
        where   newIndex = pos + t
                lastIndex = length ys - 1

moveLeft :: Instructions -> Tape -> Int -> String
moveLeft x y 0 = runner x y
moveLeft x (ys, pos) t
    | isJust (elemIndex newIndex ys) = runner x (ys, newIndex)
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

tokenizer :: String -> [Token] -> [Token]
tokenizer ('>':xs) ip = let (tok, str) = howChars ('>':xs) (isChar '>')
    in (TRight tok : ip)
tokenizer ('<':xs) ip = let (tok, str) = howChars ('<':xs) (isChar '<')
    in (TLeft tok : ip)
tokenizer ('+':xs) ip = let (tok, str) = howChars ('+':xs) (isChar '+')
    in (TPlus tok : ip)
tokenizer ('-':xs) ip = let (tok, str) = howChars ('-':xs) (isChar '-')
    in (TMinus tok : ip)
tokenizer ('.':xs) ip = let (tok, str) = howChars ('.':xs) (isChar '.')
    in (TPrint tok : ip)
tokenizer (',':xs) ip = let (tok, str) = howChars (',':xs) (isChar ',')
    in (TRead tok : ip)
tokenizer ('[':xs) ip = TFirst : ip
tokenizer (']':xs) ip = TLast : ip
tokenizer "" ip = ip
tokenizer (_:xs) ip = tokenizer xs ip
-- tokenizer _ = error "Oh, fiddlesticks! What now?"

runner :: Instructions -> Tape -> String
runner ((TRight t):xs, pos) y = moveRight (TRight t:xs, pos + 1) y t
runner ((TLeft t):xs, pos) y = moveLeft (TLeft t:xs, pos + 1) y t
runner ((TPlus t):xs, pos) y = plus (TPlus t:xs, pos + 1) y t
runner ((TMinus t):xs, pos) y = minus (TMinus t:xs, pos + 1) y t
runner ((TPrint t):xs, pos) y = printC (TPrint t:xs, pos + 1) y t
-- runner ((TRead t):xs, pos) y = moveRight (TRead t:xs, pos + 1) y t
runner (TFirst:xs, pos) y = loopInit (TFirst:xs, pos + 1) y
runner (TLast:xs, pos) y = loopEnd (TLast:xs, pos + 1) y
runner _ _ = error "Oh, fiddlesticks! What now?"

machine :: String -> String
machine "" = error "Oh, fiddlesticks! What now?"
machine x = runner (tokenizer x [], 0) ([0], 0)

-- run :: String -> String
-- run (t:ts) = machine ([], t, ts) ([], 0, [])
-- run [] = ""

main :: IO ()
main = putStrLn (machine "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")