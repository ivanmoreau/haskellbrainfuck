


import Data.Maybe (isJust)
import Data.List (elemIndex)
type Tape = ([Int], Int)
-- type Instructions = Token

data Token = 
    TLeft    {left :: (Maybe Token), right :: (Maybe Token), val :: Int } |
    TRight   {left :: (Maybe Token), right :: (Maybe Token), val :: Int } |
    TPlus    {left :: (Maybe Token), right :: (Maybe Token), val :: Int } |
    TMinus   {left :: (Maybe Token), right :: (Maybe Token), val :: Int } |
    TPrint   {left :: (Maybe Token), right :: (Maybe Token), val :: Int } |
    TRead    {left :: (Maybe Token), right :: (Maybe Token), val :: Int } |
    TFirst   {left :: (Maybe Token), right :: (Maybe Token)             } |
    TLast    {left :: (Maybe Token), right :: (Maybe Token)             } |
    TEnd -- Funny, isn't it?
    deriving (Eq, Show)


moveRight :: Token -> Token -> Tape -> Int -> String
moveRight x y 0 = runner x y
moveRight x (ys, pos) t
    | isJust (elemIndex newIndex ys) = runner x (ys, newIndex)
    | otherwise = runner x (ys ++ replicate (newIndex - lastIndex) 0, newIndex)
        where   newIndex = pos + t
                lastIndex = length ys - 1

moveLeft :: Token -> Token -> Tape -> Int -> String
moveLeft x y 0 = runner x y
moveLeft x (ys, pos) t
    | isJust (elemIndex newIndex ys) = runner x (ys, newIndex)
    | otherwise = error "index < -1"
        where   newIndex = pos - t


plus :: Token -> Token -> Tape -> Int -> String
plus x y 0 = runner x y
plus x (y:ys, pos) t = runner x ((y + t):ys, pos)
plus _ ([], _) _ = error "empty list"

minus :: Token -> Token -> Tape -> Int -> String
minus x y 0 = runner x y
minus x (y:ys, pos) t = runner x ((y - t):ys, pos)
minus _ ([], _) _ = error "empty list"

loopInit :: Token -> Token -> Tape -> String
loopInit x (0 : ys, pos) = runner (jumpToEndLoop x) (0 : ys, pos)
loopInit x y = runner t y
    where t = 

-- loopEnd :: Instructions -> Tape -> String


-- Something isn't working in this two functions

-- [ --------> ]
jumpToEndLoop :: Token -> Token
jumpToEndLoop (TLast _ (Just r)) = r
jumpToEndLoop (TFirst _ (Just r)) = jumpToEndLoop r
jumpToEndLoop (TLast _ Nothing) = error "Unexpected"
jumpToEndLoop (TFirst _ Nothing) = error "Unexpected"
jumpToEndLoop TEnd = error "END"
jumpToEndLoop e = case right e of
    Nothing -> error "END"
    Just a -> jumpToEndLoop a


-- [ <-------- ]
jumpToStartLoop :: Token -> Token
jumpToEndLoop (TLast Just l r) = TLast Nothing r

-- printC :: Instructions -> Tape -> String

printC :: Token -> Tape -> Int -> String
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

tokenizer :: String -> Token
tokenizer ('>':xs) = let (tok, str) = howChars ('>':xs) (isChar '>')
    in TRight Nothing (Just (tokenizer str)) tok
tokenizer ('<':xs) = let (tok, str) = howChars ('<':xs) (isChar '<')
    in TLeft Nothing (Just (tokenizer str)) tok
tokenizer ('+':xs) = let (tok, str) = howChars ('+':xs) (isChar '+')
    in TPlus Nothing (Just (tokenizer str)) tok
tokenizer ('-':xs) = let (tok, str) = howChars ('-':xs) (isChar '-')
    in TMinus Nothing (Just (tokenizer str)) tok
tokenizer ('.':xs) = let (tok, str) = howChars ('.':xs) (isChar '.')
    in TPrint Nothing (Just (tokenizer str)) tok
tokenizer (',':xs) = let (tok, str) = howChars (',':xs) (isChar ',')
    in TRead Nothing (Just (tokenizer str)) tok
tokenizer ('[':xs) = TFirst Nothing (Just (tokenizer xs))
tokenizer (']':xs) = TLast Nothing (Just (tokenizer xs))
tokenizer "" = TEnd
tokenizer (_:xs) = tokenizer xs
-- tokenizer _ = error "Oh, fiddlesticks! What now?"

runner :: Token -> Tape -> String
runner (TRight _ x v) y = error "Oh, fiddlesticks! What now?"
runner _ _ = error "Oh, fiddlesticks! What now?"

machine :: String -> String
machine "" = error "Oh, fiddlesticks! What now?"
machine x = runner (tokenizer x) ([0], 0)

-- run :: String -> String
-- run (t:ts) = machine ([], t, ts) ([], 0, [])
-- run [] = ""

main :: IO ()
main = putStrLn (machine "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")