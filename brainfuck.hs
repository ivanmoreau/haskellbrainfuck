


type Tape = ([Int], Int, [Int])
type Instructions = ([Char], Char, [Char])

mRIP :: Instructions -> Instructions
mRIP ([], f, h:hs) = ([f], h, hs)
mRIP (fs, f, []) = (fs ++ [f], ' ', [])
mRIP (fs, f, h:hs) = (fs ++ [f], h, hs)

moveRight :: Instructions -> Tape -> String
moveRight xs ([], x, []) = machine xs ([x], 0, [])
moveRight xs ([], x, y:ys) = machine xs ([x], y, ys)
moveRight xs (ys, x, []) = machine xs (ys ++ [x], 0, [])
moveRight xs (ys, x, t:ts) = machine xs (ys ++ [x], t, ts)

moveLeft :: Instructions -> Tape -> String
moveLeft _ ([], _, _) = error "Parse error!"
moveLeft xs (ys, x, []) = machine xs (init ys, last ys, [x])
moveLeft xs (ys, x, ts) = machine xs (init ys, last ys, x : ts)

plus :: Instructions -> Tape -> String
plus xs (ys, x, ts) = machine xs (ys, x + 1, ts)

minus :: Instructions -> Tape -> String
minus xs (ys, x, ts) = machine xs (ys, x - 1, ts)

loopInit :: Instructions -> Tape -> String
loopInit xs (ys, 0, ts) = machine (jumpToEndLoop xs) (ys, 0, ts)
loopInit xs t = machine xs t

loopEnd :: Instructions -> Tape -> String
loopEnd xs (ys, 0, ts) = machine xs (ys, 0, ts)
loopEnd xs t = machine (jumpToStartLoop xs) t

-- Something isn't working in this two functions
jumpToEndLoop :: Instructions -> Instructions
jumpToEndLoop (fs, '[', h:hs) = jumpToEndLoop (jumpToEndLoop (fs ++ ['['], h, hs))
jumpToEndLoop (fs, ']', h:hs) = (fs ++ [']'], h, hs)
jumpToEndLoop (fs, ']', []) = (fs ++ [']'], ' ', [])
jumpToEndLoop (_, _, []) = error "bad syntax"
jumpToEndLoop (fs, x, h:hs) = jumpToEndLoop (fs ++ [x], h, hs)

jumpToStartLoop :: Instructions -> Instructions
jumpToStartLoop (fs, ']', hs) = let (a,b,c) = (jumpToStartLoop (init fs, last fs, ']':hs))
    in jumpToStartLoop (init a, last a, b:c)
jumpToStartLoop (fs, '[', hs) = (fs, '[', hs)
jumpToStartLoop ([], _, _) = error "bad syntax"
jumpToStartLoop (fs, x, hs) = jumpToStartLoop (init fs, last fs, x:hs)

printC :: Instructions -> Tape -> String
printC f (xs, x, ys) = (toEnum x :: Char ) : machine f (xs, x, ys)

machine :: Instructions -> Tape -> String
machine (ys, '>', x:xs) t = moveRight (ys ++ ['>'], x, xs) t
machine (ys, '<', x:xs) t = moveLeft (ys ++ ['<'], x, xs) t
machine (ys, '+', x:xs) t = plus (ys ++ ['+'], x, xs) t
machine (ys, '-', x:xs) t = minus (ys ++ ['-'], x, xs) t
machine (ys, '.', x:xs) t = printC (ys ++ ['.'], x, xs) t
-- machine (ys, ',', x:xs) t = moveRight (ys ++ [','], x, xs) t
machine (ys, '[', x:xs) t = loopInit (ys ++ ['['], x, xs) t
machine (ys, ']', x:xs) t = loopEnd (ys ++ [']'], x, xs) t
machine (_, _, []) _ = ""
machine _ _ = error ""

run :: String -> String
run (t:ts) = machine ([], t, ts) ([], 0, [])
run [] = ""

