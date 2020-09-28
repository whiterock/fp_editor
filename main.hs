import System.IO
import System.Environment
import System.Directory
import System.Console.Terminal.Size
import Data.List
import Data.Char
import System.Exit
import Control.Monad (when)
import Data.Maybe
import qualified Data.Text as T

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

clear = putStr "\ESC[2J"
moveCursorTo a b = putStr ("\ESC[" ++ show a ++ ";" ++ show b ++"H")

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

-- Gets us 6 different colors for an arbit. integer
color :: Int -> [Char]
color (v) = "\ESC[" ++ show (31 + (v `mod` 6)) ++ "m"

-- remaining string, position, position_to_watch, current_level, level_to_highlight
type Stream = ([Char], Int, Maybe Int, Maybe Int, Int)

highlight :: Stream -> String
highlight (s, pos, pos_match_left, pos_match_right, lvl) -- pos_watch is unused as of now, but could be used for putting cursor here.
  | null s = ""
  | otherwise = do
    let next_level | head s == '(' || head s == '{' = lvl + 1
                   | head s == ')' || head s == '}' = lvl - 1
                   | otherwise = lvl
        ret | next_level == lvl = if head s == '+' || head s == '-' || head s == '/' || head s == '*' || head s == '?' then "\ESC[1m" ++ [head s] ++ "\ESC[0m" else [head s]
            | next_level > lvl = (color lvl ++ (if isJust pos_match_left && pos == fromJust pos_match_left then "\ESC[4m" else "") ++ [head s] ++ "\ESC[0m")
            | otherwise = (color next_level ++ (if isJust pos_match_right && pos == fromJust pos_match_right then "\ESC[4m" else "") ++ [head s] ++ "\ESC[0m" )
    ret ++ highlight (tail s, pos+1, pos_match_left, pos_match_right, next_level)


findMatchRight :: ([Char], Int, Int, Int, Maybe Int) -> Maybe Int
findMatchRight (s, pos, pos_watch, lvl, lvl_end) = do
  let next_level | head s == '(' || head s == '{' = lvl + 1
                 | head s == ')' || head s == '}' = lvl - 1
                 | otherwise = lvl

  if length s /= 0 then
    if pos < pos_watch then
      findMatchRight (tail s, pos+1, pos_watch, next_level, lvl_end)
    else
      if pos == pos_watch then
        findMatchRight (tail s, pos+1, pos_watch, next_level, Just lvl)
      else
        if isJust lvl_end && next_level == fromJust lvl_end then
          Just pos
        else
          findMatchRight (tail s, pos+1, pos_watch, next_level, lvl_end)
  else
    Nothing

findMatchLeft :: ([Char], Int, Int, Int, Maybe Int) -> Maybe Int
findMatchLeft (s, pos, pos_watch, lvl, lvl_end) = do
  let next_level | last s == '(' || last s == '{' = lvl - 1
                 | last s == ')' || last s == '}' = lvl + 1
                 | otherwise = lvl

  if length s /= 0 then
    if pos > pos_watch then
      findMatchLeft (init s, pos-1, pos_watch, next_level, lvl_end)
    else
      if pos == pos_watch then
        findMatchLeft (init s, pos-1, pos_watch, next_level, Just lvl)
      else
        if isJust lvl_end && next_level == fromJust lvl_end then
          Just pos
        else
          findMatchLeft (init s, pos-1, pos_watch, next_level, lvl_end)
  else
    Nothing

type Params = (Int, Int, Int)
type Editorstate = (Params, [Char], [Char])

isWord :: String -> Bool
isWord = and . map isLetter

-- needed for custom myWords.
-- Note: Here I define a letter as ascii upper or lower
-- Note: This doesn't include modifier letters and other funky stuff by haskell
isNotMyLetter :: Char -> Bool
isNotMyLetter c = not ((isUpper $ c) || (isLower $ c))

myWords :: String -> [String]
myWords s | s' == ""  = []
          | otherwise = word : myWords rest
  where s' = dropWhile isNotMyLetter s
        (word, rest) = break isNotMyLetter s'

myBreak :: String -> [String]
myBreak s | s == "" = []
          | otherwise = do
            let (word, rest) = break isNotMyLetter s
            if word == "" then
              [(head s)] : myBreak (tail rest)
            else
              word : myBreak rest

getWordUnderCursor :: (String,String) -> String
getWordUnderCursor (p,q) = current_word
  where current_word = if isWord (pe ++ qu) then (pe ++ qu) else ""
        pe = if null p then "" else if isNotMyLetter $ last p then "" else last $ myWords p
        qu = if null q then "" else if isNotMyLetter $ head q then "" else head $ myWords q

-- from https://codereview.stackexchange.com/questions/85703/replacing-items-in-a-list
replace' :: Eq b => b -> b -> [b] -> [b]
replace' a b = map (\x -> if (a == x) then b else x)

highlightWords :: String -> String -> String
highlightWords "" t = t
highlightWords w t = intercalate "" (replace' w ("\ESC[4m" ++ w ++ "\ESC[0m") (myBreak t))

current_line :: String -> Int
current_line p = count '\n' p + 1

current_column :: String -> Int
current_column p
    | length p == 0 || last p == '\n' = 1
    | otherwise = (length $ last $ lines p)+1

editor :: Editorstate -> IO Editorstate
editor ((skip,w,h),p,q) 
  | current_line p - skip == 0 = editor ((skip-1,w,h), p, q)
  | current_line p - skip > h = editor ((skip+1,w,h), p,q)
  | otherwise = do
    s <- size
    let h | isJust s = height (fromJust s) -1
          | otherwise = 24
    let w | isJust s = width (fromJust s)
          | otherwise = 80

    clear
    moveCursorTo 0 0
    let s | head q == '(' || head q == '{' = highlight ((p ++ q), 0, Just (length p), findMatchRight (p++q, 0, length p, 0, Nothing), 0)
          | head q == ')' || head q == '}' = highlight ((p ++ q), 0, findMatchLeft (p++q, length (p++q) - 1, length p, 0, Nothing), Just (length p), 0) --fixme see below (fixed?)
          | otherwise = highlight ((p ++ q), 0, Nothing, Nothing, 0)

    --NOTE: possible optimisation involes only checking the region shown on screen
    let r = highlightWords (if not $ isNotMyLetter $ head q then getWordUnderCursor (p,q) else "") s

    putStr $ unlines (take h $ drop skip $ lines r)

    -- *** Status Bar *** --
    moveCursorTo (h+1) 0
    putStr "\ESC[7m"
    putStr $ replicate w ' '
    moveCursorTo (h+1) 0
    putStr "Current Line: "
    putStr $ show $ current_line p
    putStr " Current Column: "
    putStr $ show $ current_column p
    when (count '(' (p++q) /= count ')' (p++q)) (putStr " \ESC[31mError: Unbalanced '()'!\ESC[0m\ESC[7m")
    when (count '<' (p++q) /= count '>' (p++q)) (putStr " \ESC[31mError: Unbalanced '<>'!\ESC[0m\ESC[7m")
    when (count '{' (p++q) /= count '}' (p++q)) (putStr " \ESC[31mError: Unbalanced '{}'!\ESC[0m\ESC[7m")
    moveCursorTo (h+1) (w-21)
    putStr "^C: quit | <esc>: save"
    putStr "\ESC[0m"
    

    -- Set cursor to actual cursor position
    moveCursorTo (current_line p - skip) (current_column p)
    hFlush stdout
    c <- getKey
    if c == "\ESC" then return ((skip,w,h), p,q) -- promote Editorstate to IO Editorstate
      else do
        case c of
          "\ESC[A" -> if (length $ lines p) > 1 then editor ((skip,w,h), (unlines $ init $ (if last p == '\n' then (lines p) else (init $ lines p))) ++ take (min (current_column p - 1) (length $ last $ init $ lines p)) (last $ init $ lines p), drop (min (current_column p - 1) (length $ last $ init $ lines p)) (if last p == '\n' then (last $ lines p)++"\n" else (last $ init $ lines p) ++"\n"++ (last $ lines p)) ++ q) else (if length p == 0 then editor ((skip,w,h), p, q) else (if last p == '\n' then editor ((skip,w,h), [], p++q) else editor ((skip,w,h), p, q)))-- Up
          "\ESC[B" -> if (length $ lines q) > 1 then editor ((skip,w,h), p ++ (head $ lines q) ++ "\n" ++ take (min (current_column p - 1) (length $ head $ tail $ lines q)) (head $ tail $ lines q), drop (min (current_column p - 1) (length $ head $ tail $ lines q)) (head $ tail $ lines q) ++ "\n" ++ (unlines $ tail $ tail $ lines q)) else editor ((skip,w,h), p, q)-- Down
          "\ESC[C" -> if length q > 1 then editor ((skip,w,h), p ++ [head q], tail q) else editor ((skip,w,h), p, q) -- Right, needs > 1 for special last char
          "\ESC[D" -> if length p > 0 then editor ((skip,w,h), init p, last p : q) else editor ((skip,w,h), p, q) -- Left
          "\DEL"   -> if not (null p) then -- Delete
                        if not (null q) && 
                          ((last p == '(' && head q == ')') || 
                          (last p == '<' && head q == '>') || 
                          (last p == '{' && head q == '}')) then
                          editor ((skip,w,h), init p, tail q) -- Delete additional parens
                        else
                          editor ((skip,w,h), init p, q)
                      else 
                        editor ((skip,w,h), p, q)             
          "("      -> editor ((skip,w,h), p ++ "(", ")" ++ q)
          "<"      -> editor ((skip,w,h), p ++ "<", ">" ++ q)
          "{"      -> editor ((skip,w,h), p ++ "{", "}" ++ q)
          "\t"     -> editor ((skip,w,h), p ++ "  ", q)
          _        -> editor ((skip,w,h), p ++ (if isPrint $ head c then c else ""),q)

getContent :: String -> IO String
getContent name = do
        w <- doesFileExist name
        if w then readFile name
          else do
            h <- openFile name WriteMode
            hClose h
            return []

loop :: String -> Editorstate -> IO ()
loop n t = do
        (s, p, q) <- editor t
        writeFile n (p++q++"\n")
        clear
        moveCursorTo 0 0
        putStr "      ___           ___                         ___                   \n     /\\__\\         /\\  \\          ___          /\\__\\         _____    \n    /:/ _/_       /::\\  \\        /\\  \\        /:/ _/_       /::\\  \\   \n   /:/ /\\  \\     /:/\\:\\  \\       \\:\\  \\      /:/ /\\__\\     /:/\\:\\  \\  \n  /:/ /::\\  \\   /:/ /::\\  \\       \\:\\  \\    /:/ /:/ _/_   /:/  \\:\\__\\ \n /:/_/:/\\:\\__\\ /:/_/:/\\:\\__\\  ___  \\:\\__\\  /:/_/:/ /\\__\\ /:/__/ \\:|__|\n \\:\\/:/ /:/  / \\:\\/:/  \\/__/ /\\  \\ |:|  |  \\:\\/:/ /:/  / \\:\\  \\ /:/  /\n  \\::/ /:/  /   \\::/__/      \\:\\  \\|:|  |   \\::/_/:/  /   \\:\\  /:/  / \n   \\/_/:/  /     \\:\\  \\       \\:\\__|:|__|    \\:\\/:/  /     \\:\\/:/  /  \n     /:/  /       \\:\\__\\       \\::::/__/      \\::/  /       \\::/  /   \n     \\/__/         \\/__/        ~~~~           \\/__/         \\/__/    \n\n\n\t\t\t Press Any Key to Continue"
        hFlush stdout
        a <- getKey
        loop n (s,p,q)

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) (putStr "no filename provided!\n" >> exitWith ExitSuccess) -- FIXME: success?
  c <- getContent $ head args

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clear
  putStr "\ESC[?7l" -- disables line wrapping (VT100)
  loop (head args) ((0,80,24),[],c)
