import System.IO
import System.Environment
import System.Directory
import System.Console.Terminal.Size
import Data.List
import System.Exit
import Control.Monad (when)
import Data.Maybe

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
        ret | next_level == lvl = [head s]
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

type Text = (Int, [Char], [Char])


current_line :: String -> Int
current_line p = count '\n' p + 1 

editor :: Text -> IO Text
editor (skip,p,q) 
  | current_line p - skip == 0 = editor (skip-1, p, q)
  | current_line p - skip > 20 = editor (skip+1, p,q)
  | otherwise = do
    -- fixme: we need h two lines above already
    -- *** Figure out terminal size *** --
    -- default if nothing: 80x24
    s <- size
    let h | isJust s = height (fromJust s)
          | otherwise = 24
    let w | isJust s = width (fromJust s)
          | otherwise = 80

    clear
    moveCursorTo 0 0
    let s | head q == '(' || head q == '{' = highlight ((p ++ q), 0, Just (length p), findMatchRight (p++q, 0, length p, 0, Nothing), 0)
          | head q == ')' || head q == '}' = highlight ((p ++ q), 0, findMatchLeft (p++q, length (p++q) - 1, length p, 0, Nothing), Just (length p), 0) --fixme see below (fixed?)
          | otherwise = highlight ((p ++ q), 0, Nothing, Nothing, 0)

    -- TODO: 20 lines max for now?
    --(h, w) <- size
    --putStr (show (fromJust h))
    putStr $ unlines (take h $ drop skip $ lines s)

    -- debugging begin --
    --moveCursorTo 3 0
    --putStr (show (fromMaybe 9 (findMatchRight (p++q, 0, length p, 0, Nothing))))
    --putStr " "
    --putStr (show (fromMaybe 9 (findMatchLeft (p++q, length (p++q) - 1, length p, 0, Nothing))))
    -- debugging end --

    -- *** Status Bar *** --
    moveCursorTo (h+1) 0
    putStr "\ESC[7m"
    putStr $ replicate w ' '
    moveCursorTo (h+1) 0
    putStr "Current Line: "
    putStr $ show $ current_line p
    putStr " Current Column: "
    putStr $ show (if length p == 0 || last p == '\n' then 1 else (length $ last $ lines p)+1)
    if count '(' (p++q) /= count ')' (p++q) then
      putStr " \ESC[31mSyntax Error: Unbalanced parenthesis!\ESC[0m\ESC[7m"
    else
      putStr ""
    putStr "\ESC[0m"

    -- Set cursor to actual cursor position
    moveCursorTo (current_line p - skip) (if length p == 0 || last p == '\n' then 1 else (length $ last $ lines p)+1)
    hFlush stdout
    c <- getKey
    if c == "\ESC" then return (skip, p,q) -- promote Text to IO Text
      else do
        case c of --TODO: stay at level of indentation? 
          "\ESC[A" -> if lines p /= [] then editor (skip, unlines $ init $ lines p, (last $ lines p) ++ (if last p == '\n' then "\n" else "") ++ q) else editor (skip, p, q)-- Up
          "\ESC[B" -> if (length $ lines q) > 1 then editor (skip, p ++ (head $ lines q) ++ "\n", unlines $ tail $ lines q) else editor (skip, p, q)-- Down
          "\ESC[C" -> if length q > 1 then editor (skip, p ++ [head q], tail q) else editor (skip, p, q) -- Right, needs > 1 for special last char
          "\ESC[D" -> if length p > 0 then editor (skip, init p, last p : q) else editor (skip, p, q) -- Left
          "\DEL"   -> if not (null p) then -- Delete
                        if not (null q) && 
                          ((last p == '(' && head q == ')') || 
                          (last p == '<' && head q == '>') || 
                          (last p == '{' && head q == '}')) then
                          editor (skip, init p, tail q) -- Delete additional parens
                        else
                          editor (skip, init p, q)
                      else 
                        editor (skip, p, q)             
          "("      -> editor (skip, p ++ "(", ")" ++ q)
          "<"      -> editor (skip, p ++ "<", ">" ++ q)
          "{"      -> editor (skip, p ++ "{", "}" ++ q)
          "\t"     -> editor (skip, p ++ "  ", q)
          _        -> editor (skip, p ++ c,q) --TODO: also scroll on \n if in >>last row<< :)

getContent :: String -> IO String
getContent name = do
        w <- doesFileExist name --since this returns IO Bool we have to sequence it (do block) in order to use its result
        if w then readFile name
          else do
            h <- openFile name WriteMode --erstellt File,same thing, since its IO we have to sequence it.
            hClose h -- close it immediately :) 
            return [] -- promote String to IO String

loop :: String -> Text -> IO ()
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
  loop (head args) (0,[],c)
