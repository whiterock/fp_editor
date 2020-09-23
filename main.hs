import System.IO
import System.Environment
import System.Directory
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

highlight :: Stream -> IO()
highlight (s, pos, pos_match_left, pos_match_right, lvl) -- pos_watch is unused as of now, but could be used for putting cursor here.
  | null s = putStr ""
  | otherwise = do
    let next_level | head s == '(' || head s == '{' = lvl + 1
                   | head s == ')' || head s == '}' = lvl - 1
                   | otherwise = lvl
    if next_level /= lvl then
      if next_level > lvl then 
      --  putStr (color lvl ++ "\ESC[4m" ++ [head s] ++ "\ESC[0m") -- underline
      putStr (color lvl ++ (if isJust pos_match_left && pos == fromJust pos_match_left then "\ESC[4m" else "") ++ [head s] ++ "\ESC[0m")
      else
      --  putStr (color next_level ++ "\ESC[4m" ++ [head s] ++ "\ESC[0m" ) -- underline
      putStr (color next_level ++ (if isJust pos_match_right && pos == fromJust pos_match_right then "\ESC[4m" else "") ++ [head s] ++ "\ESC[0m" )
    else
      putChar (head s)
    highlight (tail s, pos+1, pos_match_left, pos_match_right, next_level)


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

type Text = ([Char], [Char])

editor :: Text -> IO Text
editor (p,q) = do
  clear
  moveCursorTo 0 0
  if last p == '(' || last p == '{' || last p == ')' || last p == '}' then
    if last p == '(' || last p == '{' then
      highlight ((p ++ q), 0, Just (length p), findMatchRight (p++q, 0, length p, 0, Nothing), 0)
    else
      -- fixme
      highlight ((p ++ q), 0, Just (length p), findMatchRight (p++q, 0, length p, 0, Nothing), 0)
  else
    highlight ((p ++ q), 0, Nothing, Nothing, 0)
  -- debugging begin --
  --moveCursorTo 3 0
  --putStr (show (fromMaybe 9 res))
  -- debugging end --
  --moveCursorTo 0 0
  --highlight ((p ++ q), 0, head parens, tail parens,  0)
  moveCursorTo (count '\n' p + 1) (if length p == 0 || last p == '\n' then 1 else (length $ last $ lines p)+1)
  hFlush stdout
  c <- getKey
  if c == "\ESC" then return (p,q) -- promote Text to IO Text
    else do
      case c of --TODO: stay at level of indentation? 
        "\ESC[A" -> if lines p /= [] then editor (unlines $ init $ lines p, (last $ lines p) ++ (if last p == '\n' then "\n" else "") ++ q) else editor (p, q)-- Up
        "\ESC[B" -> if (length $ lines q) > 1 then editor (p ++ (head $ lines q) ++ "\n", unlines $ tail $ lines q) else editor (p, q)-- Down
        "\ESC[C" -> if length q > 1 then editor (p ++ [head q], tail q) else editor (p, q) -- Right, needs > 1 for whatever reason
        "\ESC[D" -> if length p > 1 then editor (init p, last p : q) else editor (p, q) -- Left
        "\DEL"   -> if not (null p) then -- Delete
                      if not (null q) && 
                        ((last p == '(' && head q == ')') || 
                        (last p == '<' && head q == '>') || 
                        (last p == '{' && head q == '}')) then
                        editor (init p, tail q) -- Delete additional parens
                      else
                        editor (init p, q)
                    else 
                      editor (p, q)             
        "("      -> editor (p ++ "(", ")" ++ q)
        "<"      -> editor (p ++ "<", ">" ++ q)
        "{"      -> editor (p ++ "{", "}" ++ q)
        "\t"     -> editor (p ++ "  ", q)
        _        -> editor (p ++ c,q) --TODO: careful about other escape sequences (like up and down arrow.)

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
        (p, q) <- editor t
        writeFile n (p++q++"\n")
        moveCursorTo 0 0
        putStr "      ___           ___                         ___                   \n     /\\__\\         /\\  \\          ___          /\\__\\         _____    \n    /:/ _/_       /::\\  \\        /\\  \\        /:/ _/_       /::\\  \\   \n   /:/ /\\  \\     /:/\\:\\  \\       \\:\\  \\      /:/ /\\__\\     /:/\\:\\  \\  \n  /:/ /::\\  \\   /:/ /::\\  \\       \\:\\  \\    /:/ /:/ _/_   /:/  \\:\\__\\ \n /:/_/:/\\:\\__\\ /:/_/:/\\:\\__\\  ___  \\:\\__\\  /:/_/:/ /\\__\\ /:/__/ \\:|__|\n \\:\\/:/ /:/  / \\:\\/:/  \\/__/ /\\  \\ |:|  |  \\:\\/:/ /:/  / \\:\\  \\ /:/  /\n  \\::/ /:/  /   \\::/__/      \\:\\  \\|:|  |   \\::/_/:/  /   \\:\\  /:/  / \n   \\/_/:/  /     \\:\\  \\       \\:\\__|:|__|    \\:\\/:/  /     \\:\\/:/  /  \n     /:/  /       \\:\\__\\       \\::::/__/      \\::/  /       \\::/  /   \n     \\/__/         \\/__/        ~~~~           \\/__/         \\/__/    \n\n\n\t\t\t Press Any Key to Continue"
        hFlush stdout
        a <- getKey
        loop n (p,q)

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) (putStr "no filename provided!\n" >> exitWith ExitSuccess) -- FIXME: success?
  c <- getContent $ head args

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clear
  loop (head args) ([],c)
