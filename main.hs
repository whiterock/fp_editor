import System.IO
import System.Environment
import System.Directory
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
type Stream = ([Char], Int, Int, Int, Maybe Int)

highlight :: Stream -> IO()
highlight (s, pos, pos_watch, lvl, lvl_oi)
  | null s = putStr ""
  | otherwise = do
    let next_level | head s == '(' || head s == '{' = lvl + 1
                   | head s == ')' || head s == '}' = lvl - 1
                   | otherwise = lvl
    if next_level /= lvl then
      -- todo: figure out the conditions that have to be met here for special highlighting
      -- (for instance: pos <= pos_watch for a starting paren and the lvl correct)
      putStr ( color(if next_level > lvl then lvl else next_level) ++ [head s] ++ "\ESC[0m" )
    else
      putChar (head s)
    highlight (tail s, pos+1, pos_watch, next_level, lvl_oi)

findMatch :: ([Char], Int, Int, Int) -> Maybe Int
findMatch (s, pos, pos_watch, lvl) = do
  let next_level | head s == '(' || head s == '{' = lvl + 1
                 | head s == ')' || head s == '}' = lvl - 1
                 | otherwise = lvl

  if pos < pos_watch then
    findMatch (tail s, pos+1, pos_watch, next_level)
  else
    if next_level /= lvl then
      Just (if next_level > lvl then lvl else next_level)
    else
      Nothing

type Text = ([Char], [Char])

editor :: Text -> IO Text
editor (p,q) = do
  clear
  let res = findMatch (p++q, 0, length p, 0)
  -- debugging begin --
  --moveCursorTo 3 0
  --putStr (show (fromMaybe 9 res))
  -- debugging end --
  moveCursorTo 0 0
  highlight ((p ++ q), 0, length p, 0, res)
  moveCursorTo (count '\n' p + 1) (if length p == 0 || last p == '\n' then 1 else (length $ last $ lines p)+1)
  hFlush stdout
  c <- getKey
  if c == "\ESC" then return (p,q) -- promote Text to IO Text
    else do
      case c of -- TODO: maybe implement up and down arrows? 
        "\ESC[C" -> if not (null q) then editor (p ++ [head q], tail q) else editor (p, q) -- Right FIXME: this fails again, dunno y
        "\ESC[D" -> if not (null p) then editor (init p, last p : q) else editor (p, q) -- Left
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
  when (length args == 0) (putStr "no filename provided!\n" >> exitWith ExitSuccess)
  c <- getContent $ head args

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clear
  loop (head args) ([],c)
