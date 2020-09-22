import System.IO
import Control.Monad (when)

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

clear = putStr "\ESC[2J"
moveCursorTo a b = putStr ("\ESC[" ++ show a ++ ";" ++ show b ++"H")

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

-- Gets us 6 different colors for an arbit. integer
color :: Int -> [Char]
color (v) = "\ESC[" ++ show (31 + (v `mod` 6)) ++ "m"

-- remaining string, position, position_to_watch, current_level, level_to_highlight
type Stream = ([Char], Int, Int, Int, Int)

highlight :: Stream -> IO()
highlight (s, pos, pos_watch, lvl, lvl_end)
  | null s = putStr ""
  | otherwise = do
    let next_level | head s == '(' = lvl + 1
                   | head s == ')' = lvl - 1
                   | otherwise = lvl
    if next_level /= lvl then
      putStr ( color(if next_level > lvl then lvl else next_level) ++ [head s] ++ "\ESC[0m" )
    else
      putChar (head s)
    highlight (tail s, pos+1, pos_watch, next_level, lvl_end)

type Text = ([Char], [Char])

editor :: Text -> IO()
editor (p,q) = do
  clear
  moveCursorTo 0 0
  highlight ((p ++ q), 0, length p, 0, -1)
  moveCursorTo (count '\n' p + 1) (if length p == 0 || last p == '\n' then 1 else (length $ last $ lines p)+1)
  hFlush stdout
  c <- getKey
  when (c /= "\ESC") $ do -- TODO: implement save/load (maybe \ESC S|W)
    case c of -- TODO: maybe implement up and down arrows? 
      "\ESC[C" -> if not (null q) then editor (p ++ [head q], tail q) else editor (p, q) -- Right
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
      _        -> editor (p ++ c,q)

main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clear
  editor ([],"((<y>(<x>(- y x)) 5) 3)")
