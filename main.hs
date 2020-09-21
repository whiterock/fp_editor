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

type Text = ([Char], [Char])

editor :: Text -> IO()
editor (p,q) = do
  clear
  moveCursorTo 0 0
  putStr (p ++ q)
  moveCursorTo (count '\n' p + 1) (if length p == 0 || last p == '\n' then 1 else (length $ last $ lines p)+1)
  hFlush stdout
  c <- getKey
  when (c /= "\ESC") $ do -- TODO: implement save/load (maybe \ESC S|W)
    case c of -- TODO: maybe implement up and down arrows? 
      "\ESC[C" -> editor (p ++ [head q], tail q) -- TODO: fix head empty list
      "\ESC[D" -> editor (init p, [last p] ++ q) -- TODO: fix init empty list
      "\DEL"   -> editor (init p, q)
      _        -> editor (p ++ c,q)

main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clear
  editor ([],[' '])
