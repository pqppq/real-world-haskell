import Data.Char (toUpper)
import System.IO

main :: IO ()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  loop inh outh
  hClose inh
  hClose outh

loop :: Handle -> Handle -> IO ()
loop inh outh = do
  ineof <- hIsEOF inh
  if ineof
    then return ()
    else do
      inpStr <- hGetLine inh
      hPutStrLn outh (map toUpper inpStr)
      loop inh outh
