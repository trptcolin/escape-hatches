import Debug.Trace

count :: String -> Int
count = length

count' :: String -> Int
count' text =
  trace ("OHHHH NOOOOOO: " ++ show text) (length text)

main :: IO ()
main = do
  print $ count "Hello, World!"
  print $ count' "Hello, World!"
