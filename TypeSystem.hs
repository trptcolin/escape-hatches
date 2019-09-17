import System.IO.Unsafe (unsafePerformIO)

count :: String -> Int
count text =
  unsafePerformIO $ do
    print "OHH NOOOOOO"
    return $ length text

data StringData = StringData {
  text :: String,
  reversed :: String,
  size :: Int
} deriving (Show)

stats :: String -> StringData
stats text = StringData {
  text = text,
  reversed = reverse text,
  size = count text
}

main :: IO ()
main = print (size (stats "Hello, World!"))

-- stack runghc ./TypeSystem.hs
