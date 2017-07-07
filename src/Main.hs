import IML.FrontEnd.Lexer

example = ["quo <- 0",
           "rem <- 42",
           "while(rem >= 6) {",
           "  quo <- (quo + 1)",
           "  rem <- (rem - 6)",
           "}"]

main = do
  putStrLn "Current program:"
  putStrLn $ unlines example
  putStrLn "\nToken Stream:"
  print $ tokenize $ unlines example
