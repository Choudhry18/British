import Lexer
import Data.Maybe


inputs = ["5",
          "52",
          "172.2",
          "7*3",
          "MR",
          "MS",
          "mole",
          "fee",
          "ifz",
          "then",
          "else",
          "+ * / -",
          "ifz 3 then 35.23 else phi",
          "7 - 2 35.23 / pie",
          "76 - (7 % 10) - 6 MS",
          "ifz MR then sqrt 32 else pie",
          "else ifz then MR MS - 0.0",
          "0.0 0 0.9 1.9 1.5",
          "5 = 5",
          "() ) )",
          "><",
          "/\\",
          "\\/"
          ]

pad str n = 
    let spaces = ' ':spaces
        padding = take (n - length(str)) spaces
    in str++padding


testStr n str = 
    let resStr = show $ scanTokens str in
    do
      putStr (pad str n)
      putStrLn resStr

main = 
    let ml = (maximum (map length inputs)) + 4
        tests = map (testStr ml) inputs
    in do
      sequence tests
      return ()