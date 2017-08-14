module Main where

import Data.Char -- isDigit digitToInt

-- general testing function taking
-- a name to print
-- a function to test: a -> b
-- a list of pairs of input and expected output: [(a,b)]
myTest :: Show a => Eq b => Show b => String -> (a->b) -> [(a,b)] -> IO()
myTest name func testcases = do
        putStrLn ""
        putStrLn ("Testing " ++ name)
        myTest_inner func testcases
        putStrLn ("Finished testing for " ++ name)
        putStrLn ""
    where
        myTest_inner :: Show a => Eq b => Show b => (a->b) -> [(a,b)] -> IO()
        myTest_inner func []     = return ()
        myTest_inner func ((a,b):xs) = do
                                   myTest_inner_single func a b
                                   myTest_inner func xs
        myTest_inner_single :: Show a => Eq b => Show b => (a->b) -> a -> b -> IO()
        myTest_inner_single func inp exp =
                            if exp == (func inp)
                                then return () -- putStrLn ("Success for input " ++ (show inp))
                                else putStrLn ("FAILURE for input " ++ (show inp) ++ " got: " ++ (show (func inp)))

-- a function to convert entire decimal strings to integer
stringToInt :: String -> Int
stringToInt "" = 0
stringToInt xs = stringToIntInner 0 xs
    where
        stringToIntInner :: Int -> String -> Int
        stringToIntInner acc ""     = acc
        stringToIntInner acc (x:xs) = stringToIntInner (acc*10 + x') xs
            where x' = digitToInt x


-- (fn (name arg1 arg2 ... argn)
--     body)
-- (let (name value)
--      body)
-- (if cond
--     then-body
--     else-body)
-- (== a b)
-- (< a b)
-- (> a b)
-- (concat str1 str2)
-- "..."
-- [0-9]+
-- (print value)
-- (println value)
-- (println)

data Token = TSymbol String
           | TLparen
           | TRparen
           | TString String
           | TNumber Int
    deriving (Show, Eq)

lexer :: String     -> [Token]
lexer    ""          = []
lexer    (' ' :rest) = lexer rest
lexer    ('\n':rest) = lexer rest
lexer    ('\t':rest) = lexer rest
lexer    ('\r':rest) = lexer rest
lexer    ('(' :rest) = TLparen : lexer rest
lexer    (')' :rest) = TRparen : lexer rest
lexer    ('"' :rest) = (TString contents) : lexer rrest
    where (contents, rrest) = consumeString rest
             where consumeString :: String -> (String, String)
                   consumeString str = consumeStringInner str ""
                        where consumeStringInner :: String -> String -> (String, String)
                              consumeStringInner left ('"':right) = (left, right)
                              consumeStringInner left ""          = (left, "")
                              consumeStringInner left (r:right)   = consumeStringInner (left ++ [r]) right
lexer    (n   :rest) | isDigit n = TNumber (stringToInt (n:first)) : (lexer second )
              where (first, second) = break (not . isDigit) rest
lexer    (c   :rest) = (TSymbol (c:contents)) : lexer rrest
    where (contents, rrest) = break breaksSymbol rest
            where breaksSymbol :: Char -> Bool
                  breaksSymbol ' ' = True
                  breaksSymbol '\n' = True
                  breaksSymbol '\r' = True
                  breaksSymbol '\t' = True
                  breaksSymbol ')' = True
                  breaksSymbol '"' = True
                  breaksSymbol '(' = True
                  breaksSymbol _ = False


lexer_test :: IO ()
lexer_test = myTest "lexer" lexer testcases
    where testcases = [
                        ("(", [TLparen]),
                        (")", [TRparen]),
                        ("(hello + 11 1)", [TLparen, (TSymbol "hello"), (TSymbol "+"), (TNumber 11), (TNumber 1), TRparen]),
                        ("", [])
                      ]

data Sexpr = SSymbol String
           | SString String
           | SNumber Int
           | SLet String Sexpr Sexpr
           | SFdecl String [String] Sexpr
           | SFcall String [Sexpr]
    deriving (Show, Eq)

data Program = Program [Sexpr]
    deriving (Show, Eq)

parse :: [Token] -> Program
parse [] = Program []

parse_test :: IO ()
parse_test = myTest "parse" parse testcases
    where testcases = [
                        ([TLparen, (TSymbol "+"), (TNumber 1), (TNumber 2), TRparen], (Program [(SFcall "+" [(SNumber 1), (SNumber 2)])])),
                        ([(TString "hello world")], (Program [(SString "hello world")])),
                        ([], (Program []))
                      ]

main :: IO ()
main = do
    putStrLn ""
    lexer_test
    putStrLn ""
    parse_test
    putStrLn ""

