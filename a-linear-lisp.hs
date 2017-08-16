module Main where

-- a minimal algebraic-normal-form lisp

import Data.Char (isDigit, digitToInt)
import Control.Monad (foldM)

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll x (y:ys) | x == y = deleteAll x ys
deleteAll x (y:ys) | x /= y = y : (deleteAll x ys)

-- abstract grammar
-- Program = [Expr]
-- Expr = Symbol
--      | String
--      | Number
--      | Fcall
--      | Fdecl
--      | IfExpr
--      | LetExpr
-- Fcall = Symbol Symbol...
-- Fdecl = "fn" (Symbol Symbol...) Expr
-- IfExpr = "if" Symbol Expr Expr
-- LetExpr = "let" (Symbol Expr) Expr

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

data Token = TSymbol String
           | TLparen
           | TRparen
           | TString String
           | TNumber Int
           | TBoolean Bool
    deriving (Show, Eq)

lexer :: String     -> [Token]
lexer    ""          = []
lexer    (' ' :rest) = lexer rest
lexer    ('\n':rest) = lexer rest
lexer    ('\t':rest) = lexer rest
lexer    ('\r':rest) = lexer rest
lexer    ('(' :rest) = TLparen : lexer rest
lexer    (')' :rest) = TRparen : lexer rest
lexer    ('#' : 't' : rest) = (TBoolean True) : lexer rest
lexer    ('#' : 'f' : rest) = (TBoolean False) : lexer rest
lexer    ('"' :rest) = (TString contents) : lexer rrest
    where (contents, rrest) = consumeString rest
             where consumeString :: String -> (String, String)
                   consumeString str = consumeStringInner "" str
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
                        ("a", [(TSymbol "a")]),
                        ("(if a b c)", [TLparen, (TSymbol "if"), (TSymbol "a"), (TSymbol "b"), (TSymbol "c"), TRparen]),
                        ("(\"Hello\")", [TLparen, (TString "Hello"), TRparen]),
                        ("(\"Hello world\")", [TLparen, (TString "Hello world"), TRparen]),
                        ("", [])
                      ]

data SExpr = SSymbol String
           | SString String
           | SNumber Int
           | SBoolean Bool
           | SLet String SExpr SExpr
           | SIf String SExpr SExpr
           | SFdecl String [String] SExpr
           | SFcall String [String]
           -- drop var 'String' and continue eval in SExpr
           | SDrop String SExpr
    deriving (Show, Eq)

data Program = Program [SExpr]
    deriving (Show, Eq)

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

parseValue :: Token -> SExpr
parseValue (TString contents) = (SString contents)
parseValue (TSymbol contents) = (SSymbol contents)
parseValue (TNumber contents) = (SNumber contents)
parseValue (TBoolean contents) = (SBoolean contents)
parseValue invalid = error $ "parseValue called with non-value: " ++ show invalid

parseSExprList :: [Token] -> ([SExpr], [Token])
parseSExprList [] = ([], [])
parseSExprList tokens = parseSExprListInner [] tokens
    where parseSExprListInner ::[SExpr] -> [Token]         -> ([SExpr], [Token])
          parseSExprListInner   sexprs     []               = (sexprs, [])
          parseSExprListInner   sexprs     (TRparen : rest) = (sexprs, rest)
          parseSExprListInner   sexprs     something        = parseSExprListInner (sexprs ++ [ssexpr]) rsomething
            where (ssexpr, rsomething) = parseSExpr something

parseParams :: [Token] -> ([String], [Token])
parseParams [] = ([], [])
parseParams tokens = parseParamsInner [] tokens
    where parseParamsInner ::[String] -> [Token]         -> ([String], [Token])
          parseParamsInner   strings     []               = (strings, [])
          parseParamsInner   strings     (TRparen : rest) = (strings, rest)
          parseParamsInner   strings     ((TSymbol contents) : rest) = parseParamsInner (strings ++ [contents]) rest

parseSExpr :: [Token] -> (SExpr, [Token])
parseSExpr (TLparen : (TSymbol "drop") : (TSymbol name) : rest) = ((SDrop name body), rrest)
    where (body, (TRparen:rrest)) = parseSExpr rest
parseSExpr (TLparen : (TSymbol "let") : TLparen : (TSymbol name) : value : TRparen : rest) = ((SLet name vvalue body), rrest)
    where (body, (TRparen:rrest)) = parseSExpr rest
          vvalue = parseValue value
parseSExpr (TLparen : (TSymbol "fn")  : TLparen : (TSymbol name) : rest) = ((SFdecl name params body), rest2)
    where (params, rest1) = parseParams rest
          (body,   (TRparen:rest2)) = parseSExpr rest1
parseSExpr (TLparen : (TSymbol "if")  : (TSymbol cond): rest) = ((SIf cond body_then body_else), rest2)
    where (body_then, rest1) = parseSExpr rest
          (body_else, (TRparen:rest2)) = parseSExpr rest1
parseSExpr (TLparen : (TSymbol name)  : rest) = ((SFcall name args), rrest)
    where (args, rrest) = parseParams rest
parseSExpr (val : rest) = ((parseValue val), rest)

parse :: [Token] -> Program
parse [] = Program []
parse tokens = Program (parse_inner tokens)
    where parse_inner :: [Token] -> [SExpr]
          parse_inner    [] = []
          parse_inner    tokens       = sexpr : parse_inner rest
            where (sexpr, rest) = parseSExpr tokens

parse_test :: IO ()
parse_test = myTest "parse" parse testcases
    where testcases = [
                        ((lexer "(let (a 1) (let (b 2) (+ a b)))"),
                         (Program [(SLet "a" (SNumber 1) (SLet "b" (SNumber 2) (SFcall "+" ["a", "b"])))])),
                        ([(TString "hello world")],
                         (Program [(SString "hello world")])),
                        ((lexer "(if a b c)"),
                         (Program [(SIf "a" (SSymbol "b") (SSymbol "c"))])),
                        ((lexer "(let (a b) c)"),
                         (Program [(SLet "a" (SSymbol "b") (SSymbol "c"))])),
                        ((lexer "(fn (foo a b) (+ a b))"),
                         (Program [(SFdecl "foo" ["a", "b"] (SFcall "+" ["a", "b"]))])),
                        ((lexer "(let (a \"Hello\") (let (b \" world\") (concat a b)))"),
                         (Program [(SLet "a" (SString "Hello") (SLet "b" (SString " world") (SFcall "concat" ["a", "b"])))])),
                        ((lexer "(let (a 3)(let (b 4)(drop a b)))",
                         (Program [(SLet "a" (SNumber 3) (SLet "b" (SNumber 4) (SDrop "a" (SSymbol "b"))))]))),
                        ([],
                         (Program []))
                      ]

containsRepeated :: Eq a => [a] -> Bool
containsRepeated list = containsRepeatedInner list []
    where containsRepeatedInner :: Eq a => [a] -> [a] -> Bool
          containsRepeatedInner [] _ = False
          containsRepeatedInner (x:xs) list = if (x `elem` list)
                                              then True
                                              else containsRepeatedInner xs (x:list)

declare :: ([String], [String]) -> String -> Either String ([String], [String])
declare (declared, used) name | name `elem` declared = Left $ "Error: variable redeclared overwrites unused variable '" ++ name ++ "'"
declare (declared, used) name = Right (l,r)
    where l = (name:declared)
          r = deleteAll name used

use :: ([String], [String]) -> String -> Either String ([String], [String])
use (declared, used) name | not (name `elem` declared) = Left $ "Error: variable '" ++ name ++ "' not declared " ++ (show (declared, used))
use (_, used) name | name `elem` used = Left $ "Error: variable '" ++ name ++ "' re-used"
use (declared, used) name | name `elem` declared = Right (new_declared,(name:used))
    where new_declared = deleteAll name declared
use vars name = Left $ "Internal error: use unknown vars input: " ++ show vars ++ " for name " ++ name

foldMy :: (([String], [String]) -> String -> Either String ([String], [String]))
          -> Either String ([String], [String])
          -> [String]
          -> Either String ([String], [String])
foldMy _ either [] = either
foldMy _ (Left err) _ = Left err
foldMy f (Right vars) (x:xs) = case (f vars x) of
                                   (Left err)    -> Left err
                                   (Right nvars) -> foldMy f (Right nvars) xs

eitherToMaybe :: Either String ([String], [String]) -> Maybe String
eitherToMaybe (Left str) = Just str
eitherToMaybe _ = Nothing

checkVarsResult :: Either String ([String], [String]) -> Maybe String
checkVarsResult (Left err) = Just err
checkVarsResult (Right ([],_)) = Nothing
checkVarsResult (Right (unused,_)) = Just $ "Error: unused variables '" ++ show unused ++ "'"

-- check that every variable:
--  - is used exactly once
--  - is defined exactly once before usage
checkVars :: SExpr -> Maybe String
-- we need to verify that every leaf uses all vars
checkVars sexpr = checkVarsResult $ checkVarsInner sexpr ([],[])
    where checkVarsInner :: SExpr -> ([String],[String]) -> Either String ([String], [String])
          checkVarsInner (SSymbol name) vars = use vars name
          checkVarsInner (SString _) vars = Right vars
          checkVarsInner (SNumber _) vars = Right vars
          checkVarsInner (SBoolean _) vars = Right vars

          checkVarsInner (SDrop name body) vars = case (use vars name) of
                                                    (Left err) -> Left err
                                                    (Right new_vars) -> checkVarsInner body new_vars
          checkVarsInner (SLet name _ body) vars = case (declare vars name) of
                                                    (Left err) -> Left err
                                                    (Right new_vars) -> checkVarsInner body new_vars
          checkVarsInner (SIf cond body_then body_else) vars = case (use vars cond) of
                                                                (Left err) -> Left err
                                                                (Right vars1) -> case (checkVarsInner body_then vars1) of
                                                                                (Left err) -> Left err
                                                                                -- we need to verify that every leaf uses all vars
                                                                                (Right ([], _)) -> checkVarsInner body_else vars1
                                                                                (Right (unused, _)) -> Right (unused, [])
          checkVarsInner (SFdecl _ params body) _ | containsRepeated params = Left $ "Error: parameter list is not unique '" ++ (show params) ++ "'"
          checkVarsInner (SFdecl _ params body) vars = case (foldMy declare (Right vars) params) of
                                                        (Left err) -> Left err
                                                        (Right new_vars) -> checkVarsInner body new_vars
          -- check args are not repeated (used more than once in this call)
          checkVarsInner (SFcall _ args) _ | containsRepeated args = Left $ "Error: argument list is not unique '" ++ (show args) ++ "'"
          -- check that every arg is defined and previous unused
          checkVarsInner (SFcall _ args) vars = foldMy use (Right vars) args

-- check static properties
check :: Program -> [Maybe String]
check (Program sexprs) = deleteAll Nothing (map checkVars sexprs)

data Binding = Binding String SExpr

data Scope = EmptyScope
           | Scope Binding Scope

insert :: Scope -> String -> SExpr -> Scope
insert scope str val = Scope (Binding str val) scope

fetch :: Scope -> String -> SExpr
fetch EmptyScope str = error $ "Failed to find string: " ++ str
fetch (Scope (Binding str1 val1) _) str | str == str1 = val1
fetch (Scope _ scope) str = fetch scope str

truthy :: SExpr -> Bool
truthy (SBoolean contents) = contents
truthy (SString "") = False
truthy (SNumber 0) = False
truthy _  = True

-- create the new scope for an fcall
fcallScope :: Scope -> [String] -> [SExpr] -> Scope
fcallScope scope [] [] = scope
fcallScope scope (s:strings) (v:vals) = fcallScope (insert scope s v) strings vals

primop :: Scope -> String -> [String] -> SExpr
primop scope "+" (left:right:[]) = SNumber (lleft + rright)
    where (SNumber lleft) = fetch scope left
          (SNumber rright) = fetch scope right
primop scope "==" (left:right:[]) = if (lleft == rright) then (SNumber 1) else (SNumber 0)
    where (SNumber lleft) = fetch scope left
          (SNumber rright) = fetch scope right
primop scope "<" (left:right:[]) = if (lleft < rright) then (SNumber 1) else (SNumber 0)
    where (SNumber lleft) = fetch scope left
          (SNumber rright) = fetch scope right
primop scope ">" (left:right:[]) = if (lleft > rright) then (SNumber 1) else (SNumber 0)
    where (SNumber lleft) = fetch scope left
          (SNumber rright) = fetch scope right
primop scope "concat" (left:right:[]) = SString (lleft ++ rright)
    where (SString lleft) = fetch scope left
          (SString rright) = fetch scope right
primop _ name _ = error $ "Unknown primop: " ++ show name

-- TODO FIXME make eval_single :: Scope -> SExpr -> SExpr
-- and then have the other evals wrap

-- TODO FIXME make primops n-ary

eval_in_scope :: Scope -> [SExpr] -> [SExpr]
eval_in_scope    _ [] = []
eval_in_scope    scope ((SSymbol name):rest) =  (fetch scope name) : eval_in_scope scope rest
-- TODO FIXME drop should delete name from scope
-- here we are relying on 'check' preventing reuse, which is less clean
eval_in_scope    scope ((SDrop name body):rest) = eval_in_scope scope [body]
eval_in_scope    scope ((SLet name val body):rest) = (eval_in_scope let_scope [body]) ++ eval_in_scope scope rest
    where let_scope = insert scope name val
eval_in_scope    scope ((SFdecl name params body):rest) = eval_in_scope new_scope rest
    where new_scope = insert scope name val
            where val = (SFdecl name params body)
eval_in_scope    scope ((SIf cond body_then body_else):rest) = if (truthy (fetch scope cond))
                                                               then (eval_in_scope scope [body_then])
                                                               else (eval_in_scope scope [body_else])
eval_in_scope    scope ((SFcall name args):rest) | name `elem` primops = (primop scope name args) : eval_in_scope scope rest
    where primops = [ "+", "==", "<", ">", "concat" ]
eval_in_scope    scope ((SFcall name args):rest) =  (eval_in_scope new_scope [fbody]) ++ eval_in_scope scope rest
    where (SFdecl _ params fbody) = fetch scope name
          new_scope = fcallScope scope params (map (fetch scope) args)
eval_in_scope    scope (val:rest) = val : eval_in_scope scope rest

eval :: Program -> [SExpr]
eval    (Program prog) = eval_in_scope EmptyScope prog

eval_test :: IO ()
eval_test = myTest "eval" eval testcases
    where testcases = [
                        ((parse (lexer "(let (a 1) (let (b 2)(+ a b)))")),                                                                 [SNumber 3]),
                        ((parse (lexer "(let (a 1)(== a a))")),                                                                            [SNumber 1]),
                        ((parse (lexer "(let (a 2)(let (b 3)(== a b)))")),                                                                 [SNumber 0]),
                        ((parse (lexer "(let (a 1)(let (b 2)(< a b)))")),                                                                  [SNumber 1]),
                        ((parse (lexer "(let (a 1)(let (b 2)(> a b)))")),                                                                  [SNumber 0]),
                        ((parse (lexer "(let (a \"Hello \")(let (b \"world\")(concat a b)))")),                                            [SString "Hello world"]),
                        ((parse (lexer "(let (a 2) a)")),                                                                                  [SNumber 2]),
                        ((parse (lexer "(let (a c) a)")),                                                                                  [SSymbol "c"]),
                        ((parse (lexer "(let (a \"Hello\") a)")),                                                                          [SString "Hello"]),
                        ((parse (lexer "(let (a 14) (let (b 15) a))")),                                                                    [SNumber 14]),
                        ((parse (lexer "(let (pass \"pass\")(let (fail \"fail\")(let (val 1)(if val       pass fail))))")),                [SString "pass"]),
                        ((parse (lexer "(let (pass \"pass\")(let (fail \"fail\")(let (val \"\")(if val    fail pass))))")),                [SString "pass"]),
                        ((parse (lexer "(let (pass \"pass\")(let (fail \"fail\")(let (val \"heh\")(if val pass fail))))")),                [SString "pass"]),
                        ((parse (lexer "(fn (car a b) a)")),                                                                               []),
                        ((parse (lexer "(fn (car a b) a)(fn (cdr a b) b)(let (a 1)(let (b 2)(car a b)))(let (a 3)(let (b 4)(cdr a b)))")), [SNumber 1, SNumber 4]),
                        ((parse (lexer "(let (a 3)(let (b 4)(drop a b)))")),                                                               [SNumber 4]),
                        ((parse (lexer "(let (a #t) a)")),                                                                                 [SBoolean True]),
                        ((parse (lexer "(let (a #f) a)")),                                                                                 [SBoolean False]),
                        ((parse (lexer "")), [])
                      ]

run :: String -> IO ()
run filename = do
    contents <- readFile filename
    let ast = parse (lexer contents) in
        case (check ast) of
            []   -> putStrLn $ show $ eval ast
            errs -> mapM_ (putStrLn . show) errs

main :: IO ()
main = do
    lexer_test
    parse_test
    eval_test
    putStrLn "========"
    run "t.all"

