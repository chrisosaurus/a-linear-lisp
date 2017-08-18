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
           | SFuncVal String [String] SExpr
           | SLet String SExpr SExpr
           | SIf String SExpr SExpr
           | SFcall String [String]
           -- drop var 'String' and continue eval in SExpr
           | SDrop String SExpr
           | SCons SExpr SExpr
           -- unpack cons into two parts and evaluate SExpr in context
           | SSplit String String String SExpr
    deriving (Show, Eq)

data SStmt = SStmtExpr SExpr
           | SFdecl String [String] SExpr
    deriving (Show, Eq)

data Program = Program [SStmt]
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
          parseParamsInner   strings     tokens = error $ show tokens

parseSExpr :: [Token] -> (SExpr, [Token])
parseSExpr (TLparen : (TSymbol "split") : TLparen : (TSymbol name1) : (TSymbol name2) : (TSymbol name3) : TRparen : rest) = ((SSplit name1 name2 name3 body), rrest)
    where (body, (TRparen:rrest)) = parseSExpr rest
parseSExpr (TLparen : (TSymbol "drop") : (TSymbol name) : rest) = ((SDrop name body), rrest)
    where (body, (TRparen:rrest)) = parseSExpr rest

parseSExpr (TLparen : (TSymbol "let") : TLparen : (TSymbol name) : rest) = ((SLet name sexpr body), rrrest)
    where (sexpr, (TRparen:rrest)) = parseSExpr rest
          (body, (TRparen:rrrest)) = parseSExpr rrest

parseSExpr (TLparen : (TSymbol "if")  : (TSymbol cond): rest) = ((SIf cond body_then body_else), rest2)
    where (body_then, rest1) = parseSExpr rest
          (body_else, (TRparen:rest2)) = parseSExpr rest1
parseSExpr (TLparen : (TSymbol name)  : rest) = ((SFcall name args), rrest)
    where (args, rrest) = parseParams rest
parseSExpr (val : rest) = ((parseValue val), rest)

parseSStmt :: [Token] -> (SStmt, [Token])
parseSStmt (TLparen : (TSymbol "fn")  : TLparen : (TSymbol name) : rest) = ((SFdecl name params body), rest2)
    where (params, rest1) = parseParams rest
          (body,   (TRparen:rest2)) = parseSExpr rest1
parseSStmt tokens = ((SStmtExpr sexpr), rest)
    where (sexpr, rest) = parseSExpr tokens


parse :: [Token] -> Program
parse [] = Program []
parse tokens = Program (parse_inner tokens)
    where parse_inner :: [Token] -> [SStmt]
          parse_inner    [] = []
          parse_inner    tokens       = sstmt : parse_inner rest
            where (sstmt, rest) = parseSStmt tokens

parse_test :: IO ()
parse_test = myTest "parse" parse testcases
    where testcases = [
                        ((lexer "(let (a 1) (let (b 2) (+ a b)))"),
                         (Program [(SStmtExpr (SLet "a" (SNumber 1) (SLet "b" (SNumber 2) (SFcall "+" ["a", "b"]))))])),
                        ([(TString "hello world")],
                         (Program [(SStmtExpr (SString "hello world"))])),
                        ((lexer "(if a b c)"),
                         (Program [(SStmtExpr (SIf "a" (SSymbol "b") (SSymbol "c")))])),
                        ((lexer "(let (a b) c)"),
                         (Program [(SStmtExpr (SLet "a" (SSymbol "b") (SSymbol "c")))])),
                        ((lexer "(fn (foo a b) (+ a b))"),
                         (Program [(SFdecl "foo" ["a", "b"] (SFcall "+" ["a", "b"]))])),
                        ((lexer "(let (a \"Hello\") (let (b \" world\") (concat a b)))"),
                         (Program [(SStmtExpr (SLet "a" (SString "Hello") (SLet "b" (SString " world") (SFcall "concat" ["a", "b"]))))])),
                        ((lexer "(let (a 3)(let (b 4)(drop a b)))",
                         (Program [(SStmtExpr (SLet "a" (SNumber 3) (SLet "b" (SNumber 4) (SDrop "a" (SSymbol "b")))))]))),
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
checkVars :: SExpr -> ([String],[String]) -> Either String ([String], [String])
checkVars (SSymbol name) vars = use vars name
checkVars (SCons _ _) vars = Right vars
checkVars (SString _) vars = Right vars
checkVars (SNumber _) vars = Right vars
checkVars (SBoolean _) vars = Right vars

checkVars (SDrop name body) vars = case (use vars name) of
                                          (Left err) -> Left err
                                          (Right new_vars) -> checkVars body new_vars
checkVars (SLet name expr body) vars = case (checkVars expr vars) of
                                              (Left err) -> Left err
                                              (Right new_vars) -> case (declare new_vars name) of
                                                  (Left err) -> Left err
                                                  (Right new_vars2) -> checkVars body new_vars2
checkVars (SSplit name1 name2 name3 body) vars = case (use vars name3) of
                                                      (Left err) -> Left err
                                                      (Right new_vars1) -> case (declare new_vars1 name1) of
                                                          (Left err) -> Left err
                                                          (Right new_vars2) ->  case (declare new_vars2 name2) of
                                                                          (Left err) -> Left err
                                                                          (Right new_vars3) -> checkVars body new_vars3
checkVars (SIf cond body_then body_else) vars = case (use vars cond) of
                                                      (Left err) -> Left err
                                                      (Right vars1) -> case (checkVars body_then vars1) of
                                                                      (Left err) -> Left err
                                                                      -- we need to verify that every leaf uses all vars
                                                                      (Right ([], _)) -> checkVars body_else vars1
                                                                      (Right (unused, _)) -> Right (unused, [])
-- check args are not repeated (used more than once in this call)
checkVars (SFcall _ args) _ | containsRepeated args = Left $ "Error: argument list is not unique '" ++ (show args) ++ "'"
-- check that every arg is defined and previous unused
checkVars (SFcall _ args) vars = foldMy use (Right vars) args


checkVarsStmt :: SStmt -> ([String], [String]) -> Either String ([String], [String])
checkVarsStmt (SStmtExpr sexpr) vars = checkVars sexpr vars
checkVarsStmt (SFdecl _ params body) _ | containsRepeated params = Left $ "Error: parameter list is not unique '" ++ (show params) ++ "'"
checkVarsStmt (SFdecl _ params body) vars = case (foldMy declare (Right vars) params) of
                                            (Left err) -> Left err
                                            (Right new_vars) -> checkVars body new_vars

removeNothing :: [Maybe String] -> [String]
removeNothing [] = []
removeNothing (Nothing : rest) = removeNothing rest
removeNothing ((Just s) : rest) = s : removeNothing rest


checkVarsStmts :: [SStmt] -> [String]
checkVarsStmts stmts = removeNothing $ inner stmts ([], [])
    where inner :: [SStmt] -> ([String], [String]) -> [Maybe String]
          inner [] _ = []
          inner (s:stmts) vars = case (checkVarsStmt s vars) of
                (Left err) -> (Just err) : (inner stmts vars)
                (Right new_vars) -> inner stmts new_vars

-- check static properties
check :: Program -> [String]
check (Program sstmts) = checkVarsStmts sstmts

check_test :: IO ()
check_test = myTest "check" check testcases
    where testcases = [
                        ((parse (lexer "(let (a 1) (let (b 2)(+ a b)))")),                                                                 []),
                        ((parse (lexer "(let (a 1) (let (b 1) (== a b)))")),                                                               []),
                        ((parse (lexer "(let (a 2)(let (b 3)(== a b)))")),                                                                 []),
                        ((parse (lexer "(let (a 1)(let (b 2)(< a b)))")),                                                                  []),
                        ((parse (lexer "(let (a 1)(let (b 2)(> a b)))")),                                                                  []),
                        ((parse (lexer "(let (a \"Hello \")(let (b \"world\")(concat a b)))")),                                            []),
                        ((parse (lexer "(let (a 2) a)")),                                                                                  []),
                        ((parse (lexer "(let (a \"Hello\") a)")),                                                                          []),
                        ((parse (lexer "(let (a 14) (let (b 15) (drop b a)))")),                                                           []),
                        ((parse (lexer "(let (pass \"pass\")(let (fail \"fail\")(let (val #t)(if val (drop fail pass) (drop pass fail)))))")), []),
                        ((parse (lexer "(let (pass \"pass\")(let (fail \"fail\")(let (val #f)(if val (drop pass fail) (drop fail pass)))))")), []),
                        ((parse (lexer "(fn (car a b) (drop b a))")),                                                                      []),
                        ((parse (lexer "(fn (car a b) (drop b a))(fn (cdr a b) (drop a b))(let (a 1)(let (b 2)(car a b)))(let (a 3)(let (b 4)(cdr a b)))")), []),
                        ((parse (lexer "(let (a 3)(let (b 4)(drop a b)))")),                                                               []),
                        ((parse (lexer "(let (a #t) a)")),                                                                                 []),
                        ((parse (lexer "(let (a #f) a)")),                                                                                 []),
                        ((parse (lexer "(let (a 4) (let (b 5) (let (cmp (< a b)) cmp)))")),                                                []),
                        ((parse (lexer "(let (a 4) (let (b (clone a)) b))")),                                                              []),
                        ((parse (lexer "(let (a 4) (let (b #t) (let (c (cons a b)) c)))")),                                                []),
                        ((parse (lexer "(let (a 4) (let (b #t) (let (c (cons a b)) (split (ca cb c) (drop cb ca)))))")),                   []),
                        ((parse (lexer "(let (a 4) (let (b #t) (let (c (cons a b)) (split (ca cb c) (drop ca cb)))))")),                   []),
                        ((parse (lexer "(let (a 4) (let (b (clone a)) (split (bl br b) (drop bl br))))")),                                 []),
                        ((parse (lexer "")),                                                                                               [])
                      ]

data Binding = Binding String SExpr

data Scope = EmptyScope
           | Scope Binding Scope

insert :: Scope -> String -> SExpr -> Scope
insert scope str val = Scope (Binding str val) scope

fetch :: Scope -> String -> SExpr
fetch EmptyScope str = error $ "Failed to find string: " ++ str
fetch (Scope (Binding str1 val1) _) str | str == str1 = val1
fetch (Scope _ scope) str = fetch scope str

removeFromScope :: Scope -> String -> Scope
removeFromScope EmptyScope _ = EmptyScope
removeFromScope (Scope b@(Binding s _) scope) string
    = if s == string
        then removeFromScope scope string
        else (Scope b (removeFromScope scope string))

truthy :: SExpr -> Bool
truthy (SBoolean contents) = contents

-- create the new scope for an fcall
fcallScope :: Scope -> [String] -> [SExpr] -> Scope
fcallScope scope [] [] = scope
fcallScope scope (s:strings) (v:vals) = fcallScope (insert scope s v) strings vals

unpackString :: SExpr -> String
unpackString (SString s) = s

unpackNumber :: SExpr -> Int
unpackNumber (SNumber i) = i

primop :: Scope -> String -> [String] -> SExpr
primop scope "cons" (left:right:[]) = SCons lleft rright
    where lleft = fetch scope left
          rright = fetch scope right
primop scope "clone" (arg:[]) = SCons val val
    where val = fetch scope arg
primop scope "+" args = SNumber answer
    where answer = foldl (+) 0 parts
          parts = map (\x -> unpackNumber (fetch scope x)) args
primop scope "==" (left:right:[]) = if (lleft == rright) then (SBoolean True) else (SBoolean False)
    where lleft = fetch scope left
          rright = fetch scope right
primop scope "<" (left:right:[]) = if (lleft < rright) then (SBoolean True) else (SBoolean False)
    where (SNumber lleft) = fetch scope left
          (SNumber rright) = fetch scope right
primop scope ">" (left:right:[]) = if (lleft > rright) then (SBoolean True) else (SBoolean False)
    where (SNumber lleft) = fetch scope left
          (SNumber rright) = fetch scope right
primop scope "concat" args = SString answer
    where answer = foldl (++) "" parts
          parts = map (\x -> unpackString (fetch scope x)) args
primop _ name _ = error $ "Unknown primop: " ++ show name

eval_in_scope :: Scope -> SExpr -> SExpr
eval_in_scope    scope (SSymbol name) =  fetch scope name
eval_in_scope    scope (SDrop name body) = eval_in_scope new_scope body
    where new_scope = removeFromScope scope name
eval_in_scope    scope (SSplit name1 name2 name3 body) = eval_in_scope split_scope_2 body
    where (SCons left right) = fetch scope name3
          split_scope_1 = insert scope name1 left
          split_scope_2 = insert split_scope_1 name2 right

eval_in_scope    scope (SLet name val body) = eval_in_scope let_scope body
    where nval = eval_in_scope let_scope val
          let_scope = insert scope name nval

eval_in_scope    scope (SIf cond body_then body_else) = if (truthy (fetch scope cond))
                                                               then (eval_in_scope scope body_then)
                                                               else (eval_in_scope scope body_else)
eval_in_scope    scope (SFcall name args) | name `elem` primops = primop scope name args
    where primops = [ "cons", "clone", "+", "==", "<", ">", "concat" ]
eval_in_scope    scope (SFcall name args) =  eval_in_scope new_scope fbody
    where (SFuncVal _ params fbody) = fetch scope name
          new_scope = fcallScope scope params (map (fetch scope) args)
eval_in_scope    scope val = val

eval_fdecl :: Scope -> SStmt -> Scope
eval_fdecl scope (SFdecl name params body) = new_scope
    where new_scope = insert scope name val
            where val = (SFuncVal name params body)

eval_program :: Scope -> [SStmt] -> [SExpr]
eval_program _ [] = []
eval_program scope ((SStmtExpr sexpr):xs) = (eval_in_scope scope sexpr) : eval_program scope xs
eval_program scope (fdecl:xs) = eval_program new_scope xs
    where new_scope = eval_fdecl scope fdecl

eval :: Program -> [SExpr]
eval    (Program stmts)   = eval_program EmptyScope stmts

eval_test :: IO ()
eval_test = myTest "eval" eval testcases
    where testcases = [
                        ((parse (lexer "(let (a 1) (let (b 2)(+ a b)))")),                                                                 [SNumber 3]),
                        ((parse (lexer "(let (a 1) (let (b 1) (== a b)))")),                                                               [SBoolean True]),
                        ((parse (lexer "(let (a 2)(let (b 3)(== a b)))")),                                                                 [SBoolean False]),
                        ((parse (lexer "(let (a 1)(let (b 2)(< a b)))")),                                                                  [SBoolean True]),
                        ((parse (lexer "(let (a 1)(let (b 2)(> a b)))")),                                                                  [SBoolean False]),
                        ((parse (lexer "(let (a \"Hello \")(let (b \"world\")(concat a b)))")),                                            [SString "Hello world"]),
                        ((parse (lexer "(let (a 2) a)")),                                                                                  [SNumber 2]),
                        ((parse (lexer "(let (a \"Hello\") a)")),                                                                          [SString "Hello"]),
                        ((parse (lexer "(let (a 14) (let (b 15) (drop b a)))")),                                                           [SNumber 14]),
                        ((parse (lexer "(let (pass \"pass\")(let (fail \"fail\")(let (val #t)(if val (drop fail pass) (drop pass fail)))))")), [SString "pass"]),
                        ((parse (lexer "(let (pass \"pass\")(let (fail \"fail\")(let (val #f)(if val (drop pass fail) (drop fail pass)))))")), [SString "pass"]),
                        ((parse (lexer "(fn (car a b) (drop b a))")),                                                                      []),
                        ((parse (lexer "(fn (car a b) (drop b a))(fn (cdr a b) (drop a b))(let (a 1)(let (b 2)(car a b)))(let (a 3)(let (b 4)(cdr a b)))")), [SNumber 1, SNumber 4]),
                        ((parse (lexer "(let (a 3)(let (b 4)(drop a b)))")),                                                               [SNumber 4]),
                        ((parse (lexer "(let (a #t) a)")),                                                                                 [SBoolean True]),
                        ((parse (lexer "(let (a #f) a)")),                                                                                 [SBoolean False]),
                        ((parse (lexer "(let (a 4) (let (b 5) (let (cmp (< a b)) cmp)))")),                                                [SBoolean True]),
                        ((parse (lexer "(let (a 4) (let (b (clone a)) b))")),                                                              [SCons (SNumber 4) (SNumber 4)]),
                        ((parse (lexer "(let (a 4) (let (b #t) (let (c (cons a b)) c)))")),                                                [SCons (SNumber 4) (SBoolean True)]),
                        ((parse (lexer "(let (a 4) (let (b #t) (let (c (cons a b)) (split (ca cb c) (drop cb ca)))))")),                   [SNumber 4]),
                        ((parse (lexer "(let (a 4) (let (b #t) (let (c (cons a b)) (split (ca cb c) (drop ca cb)))))")),                   [SBoolean True]),
                        ((parse (lexer "(let (a 4) (let (b (clone a)) (split (bl br b) (drop bl br))))")),                                 [SNumber 4]),
                        ((parse (lexer "(let (a 1) (let (b 2) (let (c 3) (+ a b c))))")),                                                  [SNumber 6]),
                        ((parse (lexer "(let (a \"a\") (let (b \"b\") (let (c \"c\") (concat a b c))))")),                                 [SString "abc"]),
                        ((parse (lexer "")), [])
                      ]

run :: String -> [String]
run contents =
    let ast = parse (lexer contents) in
        case (check ast) of
            []   -> map show $ eval ast
            errs -> map show errs

run_on_file :: String -> IO ()
run_on_file filename = do
    contents <- readFile filename
    mapM_ putStrLn (run contents)

main :: IO ()
main = do
    lexer_test
    parse_test
    check_test
    eval_test
    putStrLn "========"
    run_on_file "t.all"

