--{-# OPTIONS_GHC -fno-warn-no-unused-do-bind #-}

module NanoParsec where

import           Data.Char
import           Control.Monad
import           Control.Applicative

-- A parser is a function that takes an input stream of characters
-- and yields a parse tree by applying its logic over sections
-- of the character stream to build up a composite data structure
-- for the AST.
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- Running the function will result in traversing the stream of
-- character, yielding a value of type `a` or failing with a
-- parse error for malformed input, or by not consuming
-- the entire input stream.
runParser :: Parser a -> String -> a
runParser m s = case parse m s of
  [(res, [])] -> res
  [(_  , rs)] -> error "Parser did not consume entire stream."
  _           -> error "Parser error."

-- We advance the parser by extracting a single char from
-- the stream and returning it in a tuple containing itself
-- and the rest of the stream. The parser logic will
-- either transform it in some portion of the output or
-- advance the stream and proceed
item :: Parser Char
item = Parser $ \s -> case s of
  []       -> []
  (c : cs) -> [(c, cs)]

-- Take a parse operation and compose it over the result of
-- a second parse function.
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

-- Inject a single pure value as the result, without
-- reading from the parse stream.
unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [ (f a, b) | (a, b) <- cs s ])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) =
    Parser (\s -> [ (f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1 ])

instance Monad Parser where
  return = unit
  (>>=)  = bind

-- Add logic for trying multiple parse functions over the
-- same stream and handling failure and rollover.
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (const [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s -> case parse p s of
  []  -> parse q s
  res -> res

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

-- Automatically derives
-- some :: f a -> f [a] (One or more)
-- many :: F a -> f [a] (Zero or more)
instance Alternative Parser where
  empty = mzero
  (<|>) = option

--Add functionality for checking whether the current
--character in the stream matches a preficate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then unit c else Parser (const [])

-- So far we have the defined the entire core of the parser combinator.
-- All higher order behavior can be written on top of this logic.

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

-- Parse one or more occurences of `p` separated by `op`
-- and return a value obtained by recursing until failure 
-- on the left hand side of the stream.
-- Can be used to parse left-recursive grammar.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
 where
  rest a =
    (do
        f <- op
        b <- p
        rest (f a b)
      )
      <|> return a

-- We can use `satisfy` to write combinators to detect
-- the presence of specific common patterns of characters.
char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string []       = return []
string (c : cs) = do
  char c
  string cs
  return (c : cs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s  <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

-- End of parsec logic

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a + eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

int :: Parser Expr
int = Lit <$> number

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = infixOp "+" Add <|> infixOp "-" Sub

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

factor :: Parser Expr
factor = int <|> parens expr

term :: Parser Expr
term = factor `chainl1` mulop

expr :: Parser Expr
expr = term `chainl1` addop

run :: String -> Expr
run = runParser expr

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ eval $ run a
