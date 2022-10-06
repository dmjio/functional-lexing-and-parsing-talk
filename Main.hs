{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Data.Char
import Debug.Trace                (traceShow)
import Data.List
import Control.Applicative
import Data.Foldable
import Control.Monad.Trans.Except

-- | AST
data JSON
  = Array [JSON]
  | Object [(String, JSON)]
  | Number Double
  | String String
  | Bool Bool
  | Null
  deriving (Show, Eq)

data LexerError = Unexpected String
  deriving (Show, Eq)

newtype Lexer a
  = Lexer
  { runLexer :: String -> Either LexerError (a, String)
  }

instance Functor Lexer where
  fmap f (Lexer l) = Lexer $ \input -> do
    (a,r) <- l input
    pure (f a, r)

instance Applicative Lexer where
  pure x = Lexer $ \input ->
    pure (x, input)

  Lexer l1 <*> Lexer l2 = Lexer $ \input -> do
    (f, x) <- l1 input
    (a, y) <- l2 x
    pure (f a, y)

instance Alternative Lexer where
  empty = Lexer $ \input -> Left (Unexpected input)
  Lexer l1 <|> Lexer l2 = Lexer $ \input ->
    case (l1 input, l2 input) of
      (res, Left _) -> res
      (Left _, res) -> res
      (Right (a,s), Right (b,t)) ->
        if length s <= length t
        then Right (a,s)
        else Right (b,t)

data Token
  = WhiteSpace
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Comma
  | Colon
  | Quote
  | Period
  | Minus
  | Plus
  | Exponent
  | Digits String
  | StringToken String
  | Identifier String
  deriving (Show, Eq)

showToken :: Token -> String
showToken (StringToken s) = s
showToken (Identifier s) = s
showToken WhiteSpace = " "
showToken (Digits c)  = c
showToken Exponent   = "e"
showToken LBracket   = "["
showToken RBracket   = "]"
showToken LBrace     = "{"
showToken RBrace     = "}"
showToken Comma      = ","
showToken Colon      = ":"
showToken Quote      = "\""
showToken Period     = "."
showToken Minus      = "-"
showToken Plus       = "+"

-- | cool
string' :: String -> Lexer String
string' = traverse char

lexJSON :: String -> Either LexerError [Token]
lexJSON str =
  case runLexer jsonLexer str of
    Right (x,"") -> Right x
    Right (x,xs) -> Left (Unexpected xs)
    Left e -> Left e

identifier :: Lexer String
identifier = some (lower <|> upper)
  where
    lower = satisfy isLower
    upper = satisfy isUpper

whitespace :: Lexer String
whitespace =
  some $ oneOf
  [ char ' '
  , char '\n'
  , char '\r'
  , char '\t'
  ]

digit :: Lexer String
digit = some (satisfy isDigit) -- satisfy (`elem` ['0'..'9'])

-- | Fetches any character
-- item :: Lexer Char
-- item = Lexer $ \input ->
--   case input of
--     (x:xs) -> Right (x,xs)
--     xs -> Left (Unexpected xs)

-- | Returns all characters except those specified
anyBut :: String -> Lexer String
anyBut cs = many (satisfy (`notElem` cs))

-- | Remove WhiteSpace
-- WhitSpace is defined as a ' ', \n, \r, \t
postProcess :: [Token] -> [Token]
postProcess = filter (/= WhiteSpace)

satisfy :: (Char -> Bool) -> Lexer Char
satisfy f = Lexer $ \case
  x : xs | f x -> Right (x,xs)
  xs -> Left (Unexpected xs)

char :: Char -> Lexer Char
char c = satisfy (==c)

-- | Core combinator, input run equally on all combinators
oneOf :: Alternative f => [f a] -> f a
oneOf = asum -- foldr (<|>) empty

jsonLexer :: Lexer [Token]
jsonLexer = postProcess <$> many tokens
  where
    tokens
      = oneOf
      [ StringToken <$> (char '"' *> anyBut "\"" <* char '"')
      , WhiteSpace <$ whitespace
      , Identifier <$> identifier
      , LBrace     <$ char '{'
      , RBrace     <$ char '}'
      , LBracket   <$ char '['
      , RBracket   <$ char ']'
      , Colon      <$ char ':'
      , Comma      <$ char ','
      , Period     <$ char '.'
      , Minus      <$ char '-'
      , Plus       <$ char '+'
      , Digits      <$> digit
      ]

prettyTokens :: [Token] -> String
prettyTokens = concatMap showToken

demoLexer = do
  tokens <- lexJSON <$> readFile "ledger.json"
  case tokens of
    Right ts -> do
      -- putStrLn (prettyTokens ts)
      mapM_ print [head ts]
    Left e -> do
      putStrLn "oops"
      -- print e

data ParseError
  = UnexpectedParse String
  | LexicalError LexerError
  | Ambiguous [(JSON, [Token])]
  | NoParses
  deriving (Show, Eq)

newtype Parser a
  = Parser
  { runParser :: [Token] -> [(a, [Token])]
  }

instance Functor Parser where
  fmap f (Parser run) = Parser $ \input ->
    [ (f x, rest)
    | (x, rest) <- run input
    ]

instance Applicative Parser where
  pure x = Parser $ \s -> pure (x,s)
  Parser f <*> Parser g = Parser $ \input -> do
    (k, s) <- f input
    (x, t) <- g s
    pure (k x, t)

instance Alternative Parser where
  empty = Parser $ \input -> []
  Parser f <|> Parser g = Parser $ \s ->
    f s <|> g s

instance Monad Parser where
  return = pure
  Parser m >>= f = Parser $ \tokens -> do
    (x, ts) <- m tokens
    runParser (f x) ts

instance MonadFail Parser where
  fail _ = Parser $ \_ -> []

satisfyToken :: (Token -> Bool) -> Parser Token
satisfyToken f = Parser $ \input ->
  case input of
    t : ts | f t -> [(t,ts)]
    _ -> []

token :: Token -> Parser Token
token t = satisfyToken (==t)

tokenString :: Parser Token
tokenString = satisfyToken $ \t ->
  case t of
    StringToken _ -> True
    _ -> False

digits :: Parser Token
digits = satisfyToken $ \t ->
  case t of
    Digits _ -> True
    _ -> False

comma :: Parser Token
comma = token Comma

colon :: Parser Token
colon = token Colon

lbrace :: Parser Token
lbrace = token LBrace

rbrace :: Parser Token
rbrace = token RBrace

lbracket :: Parser Token
lbracket = token LBracket

rbracket :: Parser Token
rbracket = token RBracket

quote :: Parser Token
quote = token Quote

period :: Parser Token
period = token Period

-- | Peeks at the next token, doesn't consume
peek :: Parser Token
peek = Parser $ \tokens ->
  case tokens of
    (x:xs) -> [(x, x:xs)]
    _ -> []

-- | Pulls the next token, no matter what it is
next :: Parser Token
next = Parser $ \token ->
  case token of
    (x:xs) -> [(x,xs)]
    _ -> []

-- | Skips token
skip :: Parser ()
skip = () <$ next

-- | Helpful combinators
sepBy1 :: Alternative m => m sep -> m a -> m [a]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)

sepBy :: Alternative m => m sep -> m a -> m [a]
sepBy sep p = sepBy1 sep p <|> pure []

null :: Parser JSON
null = Null <$ token (Identifier "null")

bool :: Parser JSON
bool = true <|> false
  where
    true = Bool True <$
      token (Identifier "true")

    false = Bool False <$
      token (Identifier "false")

number :: Parser JSON
number = do
  -- | Illustrates look-ahead
  modify <- do
    shouldNegate <- (==Minus) <$> peek
    if shouldNegate
      then skip >> pure negate
      else pure id

  Digits wholeNumber <- digits -- requires MonadFail
  fractional <- getFractional <|> pure mempty
  exponent <- getExponent <|> pure mempty
  pure $ Number
       $ modify
       $ read (wholeNumber <> fractional <> exponent)
    where
      getFractional = do
        token Period
        Digits xs <- digits
        pure ('.' : xs)

      getExponent = do
        Identifier e <- exponent
        sign <- token Minus <|> token Plus
        Digits exp <- digits
        pure (e <> showToken sign <> exp)
          where
            exponent = oneOf
              [ token (Identifier "e")
              , token (Identifier "E")
              ]

between :: Applicative f => f a -> f b -> f c -> f (b, c)
between c l r = liftA2 (,) l (c *> r)

enclosed :: Applicative f => f a -> f b -> f c -> f c
enclosed l r x = l *> x <* r

string :: Parser String
string = do
  StringToken t <- tokenString
  pure t

array :: Parser JSON
array =
  Array <$>
    enclosed lbracket rbracket
      (sepBy comma jsonParser)

object :: Parser JSON
object =
  Object <$>
    enclosed lbrace rbrace
      (sepBy comma
        (between colon string jsonParser))

jString :: Parser JSON
jString = String <$> string

jsonParser :: Parser JSON
jsonParser
  = oneOf
  [ array
  , object
  , jString
  , Main.null
  , bool
  , number
  ]

parseJSON :: String -> Either ParseError JSON
parseJSON str =
  case lexJSON str of
    Left e ->
      Left (LexicalError e)
    Right [] ->
      Right (String mempty)
    Right tokens ->
      parseJSONFromTokens tokens

parseJSONFromTokens :: [Token] -> Either ParseError JSON
parseJSONFromTokens tokens =
  case runParser jsonParser tokens of
    [(x,[])] -> pure x
    x : xs -> Left (Ambiguous (x:xs))
    [] -> Left NoParses

main :: IO ()
main = do
  str <- getContents
  case lexJSON str of
    Left e ->
      print e
    Right tokens -> do
      mapM_ print tokens
      case parseJSONFromTokens tokens of
        Left e -> do
          print e
        Right ast -> do
          putStrLn "Success"
          print ast
