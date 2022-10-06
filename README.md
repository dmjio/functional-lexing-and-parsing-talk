---
author: David Johnson
title: Functional Lexing and Parsing
date: 10/05/2022
---

### Lexing

<img width="758" alt="image" src="https://user-images.githubusercontent.com/875324/194130021-8cee562c-7219-4929-b47a-8daf291229ef.png">

---

### Parsing

<img width="757" alt="image" src="https://user-images.githubusercontent.com/875324/194130271-80cff813-de98-4a1b-9de7-c75219c92d58.png">

---

### Frontend Compiler pipeline

<img width="768" alt="image" src="https://user-images.githubusercontent.com/875324/194138657-39195b46-3bef-4cff-9b26-06b74a198fbc.png">

---

### Lexing and Parsing libraries in Haskell ecosystem

- Lexing:
  - `alex`
	- Used By:
	   - `ghc`
	   - `dhall`
	   - `purescript`

- Parsing:
  - LALR (Look ahead left-to-right).
	- `happy`
	  - Used By:
		- `ghc`
  - Recursive Descent and CPS-based.
	- `megaparsec`
	- `ReadP`
	- `parsec`
	- `attoparsec`
	- `trifecta`

---

### Functional Parsing

From classic Graham Hutton [Monadic Parser Combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf).

[Graham Hutton Video](https://www.youtube.com/watch?v=dDtZLm7HIJs)

<img width="754" alt="image" src="https://user-images.githubusercontent.com/875324/194143209-c53d3cee-dd11-43d1-be08-79c4b9d37960.png">

---

### Functional Parsing

What does it mean to parse in Haskell?

```haskell
type Parser
  = String
  -- ^ input string
  -> Tree
  -- ^ parse tree
```

```haskell
type Parser
  = String
  -- ^ input
  -> (Tree, String)
  -- ^ parse tree, leftovers

-- type State s a = s -> (a,s) -- similar, huh?
```

```haskell
type Parser a
  = String
  -- ^ input
  -> (a, String)
  -- ^ polymorphic 'a', w/ leftovers
```

```haskell
type Parser a
  = String
  -- ^ input
  -> [(a, String)]
  -- ^ polymorphic parse, w/ leftovers, failure, ambiguity

-- type StateT s m a = s -> m (a, s) -- for transformer inclined
-- type Parser a = StateT [Token] [] a ~ [Token] -> [(a,[Token])]

```

---

### Functional Parsing formulation

Parsers can operate over lexed tokens, not just `String`, `Text`, `ByteString`, etc.

```haskell
newtype Parser a
  = Parser
  { runParser :: [Token] -> [(a, [Token])]
  }
```

### Functional Parser formulation

```haskell
-- | Required for the Applicative, can just use `liftA`
instance Functor Parser where
  fmap f (Parser run) = Parser $ \input ->
	[ (f x, rest)
	| (x, rest) <- run input
	]

instance Applicative Parser where
  pure x = Parser $ \s -> pure (x,s)
  -- ^ Puts any 'a' into a Parser, `input` becomes the `leftovers`.
  Parser f <*> Parser g = Parser $ \input -> do
	(k, s) <- f input -- parse the first input string into a result and leftovers
	(x, t) <- g s -- parse the second input string int a result and leftovers
	pure (k x, t) -- apply the function from the first parse to the value of the second parse

-- | Alternative, [] is to indicate a parse failure.
-- Must be an `Applicative` as well.
-- (<|>) to combine parsers, and to show ambiguous parses.
instance Alternative Parser where
  empty = Parser $ \input -> []
  Parser f <|> Parser g = Parser $ \s ->
	f s <|> g s -- (<|>) is just (++) for list.

-- | To sequence parsing, classic monadic parser combinator formulation
instance Monad Parser where
  return = pure
  Parser m >>= f = Parser $ \tokens -> do
	(x, ts) <- m tokens
	runParser (f x) ts

instance MonadFail Parser where
  fail _ = Parser $ \_ -> [] -- Required if we do hard pattern matching in a Monad
```

---

### Alternative

```haskell
-- | A monoid on applicative functors.
--
-- If defined, 'some' and 'many' should be the least solutions
-- of the equations:
--
-- * @'some' v = (:) 'Prelude.<$>' v '<*>' 'many' v@
--
-- * @'many' v = 'some' v '<|>' 'pure' []@
class Applicative f => Alternative f where
	-- | The identity of '<|>'
	empty :: f a
	-- | An associative binary operation
	(<|>) :: f a -> f a -> f a

	-- | One or more.
	some :: f a -> f [a]
	some v = some_v
	  where
		many_v = some_v <|> pure []
		some_v = liftA2 (:) v many_v

	-- | Zero or more.
	many :: f a -> f [a]
	many v = many_v
	  where
		many_v = some_v <|> pure []
		some_v = liftA2 (:) v many_v
```

---

### Parser primitives

`Parser` is good, but we need a primitive to faciliate construction of combinators.

```haskell
satisfyToken :: (Token -> Bool) -> Parser Token
satisfyToken f = Parser $ \input ->
  case input of
	t : ts | f t -> [(t,ts)]
	_ -> []

token :: Token -> Parser Token
token t = satisfyToken (==t)

item :: Lexer Char
item = Lexer $ \input ->
  case input of
	(x:xs) -> Right (x,xs)
	xs -> Left (Unexpected xs)
```

```haskell
-- perfect match
λ> runParser (satisfyToken (==Identifier "c")) [Identifier "c"]
[(Identifier "c",[])]

-- perfect fail
λ> runParser (satisfyToken (==Identifier "c")) [Identifier "d"]
[]

-- perfect match, w/ leftovers
λ> runParser (satisfyToken (==Identifier "c")) [Identifier "c",Identifier "c"]
[(Identifier "c",[Identifier "c"])]
```


---

### Parser combinators

```haskell
-- | Peeks at the next token, doesn't consume
peek :: Parser Token
peek = Parser $ \tokens ->
  case tokens of
	(x:xs) -> [(x, x:xs)]
	_ -> []

-- | Pulls the next token, no matter what it is
next :: Parser Token
next = Parser $ \tokens ->
  case tokens of
	(x:xs) -> [(x,xs)]
	_ -> []

-- | Skips token
skip :: Parser ()
skip = () <$ next

-- | Helpful combinators, when parsing strings like "1,2,3" (sepBy (char ','))
sepBy1 :: Alternative m => m sep -> m a -> m [a]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)

sepBy :: Alternative m => m sep -> m a -> m [a]
sepBy sep p = sepBy1 sep p <|> pure []
```

---

### Functional Lexing

`Alternative` implements the maximal munch rule.

```haskell
newtype Lexer a
  = Lexer
  { runLexer :: String -> Either LexerError (a, String)
  }

data LexerError = Unexpected String
  deriving (Show, Eq)

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

-- Maximal munch
instance Alternative Lexer where
  empty = Lexer $ \input -> Left (Unexpected input)
  Lexer l1 <|> Lexer l2 = Lexer $ \input ->
	case (l1 input, l2 input) of
	  (res, Left _) -> res
	  (Left _, res) -> res
	  (Right (a,s), Right (b,t)) ->
		if length s <= length t -- If l1 has less leftovers than l2, it has consumed more, so choose it.
		then Right (a,s)
		else Right (b,t)
```

---

### Lexer primitives + combinators

```haskell
satisfy :: (Char -> Bool) -> Lexer Char
satisfy f = Lexer $ \case
  x : xs | f x -> Right (x,xs)
  xs -> Left (Unexpected xs)
```

```haskell
char :: Char -> Lexer Char
char c = satisfy (==c)

string :: String -> Lexer String
string = traverse char

-- | Core combinator, input runs equally on all combinators
oneOf :: Alternative f => [f a] -> f a
oneOf = asum -- foldr (<|>) empty

-- asum :: (Foldable t, Alternative f) => t (f a) -> f a

-- | Returns all characters except those specified
anyBut :: String -> Lexer String
anyBut cs = many (satisfy (`notElem` cs))
```

---

### JSON Tokens

```haskell
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
```

---

### Demo

Parsing some `JSON` whitespace

<img width="688" alt="image" src="https://user-images.githubusercontent.com/875324/194129861-531e96ca-dbea-4880-96c0-a397811ba2db.png">

---

### JSON lexer combinators

```haskell
satisfy :: (Char -> Bool) -> Lexer Char
satisfy f = Lexer $ \case
  x : xs | f x -> Right (x,xs)
  xs -> Left (Unexpected xs)

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
```

---

### JSON lexer

```haskell
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

-- | Remove WhiteSpace
-- WhitSpace is defined as a ' ', \n, \r, \t
postProcess :: [Token] -> [Token]
postProcess = filter (/= WhiteSpace)
```

### Lex JSON

```haskell
lexJSON :: String -> Either LexerError [Token]
lexJSON str =
  case runLexer jsonLexer str of
	Right (x,"") -> Right x
	Right (x,xs) -> Left (Unexpected xs)
	Left e -> Left e
```

```bash
$ runghc Main.hs <<< '[1,2,3]'
LBracket
Digits "1"
Comma
Digits "2"
Comma
Digits "3"
RBracket
```

---

### Parsing into a JSON AST

```haskell
-- | AST
data JSON
  = Array [JSON]
  | Object [(String, JSON)]
  | Number Double
  | String String
  | Bool Bool
  | Null
  deriving (Show, Eq)
```

---

### JSON Parser combinators

```haskell
satisfyToken :: (Token -> Bool) -> Parser Token
satisfyToken f = Parser $ \input ->
  case input of
	t : ts | f t -> [(t,ts)]
	_ -> []

token :: Token -> Parser Token
token t = satisfyToken (==t)

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
```

---

### Top-level JSON Parser combinators

```haskell
bool :: Parser JSON
bool = true <|> false
  where
	true = Bool True <$
	  token (Identifier "true")

	false = Bool False <$
	  token (Identifier "false")
```

```haskell
null :: Parser JSON
null = Null <$ token (Identifier "null")
```

---

### Parsing JSON numbers

Parsing some `JSON`, `Number`

<img width="686" alt="image" src="https://user-images.githubusercontent.com/875324/194129648-fd581567-7d59-42df-b589-1b7d74d4afce.png">

---


### Parsing JSON numbers

```haskell
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
```

---

### Demo

Parsing some `JSON`, `Object`

<img width="866" alt="image" src="https://user-images.githubusercontent.com/875324/194129415-8b452b36-dd78-405a-93cb-6bb90c28ccd4.png">

---

### Demo

Parsing some `JSON`, `Array`

<img width="860" alt="image" src="https://user-images.githubusercontent.com/875324/194129471-72bbaae0-afa2-4e0c-b337-b0ea96073e9a.png">

---

### Parsing JSON Array and Objects

```haskell

-- | Useful for executing a parser between two parsers (discarding the results)
between :: Applicative f => f a -> f b -> f c -> f (b, c)
between c l r = liftA2 (,) l (c *> r)

-- More combinators, useful for "[ ]" and "{ }" syntax
enclosed :: Applicative f => f a -> f b -> f c -> f c
enclosed l r x = l *> x <* r

-- Parsing a JSON string
string :: Parser String
string = do
  StringToken t <- tokenString
  pure t

tokenString :: Parser Token
tokenString = satisfyToken $ \t ->
  case t of
	StringToken _ -> True
	_ -> False

array :: Parser JSON
array = Array <$> enclosed lbracket rbracket (sepBy comma jsonParser)

object :: Parser JSON
object = Object <$> enclosed lbrace rbrace (sepBy comma (between colon string jsonParser))

jString :: Parser JSON
jString = String <$> string
```

---


### Demo, Parsing + Lexing JSON

Parsing some `JSON` `Value`

<img width="696" alt="image" src="https://user-images.githubusercontent.com/875324/194127968-96f38551-9c42-484b-8b73-6edfd8e402b8.png">

---

### Top-level JSON parser

```haskell
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
```

---

### Execute pipeline

```haskell
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
```
---

### Result

```haskell
$ runghc Main.hs <<< '{ "name" : "joe", "hasPets" : false, "age" : 22.3e+0 }'
LBrace
StringToken "name"
Colon
StringToken "joe"
Comma
StringToken "hasPets"
Colon
Identifier "false"
Comma
StringToken "age"
Colon
Digits "22"
Period
Digits "3"
Identifier "e"
Plus
Digits "0"
RBrace
Success
Object [("name",String "joe"),("hasPets",Bool False),("age",Number 22.3)]
```

---

### Other Parser implemenations

---

### Parsec

```haskell
-- | @ParsecT s u m a@ is a parser with stream type @s@, user state type @u@,
-- underlying monad @m@ and return type @a@.  Parsec is strict in the user state.
-- If this is undesirable, simply use a data type like @data Box a = Box a@ and
-- the state type @Box YourStateType@ to add a level of indirection.

newtype ParsecT s u m a
	= ParsecT {unParser :: forall b .
				 State s u
			  -> (a -> State s u -> ParseError -> m b) -- consumed ok
			  -> (ParseError -> m b)                   -- consumed err
			  -> (a -> State s u -> ParseError -> m b) -- empty ok
			  -> (ParseError -> m b)                   -- empty err
			  -> m b
			 }
```

---

### Megaparsec

```haskell
-- | @'ParsecT' e s m a@ is a parser with custom data component of error
-- @e@, stream type @s@, underlying monad @m@ and return type @a@.
newtype ParsecT e s m a = ParsecT
  { unParser ::
	  forall b.
	  State s e ->
	  (a -> State s e -> Hints (Token s) -> m b) -> -- consumed-OK
	  (ParseError s e -> State s e -> m b) ->		-- consumed-error
	  (a -> State s e -> Hints (Token s) -> m b) -> -- empty-OK
	  (ParseError s e -> State s e -> m b) ->		-- empty-error
	  m b
  }
```

---


### Trifecta

```haskell
newtype Parser a = Parser
  { unparser :: forall r.
	   (a -> Err -> It Rope r)
	-> (Err -> It Rope r)
	-> (a -> Set String -> Delta -> ByteString -> It Rope r)  -- committed success
	-> (ErrInfo -> It Rope r)                                 -- committed err
	-> Delta
	-> ByteString
	-> It Rope r
  }
```

---

### Attoparsec

```haskell
newtype Parser i a = Parser {
	  runParser :: forall r.
				   State i -> Pos -> More
				-> Failure i (State i)   r
				-> Success i (State i) a r
				-> IResult i r
	}

type family State i
type instance State ByteString = B.Buffer
type instance State Text = T.Buffer

type Failure i t   r = t -> Pos -> More -> [String] -> String -> IResult i r
type Success i t a r = t -> Pos -> More -> a -> IResult i r

data IResult i r = Fail i [String] String | Partial (i -> IResult i r) | Done i r
```

---

### ReadP

```haskell
newtype ReadP a = R (forall b . (a -> P b) -> P b)

data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | Final (NonEmpty (a,String))
  deriving Functor -- ^ @since 4.8.0.0
```

---

### The End

Thanks !

Special thanks to @cronokirby's haskell-in-haskell pedagogical implementation.
