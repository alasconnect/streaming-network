{-# LANGUAGE 
  LambdaCase
, OverloadedStrings 
#-}

module Data.Attoparsec.DSL.Template 
( fromFile
, parseTemplate
, Optional(..)
, Template(..)
, Expression(..)
, NodeToken(..)
, ValueToken(..)
, TokenType(..)
, TokenPipe(..)
, Directive(..)
, ChoiceOption(..)
) where

  import Control.Monad.IO.Class
  import Data.Char
  import Data.Text (Text)
  import qualified Data.Text.IO as TIO
  import qualified Data.Text as T
  import Data.Void
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import qualified Text.Megaparsec.Char.Lexer as L

  type Parser = Parsec Void Text
  type Optional = Bool

  data Template = Template [Expression]
    deriving (Show)

  data Expression
    = TokenExpression ValueToken
    | LiteralExpression Text
    | DirectiveExpression Directive
    deriving (Show)

  data NodeToken = NodeToken [Text]
    deriving (Show)

  data ValueToken = ValueToken [Text] Optional TokenType [TokenPipe]
    deriving (Show)
  
  data TokenType 
    = CharToken 
    | StringToken 
    | IntegerToken 
    | UnsignedIntegerToken 
    | DoubleToken  
    | UnsignedDoubleToken 
    | ScientificToken 
    | UnsignedScientificToken 
    | BooleanToken
    | ListToken TokenType
    deriving (Show)

  data TokenPipe = TrimPipe
    deriving (Show)

  data Directive 
    = SpacesDirective | NewLineDirective 
    | CharExistsDirective NodeToken Char 
    | StringExistsDirective NodeToken Text 
    | RepeatDirective NodeToken [Expression] 
    | OptionalDirective [Expression] 
    | ChoiceDirective [ChoiceOption]
    deriving (Show)

  data ChoiceOption = ChoiceOption [Expression]
    deriving (Show)

  fromFile :: MonadIO m => FilePath -> Text -> m (Either Text Template)
  fromFile p n = do
    t <- liftIO $ TIO.readFile p
    return $ parseTemplate n t

  parseTemplate :: Text -> Text -> Either Text Template
  parseTemplate n t = case result of
    Left err -> Left . T.pack . show $ err
    Right o  -> Right . Template $ o
    where 
      result = parse expressions (T.unpack n) t

  expressions :: Parser [Expression]
  expressions = manyTill (try sc *> try expression) eof

  expression :: Parser Expression
  expression = (tokenExpression <|> literalExpression <|> directiveExpression) <* sc

  literalExpression :: Parser Expression
  literalExpression = literalString >>= pure . LiteralExpression
  
  nodeToken :: Parser NodeToken
  nodeToken = char '$' *> tokenName >>= pure . NodeToken

  tokenExpression :: Parser Expression
  tokenExpression = do
    try (char '$')
    name <- tokenName
    optional <- withRecovery (\_ -> pure False) (try (char '?') *> pure True)
    tType <- withRecovery (\_ -> pure StringToken) tokenType
    pipes <- withRecovery (\_ -> pure []) tokenPipes
    return $ TokenExpression $ ValueToken name optional tType pipes

  tokenName :: Parser [Text]
  tokenName = sepBy (takeWhile1P Nothing isAlphaNum) (char '.')

  tokenType :: Parser TokenType
  tokenType = try (char ':')
    *>  ((string "char" *> pure CharToken)
    <|> (string "string" *> pure StringToken)
    <|> (string "int" *> pure IntegerToken)
    <|> (string "uint" *> pure UnsignedIntegerToken)
    <|> (string "double" *> pure DoubleToken)
    <|> (string "udouble" *> pure UnsignedDoubleToken)
    <|> (string "scientific" *> pure ScientificToken)
    <|> (string "uscientific" *> pure UnsignedScientificToken)
    <|> (string "bool" *> pure BooleanToken)
    <|> (char '[' *> tokenType <* char ']' >>= pure . ListToken))
  
  tokenPipes :: Parser [TokenPipe]
  tokenPipes = many tokenPipe

  tokenPipe :: Parser TokenPipe
  tokenPipe = try (char '|')
    *> string "trim"
    *> pure TrimPipe

  directiveExpression :: Parser Expression
  directiveExpression = try (char '#') *> directive >>= pure . DirectiveExpression
  
  directive :: Parser Directive
  directive = (string "spaces" *> pure SpacesDirective)
    <|> (string "newLine" *> pure NewLineDirective)
    <|> charExistsDirective
    <|> stringExistsDirective
    <|> repeatDirective
    <|> optionalDirective
    <|> choiceDirective
    
  repeatDirective :: Parser Directive
  repeatDirective = do
    string "repeat"
    nToken <- parens nodeToken
    exprs <- braces expressions
    return $ RepeatDirective nToken exprs
  
  optionalDirective :: Parser Directive
  optionalDirective = string "optional" 
    *> braces expressions 
    >>= pure . OptionalDirective
  
  choiceDirective :: Parser Directive
  choiceDirective = string "choice" 
    *> braces (many choiceDirectiveOption)
    >>= pure . ChoiceDirective
  
  choiceDirectiveOption :: Parser ChoiceOption
  choiceDirectiveOption = string "#option"
    *> braces expressions
    >>= pure . ChoiceOption
  
  charExistsDirective :: Parser Directive
  charExistsDirective = string "charExists" 
    *> parens charExistsDirectiveParams 
    >>= (\(n, c) -> return $ CharExistsDirective n c)

  charExistsDirectiveParams :: Parser (NodeToken, Char)
  charExistsDirectiveParams = do
    node <- nodeToken
    char ','
    charValue <- literalChar
    return (node, charValue)
  
  stringExistsDirective :: Parser Directive
  stringExistsDirective = string "charExists" 
    *> parens stringExistsDirectiveParams 
    >>= (\(n, s) -> return $ StringExistsDirective n s)

  stringExistsDirectiveParams :: Parser (NodeToken, Text)
  stringExistsDirectiveParams = do
    node <- nodeToken
    char ','
    stringValue <- literalString
    return (node, stringValue)

  literalChar :: Parser Char
  literalChar = quotes charBody

  literalString :: Parser Text
  literalString = quotes stringBody

  stringBody :: Parser Text
  stringBody = many charBody >>= pure . T.pack
  
  charBody :: Parser Char
  charBody = normalChar <|> escapedChar
    where 
      normalChar = satisfy (\x -> x /= '\\' && x /= '"')
      escapedChar = (string "\\\\" *> pure '\\') <|> (string "\\\"" *> pure '"')

  sc :: Parser ()
  sc = L.space space1 lineCmnt blockCmnt
    where
      lineCmnt  = L.skipLineComment "--"
      blockCmnt = L.skipBlockComment "/*" "*/"

  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme sc

  symbol :: Text -> Parser Text
  symbol = L.symbol sc

  quotes :: Parser a -> Parser a
  quotes = between (symbol "\"") (symbol "\"")

  braces :: Parser a -> Parser a
  braces = between (symbol "{") (symbol "}")

  parens :: Parser a -> Parser a
  parens = between (symbol "(") (symbol ")")

  integer :: Parser Integer
  integer = lexeme L.decimal
