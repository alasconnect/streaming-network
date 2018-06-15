{-# LANGUAGE 
  LambdaCase
, OverloadedStrings 
#-}

module Data.Attoparsec.DSL.Builder where

  import qualified Data.Attoparsec.DSL.Template as T
  import qualified Data.Attoparsec.DSL.Mapper as M

  import Control.Applicative hiding (empty)
  import Data.Attoparsec.ByteString
  import Data.Attoparsec.ByteString.Char8
  import Data.Attoparsec.Combinator
  import Data.ByteString (ByteString)
  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Char8 as BS8
  import qualified Data.HashMap as HM
  import Data.Scientific (Scientific, fromFloatDigits)
  import Data.Text (Text, unpack, pack, strip)
  import qualified Data.Text as TXT
  import Data.Word (Word8)
  import GHC.Float
  
  type Object = HM.Map ByteString Value

  data Value 
    = ObjectValue !Object 
    | Array ![Value]
    | Value !Primitive

  data Primitive 
    = String !ByteString
    | Number !Scientific
    | Bool !Bool
  
  empty :: Object
  empty = HM.empty

  expand :: Object -> Parser M.Value
  expand obj = pure $ expandObject obj

  expandObject :: Object -> M.Value
  expandObject = M.Object . (HM.map expandValue)

  expandValue :: Value -> M.Value
  expandValue (ObjectValue obj) = expandObject obj
  expandValue (Array arr) = M.Array $ fmap expandValue arr
  expandValue (Value (String str)) = M.String str
  expandValue (Value (Number num)) = M.Number num
  expandValue (Value (Bool b)) = M.Bool b

  buildParser :: T.Template -> Parser M.Value
  buildParser (T.Template exprs) = expressionsParser exprs empty >>= expand

  expressionsParser :: [T.Expression] -> Object -> Parser Object
  expressionsParser = go
      where
        go :: [T.Expression] -> Object -> Parser Object
        go [] h     = pure h
        go (x:xs) h = expressionParser h x xs >>= go xs

  expressionParser :: Object -> T.Expression -> [T.Expression] -> Parser Object
  expressionParser h (T.TokenExpression v) exprs      = tokenParser h v exprs
  expressionParser h (T.LiteralExpression l) _        = string (BS8.pack (unpack l)) *> pure h
  expressionParser h (T.DirectiveExpression d) exprs  = directiveParser h d exprs
      
  tokenParser :: Object -> T.ValueToken -> [T.Expression] -> Parser Object
  tokenParser h (T.ValueToken t True tt tp) exprs  = optionalTokenParser tt h t tp exprs
  tokenParser h (T.ValueToken t False tt tp) exprs = requiredTokenParser tt h t tp exprs
  
  optionalTokenParser :: T.TokenType -> Object -> [Text] -> [T.TokenPipe] -> [T.Expression] -> Parser Object
  optionalTokenParser tt h t tp exprs = try (requiredTokenParser tt h t tp exprs)

  requiredTokenParser :: T.TokenType -> Object -> [Text] -> [T.TokenPipe] -> [T.Expression] -> Parser Object
  requiredTokenParser (T.ListToken tt) h t tp exprs = tokenValueParser (T.ListToken tt) exprs >>= pipesParser tp >>= (flip (setToken t) h)
  requiredTokenParser tt h t tp exprs               = tokenValueParser tt exprs >>= pipesParser tp >>= (flip (setToken t) h)

  pipesParser :: [T.TokenPipe] -> Value -> Parser Value
  pipesParser [] v = pure v
  pipesParser (x:xs) v = pipeParser x v >>= pipesParser xs

  pipeParser :: T.TokenPipe -> Value -> Parser Value
  pipeParser T.TrimPipe (Value (String v)) = pure . Value . String . BS8.pack . unpack . strip . pack . BS8.unpack $ v
  pipeParser T.TrimPipe _ = fail "Trim can only be applied to strings."
    
  tokenValueParser :: T.TokenType -> [T.Expression] -> Parser Value
  tokenValueParser T.CharToken _                = anyChar >>= pure . Value . String . BS8.singleton
  tokenValueParser T.StringToken []             = takeByteString >>= pure . Value . String
  tokenValueParser T.StringToken (x:xs)         = manyTill anyChar (expressionParser empty x xs) >>= pure . Value . String . BS8.pack
  tokenValueParser T.IntegerToken _             = signed decimal >>= pure . Value . Number . fromIntegral 
  tokenValueParser T.UnsignedIntegerToken _     = decimal >>= pure . Value . Number . fromIntegral 
  tokenValueParser T.DoubleToken _              = signed double >>= pure . Value . Number . fromFloatDigits 
  tokenValueParser T.UnsignedDoubleToken _      = double >>= pure . Value . Number . fromFloatDigits 
  tokenValueParser T.ScientificToken _          = signed scientific >>= pure . Value . Number
  tokenValueParser T.UnsignedScientificToken _  = scientific >>= pure . Value . Number
  tokenValueParser T.BooleanToken _             = booleanParser
  tokenValueParser (T.ListToken tt) exprs       = tokenValueParser tt exprs >>= pure . Array . (: [])

  booleanParser :: Parser Value
  booleanParser = 
    (
      char '1' *> pure True
      <|> stringCI "true" *> pure True
      <|> char '0' *> pure False
      <|> stringCI "false" *> pure False
    ) >>= pure . Value . Bool 
  
  directiveParser :: Object -> T.Directive -> [T.Expression] -> Parser Object
  directiveParser h T.SpacesDirective _                                   = skipSpace *> pure h
  directiveParser h T.NewLineDirective _                                  = endOfLine *> pure h
  directiveParser h (T.CharExistsDirective (T.NodeToken token) c) _       = (char c *> setToken token (Value . Bool $ True) h) <|> (setToken token (Value . Bool $ False) h)
  directiveParser h (T.StringExistsDirective (T.NodeToken token) s) _     = (string (BS8.pack (unpack s)) *> setToken token (Value . Bool $ True) h) <|> (setToken token (Value . Bool $ False) h)
  directiveParser h (T.RepeatDirective (T.NodeToken token) rexprs) []     = repeatParser token rexprs h endOfInput
  directiveParser h (T.RepeatDirective (T.NodeToken token) rexprs) (x:xs) = repeatParser token rexprs h (expressionParser empty x xs)
  directiveParser h (T.OptionalDirective exprs) _                         = option h (expressionsParser exprs h)
  directiveParser h (T.ChoiceDirective choices) _                         = choice (fmap (choiceParser h) choices)

  repeatParser :: [Text] -> [T.Expression] -> Object -> Parser a -> Parser Object
  repeatParser token exprs h p = manyTill (expressionsParser exprs empty) (lookAhead p) 
    >>= pure . (map ObjectValue) 
    >>= (flip (setToken token) h) . Array

  choiceParser :: Object -> T.ChoiceOption -> Parser Object
  choiceParser h (T.ChoiceOption exprs) = expressionsParser exprs h

  setToken :: [Text] -> Value -> Object -> Parser Object
  setToken k v h = pure $ setProperty bsk v h
    where
      bsk = fmap (BS8.pack . unpack) k
    
  setProperty :: [ByteString] -> Value -> Object -> Object
  setProperty [] _ h = h
  setProperty (x:xs) v h = case c of
    Nothing -> HM.insert x (mkProperty xs v) h
    Just cv -> HM.insert x (setValueProperty xs v cv) h
    where c = HM.lookup x h
      
  setValueProperty :: [ByteString] -> Value -> Value -> Value
  setValueProperty [] v _ = v
  setValueProperty k v (ObjectValue obj) = ObjectValue $ setProperty k v obj
  setValueProperty (x:xs) v _ = ObjectValue $ HM.insert x (mkProperty xs v) empty

  addProperty :: [ByteString] -> Value -> Object -> Object
  addProperty [] _ h = h
  addProperty (x:xs) v h = HM.insert x (mkProperty xs v) h

  mkProperty :: [ByteString] -> Value -> Value
  mkProperty [] v = v
  mkProperty (x:xs) v = ObjectValue $ HM.insert x (mkProperty xs v) empty
