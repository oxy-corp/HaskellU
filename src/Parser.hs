module Parser (parseProgram) where

import AST
import Data.Char (isSpace)
import Text.Parsec 
  ( parse, try, (<|>), many, many1, oneOf, noneOf, char, string, spaces, newline, endOfLine, manyTill, anyChar, lookAhead, eof, skipMany )
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (between)
import Text.Parsec.Char (letter, alphaNum, digit)
import Control.Monad (void, when)

import qualified Data.List as L

-- Entry point: Parse an entire HSU file into a Program
parseProgram :: String -> Either String Program
parseProgram input =
  case parse program "HaskellU" input of
    Left err   -> Left (show err)
    Right prog -> Right prog

-- A program is a list of expressions (each can be on its own line)
-- program :: Parser Program
-- program = do
--   spaces
--   exprs <- many (parseHaskellU <* spaces)
--   eof
--   return $ Program exprs

whitespace :: Parser ()
whitespace = skipMany (oneOf " \t\r\n")

program :: Parser Program
program = do
  whitespace
  exprs <- many (parseHaskellU <* whitespace)
  eof
  return $ Program exprs

parseHaskellU :: Parser Expression
parseHaskellU = try parseOnEvent
            <|> try parseGlobalDecl
            <|> try parseLocalDecl
            <|> try parseVariableDecl
            <|> try parseWorkspace
            <|> try parseServiceRef
            <|> try parseEvent
            <|> try parseGetPlayer
            <|> try parseCondition
            <|> try parsePrint
            <|> try parseGameRef

parseVariableDecl :: Parser Expression
parseVariableDecl = do
  scope <- (string "local " >> return LocalScope)
           <|> (string "global " >> return GlobalScope)
  spaces
  varName <- many1 letter
  spaces
  _ <- string "="
  spaces
  value <- try parseGameRef
         <|> try parseWorkspace
         <|> try parseServiceRef
         <|> try parseEvent
         <|> try parseGetPlayer
         <|> try parseCondition
         <|> try parsePrint
  return $ VariableDecl scope varName value

-- Parse a workspace reference, e.g.:
--    workspace "Finish"
parseWorkspace :: Parser Expression
parseWorkspace = do
  _ <- string "workspace "
  name <- between (char '"') (char '"') (many1 alphaNum)
  return $ WorkspaceRef name

-- Parse a service reference, e.g.:
--    service gameInstance "Players"
parseServiceRef :: Parser Expression
parseServiceRef = do
  _ <- string "service "
  gameVar <- many1 letter
  spaces
  serviceName <- between (char '"') (char '"') (many1 letter)
  return $ ServiceRef serviceName (Print gameVar)

-- Parse an event declaration, e.g.:
--    event finishPart "Touched"
parseEvent :: Parser Expression
parseEvent = do
  _ <- string "event "
  obj <- many1 letter
  spaces
  eventName <- between (char '"') (char '"') (many1 letter)
  return $ Event obj (Print eventName)

-- Parse a getPlayer expression, e.g.:
--    getPlayer playersService hitEvent
parseGetPlayer :: Parser Expression
parseGetPlayer = do
  _ <- string "getPlayer "
  serviceVar <- many1 letter
  spaces
  eventVar <- many1 letter
  return $ GetPlayer (Print serviceVar) (Print eventVar)

-- Parse a condition, e.g.:
--    when ( ... ) do <action>
parseCondition :: Parser Expression
parseCondition = do
  _ <- string "when ("
  condition <- try parseServiceRef
           <|> try parseEvent
           <|> try parseGetPlayer
           <|> try parsePrint
           <|> try parseSimpleExpr
  _ <- string ") do"
  spaces
  action <- try parseServiceRef
         <|> try parseEvent
         <|> try parseGetPlayer
         <|> try parsePrint
         <|> try parseSimpleExpr
  return $ Condition condition action


-- Parse a print expression, e.g.:
--    print "Hello, player!"
parsePrint :: Parser Expression
parsePrint = do
  _ <- string "print "
  msg <- between (char '"') (char '"') (many (noneOf "\""))
  return $ Print msg

-- Parse a global declaration with a type annotation block:
--   gameInstance :: Global Scope -> Service
--   gameInstance = game
parseGlobalDecl :: Parser Expression
parseGlobalDecl = do
  varName <- many1 letter
  spaces
  _ <- string "::"
  spaces
  _ <- many1 (noneOf "\r\n")
  newline
  spaces
  varName2 <- string varName
  spaces
  _ <- char '='
  spaces
  value <- try parseGameRef
         <|> parseServiceRef
         <|> parseWorkspace
         <|> parseEvent
         <|> parseGetPlayer
         <|> parseCondition
         <|> parsePrint
  return $ VariableDecl GlobalScope varName2 value

parseLocalDecl :: Parser Expression
parseLocalDecl = do
  -- Declaration line (with type annotation)
  _ <- string "local "
  varName <- many1 letter
  spaces
  _ <- string "::"
  spaces
  _ <- many1 (noneOf "\r\n")  -- Consume and ignore the type annotation
  endOfLine                   -- Expect an end-of-line here
  skipMany (oneOf " \t")      -- Skip any indentation on the next line
  -- Assignment line: try matching an optional "local " prefix
  varName2 <- try (string "local " >> spaces >> string varName)
             <|> try (string varName)
  spaces
  _ <- char '='
  spaces
  value <- parseWorkspace
         <|> parseServiceRef
         <|> parseEvent
         <|> parseGetPlayer
         <|> parseCondition
         <|> parsePrint
         <|> parseGameRef
  return $ VariableDecl LocalScope varName2 value

-- Parse a lambda expression as:  (\_ -> <body as raw string>)
parseLambda :: Parser (String, Expression)
parseLambda = do
  _ <- char '\\'
  spaces
  param <- many1 (alphaNum <|> char '_')
  spaces
  _ <- string "->"
  spaces
  -- Instead of recursing to parse a full HSU expression,
  -- we grab everything until we hit a closing parenthesis.
  bodyStr <- manyTill anyChar (lookAhead (char ')'))
  return (param, Print bodyStr)

-- Helper: trim whitespace from both ends
trim :: String -> String
trim = L.dropWhileEnd isSpace . dropWhile isSpace

parseOnEvent :: Parser Expression
parseOnEvent = do
  _ <- string "onEvent "
  eventName <- many1 (noneOf " (")
  spaces
  _ <- char '('
  spaces
  lambdaSymbol <- anyChar  -- Read one character
  if lambdaSymbol /= '\\'
    then fail "Expected lambda backslash"
    else return ()
  spaces
  param <- many1 (alphaNum <|> char '_')
  spaces
  _ <- string "->"
  spaces
  body <- manyTill anyChar (char ')')
  return $ OnEvent eventName param (trim body)

-- Parse the literal "game"
parseGameRef :: Parser Expression
parseGameRef = do
  _ <- string "game"
  return GameRef

parseSimpleExpr :: Parser Expression
parseSimpleExpr = do
  -- This will collect one or more "words" (letters/digits/underscores)
  parts <- many1 (do { w <- many1 (alphaNum <|> char '_'); spaces; return w })
  return $ Identifier (unwords parts)
