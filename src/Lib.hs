module Lib (parseAndGenerate) where

import Parser (parseProgram)
import CodeGen (generateLuauProgram)

parseAndGenerate :: String -> Either String String
parseAndGenerate input =
  case parseProgram input of
    Left err   -> Left err
    Right prog -> Right (generateLuauProgram prog)
