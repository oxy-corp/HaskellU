module AST where

data Scope = LocalScope | GlobalScope
  deriving (Show)

data Expression
  = VariableDecl Scope String Expression
  | WorkspaceRef String
  | ServiceRef String Expression  -- References game services like "Players"
  | Event String Expression
  | GetPlayer Expression Expression  -- Requires both PlayersService and event
  | Condition Expression Expression
  | Print String
  | GameRef                        -- Represents the literal "game"
  | OnEvent String String String   -- onEvent: event name, lambda parameter, raw lambda body
  | Identifier String              -- New: represents a simple identifier or compound expression
  deriving (Show)

data Program = Program [Expression]
  deriving (Show)
