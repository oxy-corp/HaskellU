module CodeGen (generateLuauProgram) where

import AST

-- Generate Luau code for a full program (list of expressions)
generateLuauProgram :: Program -> String
generateLuauProgram (Program exprs) = unlines (map generateLuau exprs)

-- Generate Luau code for each expression
generateLuau :: Expression -> String
-- Global variable declarations:
generateLuau (VariableDecl GlobalScope var GameRef) =
  "local " ++ var ++ " = game"
generateLuau (VariableDecl GlobalScope var (ServiceRef serviceName (Print gameVar))) =
  "local " ++ var ++ " = " ++ gameVar ++ ":GetService(\"" ++ serviceName ++ "\")"
-- Local variable declarations for workspace references:
generateLuau (VariableDecl LocalScope var (WorkspaceRef name)) =
  "local " ++ var ++ " = game.Workspace." ++ name
-- For event declarations, assume they are variable assignments:
-- e.g., hitEvent = event finishPart "Touched" becomes:
generateLuau (VariableDecl _ var (Event obj (Print eventName))) =
  "local " ++ var ++ " = " ++ generateLuau (WorkspaceRef (dropPrefix obj)) ++ "." ++ eventName
  where
    -- dropPrefix removes any trailing whitespace; adjust as needed.
    dropPrefix s = s  
-- For getPlayer, assume the pattern:
-- getPlayer playersService hitEvent, which is used in a variable declaration.
generateLuau (VariableDecl _ var (GetPlayer (Print serviceVar) (Print eventVar))) =
  "local " ++ var ++ " = " ++ serviceVar ++ ":GetPlayerFromCharacter(" ++ eventVar ++ ".Parent)"
-- Catch-all for any other variable declarations:
generateLuau (VariableDecl _ var expr) =
  "local " ++ var ++ " = " ++ generateLuau expr
-- For WorkspaceRef not in a declaration:
generateLuau (WorkspaceRef name) = "game.Workspace." ++ name
-- For ServiceRef not in a declaration:
generateLuau (ServiceRef serviceName (Print gameVar)) =
  gameVar ++ ":GetService(\"" ++ serviceName ++ "\")"
-- For GameRef:
generateLuau GameRef = "game"
-- For Event and GetPlayer when not wrapped in a VariableDecl, output nothing:
generateLuau (Event _ _) = ""
generateLuau (GetPlayer _ _) = ""
-- For Print:
generateLuau (Print message) = "print(\"" ++ message ++ "\")"
-- For any other Expression:
generateLuau _ = ""
