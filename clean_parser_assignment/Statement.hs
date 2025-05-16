module Statement(T, parse, toString, fromString, exec) where

import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import Data.Text.Internal.Fusion (Step(Skip))

type T = Statement
data Statement =
    Assignment String Expr.T            -- tilldela variabelnamn & uttryck, tex x := 5
    | Skip                              -- gör inget, tom sats som hoppas över
    | Begin [Statement]                 -- block med flera statements i följd begin...end
    | If Expr.T Statement Statement     -- if Expr.T then statement1 else statement2
    | While Expr.T Statement            -- while ( Expr.T ) do Statement
    | Read String                       -- läser tar från input till variabel
    | Write Expr.T                      -- skriver ut ett uttrycks värde
    deriving Show                       -- implementatio av show-klassen för Statement (värde till sträng)


----- Parsing Functions (3b) -----

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- "skip;...blabla..."
skipStatement = accept "skip" #- require ";" >-> const Skip

-- begin bla bla bla bla end -> Begin [bla bla bla bla]
beginStatement = accept "begin" -# statements #- require "end" >-> Begin

-- "if expression then statement1 else statement2" -> buildIF ((expression, statement1), statement2)
ifStatement = accept "if" -# Expr.parse #- require "then" # parse
         #- require "else" # parse >-> buildIf
buildIf ((cond, thn), els) = If cond thn els

whileStatement = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (cond, body) = While cond body

readStatement = accept "read" -# word #- require ";" >-> Read

writeStatement = accept "write" -# Expr.parse #- require ";" >-> Write


----- The Function exec (3d) -----

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []

exec (Skip : stmts) dict input = exec stmts dict input

exec (Begin stmtsBlock : stmts) dict input = exec (stmtsBlock ++ stmts) dict input

exec (If cond thenStmts elseStmts : stmts) dict input = 
    if Expr.value cond dict > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While cond body : stmts) dict input =
    if Expr.value cond dict > 0
    then exec (body : While cond body : stmts) dict input
    else exec stmts dict input

exec (Read _:_) _ [] =
    error "No more input to read"

exec (Read var : stmts) dict (i:is) = 
    let newDict = Dictionary.insert (var, i) dict
    in exec stmts newDict is

exec (Write expr : stmts) dict input =
  Expr.value expr dict : exec stmts dict input  -- lägget till output:en först i listan och kör vidare med resten av statementsen


----- Define Parse (3c) -----

instance Parse Statement where
  parse = assignment ! skipStatement ! beginStatement ! ifStatement ! whileStatement ! readStatement ! writeStatement
  toString = error "Statement.toString not implemented"

statement :: Parser Statement
statement = parse

statements :: Parser [Statement]
statements = iter statement
