module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

instance Parse T where
  parse = Statement.parse >-> Program   -- => parse = Program [Statement.T]
  toString (Program stmts) = concatMap Statement.toString stmts

exec :: T -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts


--- OLD CODE ---

-- newtype T = Program () -- to be defined

-- instance Parse T where
--   parse = error "Program.parse not implemented"
--   toString = error "Program.toString not implemented"
             
-- exec = error "Program.exec not implemented"
