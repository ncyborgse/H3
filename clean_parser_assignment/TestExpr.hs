import qualified Dictionary
import Expr

main :: IO ()
main = do
       let n1 = testValue "1"
       let n2 = testValue "x"
       let n3 = testValue "x+y"
       let n4 = testValue "x-y-y"
       --let n21 = testValue "1/(2-y)" {-  Expr.value: division by 0 -}
       --let n31 = testValue "2+z"     {-  Expr.value: undefined variable z -}
       print n1
       print n2
       print n3
       print n4
       --print n21
       --print n31


{- Test for Expr-}
--module TestExpr where



dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty 

testValue string = value (fromString string) dict


