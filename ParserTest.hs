module ParserTest where

import Test.HUnit
import qualified SubsParser as Parser
import SubsAst 

-- Test that strings are parsed correctly (Cannot contain ' character)    
test1 :: Test    
test1 = TestCase $ assertEqual ("The string to be parsed is valid")  (Right (Prog [ExprAsStm (String "TestString")])) (Parser.parseString "'TestString';")

test2 :: Test  
test2 = TestCase $ assertEqual ("The string to be parsed is invalid")  (Left Parser.NoParse) (Parser.parseString "'Test'invalidString;")  

-- Test that parsing numbers are ok. Numbers of more than 8 digits are not parsed
test3 :: Test  
test3 = TestCase $ assertEqual ("The number to be parsed is correct")  (Right (Prog [ExprAsStm (Number 124542)])) (Parser.parseString "124542;")  

test4 :: Test  
test4 = TestCase $ assertEqual ("The number to be parsed is correct")  (Right (Prog [ExprAsStm (Number $ -124542)])) (Parser.parseString "-124542;")  

test5 :: Test  
test5 = TestCase $ assertEqual ("The number to be parsed is incorrect")  (Left Parser.NoParse) (Parser.parseString "1245421453;")  

test6 :: Test  
test6 = TestCase $ assertEqual ("The number to be parsed is incorrect")  (Left Parser.NoParse) (Parser.parseString "-1245421453;")  

-- Test different functions of the grammar
test7 :: Test  
test7 = TestCase $ assertEqual ("The function to be parsed is correct")  (Right (Prog [ExprAsStm (Call "-" [Number 122,Number 122])])) (Parser.parseString "122 - 122;")  

test8 :: Test  
test8 = TestCase $ assertEqual ("The function to be parsed is correct")  (Right (Prog [ExprAsStm (Call "+" [Number 122,TrueConst])])) (Parser.parseString "122 + true;")  

test9 :: Test  
test9 = TestCase $ assertEqual ("The function to be parsed is correct")  (Right (Prog [ExprAsStm (Call "%" [Number 122,FalseConst])])) (Parser.parseString "122 % false;")  

test10 :: Test  
test10 = TestCase $ assertEqual ("The function to be parsed is incorrect")  (Left Parser.NoParse) (Parser.parseString "122 / 122;")  

-- Test over operator precedences 
test11 :: Test  
test11 = TestCase $ assertEqual ("Precedences to be parsed are wrong")  (Right (Prog [ExprAsStm (Comma (Call "<" [Number 42,Call "===" [Number 31,Call "*" [Number 2,Number 14]]]) (Call "+" [Number 13,Call "<" [Number 12,Call "===" [Number 3,Call "%" [Number 12,Number 2]]]]))])) (Parser.parseString " 42 < 31 === 2 * 14 , 13 + 12 < 3 === 12 % 2 ;") 

-- Test whitespace handling 
test12 :: Test  
test12 = TestCase $ assertEqual ("Whitespace handling is wrong")  (Right (Prog [ExprAsStm (Call "%" [Number 122,FalseConst])])) (Parser.parseString "  122 % false;")  

test13 :: Test  
test13 = TestCase $ assertEqual ("Whitespace handling is wrong")  (Right (Prog [ExprAsStm (Call "%" [Number 122,FalseConst])])) (Parser.parseString " 122 %   false;     ")  

test14 :: Test  
test14 = TestCase $ assertEqual ("Whitespace handling is wrong")  (Right (Prog [ExprAsStm (Call "%" [Number 122,FalseConst])])) (Parser.parseString "  122 % false   ;")  

test15 :: Test  
test15 = TestCase $ assertEqual ("Whitespace handling is wrong")  (Right (Prog [ExprAsStm (Call "%" [Number 122,Var "varxs"])])) (Parser.parseString "  122 %  varxs;")  

tests :: Test  
tests = TestList [TestLabel "Strings" $ TestList [test1, test2], TestLabel "Numbers" $ TestList [test3, test4, test5, test6] , 
                  TestLabel "Operations" $ TestList [test7, test8, test9, test10], TestLabel "Precedences" test11, TestLabel "Spaces" $ TestList [test12, test13, test14, test15] ]

main :: IO Counts                  
main = runTestTT tests
    
