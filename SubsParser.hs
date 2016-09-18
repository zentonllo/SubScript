module SubsParser (
  ParseError(..),
  parseString,
  parseFile
) where


import SubsAst
import Control.Applicative hiding (Const)
import Data.Char
-- I'll use munch and munch1 defined in this document (I assume that it would be nicer 
-- that they should be included in SimpleParse.hs but I prefer not to modify this library since is the original I took from the lectures)
import SimpleParse hiding (munch1, munch)


{-


-------------------
-------------------
Original Grammar (v1.3)
This parser is running under version 1.3 of the grammar provided in the exam paper

Program ::= Stms
Stms ::= e | Stm ’;’ Stms
Stm ::= ’var’ Ident AssignOpt | Expr
AssignOpt ::= e | ’=’ Expr1
Expr ::= Expr ’,’ Expr | Expr1
Expr1 ::= Number
        | String
        | ’true’
        | ’false’
        | ’undefined’
        | Expr1 ’+’ Expr1
        | Expr1 ’-’ Expr1
        | Expr1 ’*’ Expr1
        | Expr1 ’%’ Expr1
        | Expr1 ’<’ Expr1
        | Expr1 ’===’ Expr1
        | Ident AfterIdent
        | ’[’ Exprs ’]’
        | ’[’ ’for’ ’(’ Ident ’of’ Expr1 ’)’ ArrayCompr Expr1 ’]’
        | ’(’ Expr ’)’
AfterIdent ::= e
            | ’=’ Expr1
            | FunCall
FunCall ::= ’.’ Ident FunCall
            | ’(’ Exprs ’)’
Exprs ::= e
         | Expr1 CommaExprs
CommaExprs ::= e
            | ’,’ Expr1 CommaExprs
ArrayCompr ::= e
            | ’if’ ’(’ Expr1 ’)’ ArrayCompr
            | ’for’ ’(’ Ident ’of’ Expr1 ’)’ ArrayCompr

-------------------------------------
-------------------------------------
Changed Grammar

With this new grammar I avoid left recursion as well as I provide each operator the right priority

This change will make the parser easier (Maybe I could have considered the function sepBy instead)

Stms ::= e | Stm Stms
Stm ::= 'var' Ident AssignOpt ';'
        | Expr ';'
        
I call Terminal expressions to all that expressions that don't make the parser go recursively

TerminalExpr ::=  Number
                | String
                | ’true’
                | ’false’
                | ’undefined’
                | Ident
                | Ident = FunCall
                | ’[’ Exprs ’]’
                | ’[’ ’for’ ’(’ Ident ’of’ Expr ’)’ ArrayCompr Expr ’]’
                | ’(’ Expr ’)’

Adapted grammar in order to give each operator its priority. Notice how expressions from Topt4 could go to Topt2 and get an Epsilon.
This is valid since we parse an expression from Topt5
                
Expr ::= Expr1 ',' Expr | Expr1
Expr1 ::= Topt5 
Topt5 ::= Ident '=' Topt5 | Topt Topt4 
Topt4 ::= '===' Topt Topt4 | Topt3
Topt3 ::= '<' Topt Topt3 | Topt2
Topt2 ::= '+' Topt Topt2 | '-' Topt Topt2 | e
Topt ::= TerminalExpr T
T ::= '*' TerminalExpr T | '%' TerminalExpr T | e

-}


-- Taken from SalsaInterp.hs (Assignment 2)
data ParseError = AmbiguousParse
                  | NoParse
                deriving (Show, Eq)
                
-- List of reserved words        
reservedWords :: [Ident]
reservedWords = ["true", "false", "undefined", "for", "of", "if"]                
                
-- Special characters not valid in a String (Disallow characters to be part of a Subscript String)                
specChar :: String
specChar = "'"

-- Taken from Salsa SalsaInterp.hs (Assignment 2) 
-- I remove the spaces from the end of the input so our parser would parse correctly
-- Otherwise, it won't do it, since it doesn't remove spaces after the last ';'              
parseString :: String -> Either ParseError Program
parseString s = case parseEof parseProgram (trim s) of 
                    [] -> Left  NoParse
                    [(p,_)] -> Right p
                    _ -> Left AmbiguousParse

-- Taken from https://en.wikipedia.org/wiki/Trimming_(computer_programming)#Haskell                    
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace                    
                    
parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = fmap parseString $ readFile path


parseProgram :: Parser Program 
parseProgram = do
                l <- parseStms
                return $ Prog l

-- Recall that the program might be empty so I use munch                
parseStms :: Parser [Stm]
parseStms = munch parseStm


parseStm :: Parser Stm
parseStm = (do
            symbol "var"
            many1 space
            ident <- parseIdent
            assigned <- parseAssignOpt
            symbol ";"
            return $ VarDecl ident assigned)
            <|>
            (do
             expr <- parseExpr
             symbol ";"
             return $ ExprAsStm expr)
             
             
parseTerminalExpr :: Parser Expr
parseTerminalExpr = parseNumberExpr
                    <|>
                    parseStringExpr
                    <|>
                    (do
                     symbol "true"
                     return TrueConst)
                    <|>
                    (do
                     symbol "false"
                     return FalseConst)
                    <|>
                    (do
                     symbol "undefined"
                     return Undefined)
                     <|>
                     (do 
                      ident <- token $ parseIdent
                      return $ Var ident 
                     )
                     <|>
                     (do 
                       ident <- token $ parseIdent
                       parseFunCall ident
                     )
                    <|>
                    (do
                     symbol "["
                     lExprs <- parseExprs
                     symbol "]"
                     return $ Array lExprs)        
                    <|>
                    (do
                     symbol "["
                     symbol "for"
                     symbol "("
                     ident <- token $ parseIdent
                     symbol "of"
                     expr <- parseExpr1
                     symbol ")"
                     arrCompr <- parseArrayCompr
                     expr' <- parseExpr1
                     symbol "]"
                     return $ Compr (ident, expr, arrCompr) expr' 
                     ) 
                    <|>
                    (do 
                     symbol "("
                     expr <- parseExpr
                     symbol ")"
                     return expr)
                     

parseAssignOpt :: Parser (Maybe Expr)
parseAssignOpt = return Nothing
                 <|>
                 (do 
                  symbol "="
                  expr <- parseExpr1
                  return $ Just expr)
                  


parseExpr :: Parser Expr
parseExpr = chainl1 parseExpr1 ( symbol "," >> return Comma)


parseExpr1 :: Parser Expr
parseExpr1 = parseTopt5 


parseTopt5 :: Parser Expr
parseTopt5  = (do 
                ident <- token $ parseIdent
                symbol "="
                expr <- parseTopt5
                return $ Assign ident expr
               )
                <|>
               (do 
                expr <- parseTopt 
                parseTopt4 expr
                )
                  
parseTopt4 :: Expr -> Parser Expr
parseTopt4 expr = (do 
                   symbol "==="
                   expr' <- parseTopt
                   expr'' <- parseTopt4 expr'
                   return $ Call "===" [expr, expr'']
                    )
                  <|>
                  parseTopt3 expr


parseTopt3 :: Expr -> Parser Expr
parseTopt3 expr = (do 
                   symbol "<"
                   expr' <- parseTopt
                   expr'' <- parseTopt4 expr'
                   return $ Call "<" [expr, expr'']
                    )
                <|>
                  parseTopt2 expr
                  
parseTopt2 :: Expr -> Parser Expr 
parseTopt2 expr = return expr 
                <|>
                (do 
                   symbol "+"
                   expr' <- parseTopt
                   expr'' <- parseTopt4 expr'
                   return $ Call "+" [expr, expr'']
                    )
                <|>
                (do 
                   symbol "-"
                   expr' <- parseTopt
                   expr'' <- parseTopt4 expr'
                   return $ Call "-" [expr, expr'']
                    )

parseTopt :: Parser Expr 
parseTopt = do expr <- parseTerminalExpr
               parseT expr
 
parseT :: Expr -> Parser Expr
parseT expr = return expr
            <|>
            (do
             symbol "*"
             expr' <- parseTerminalExpr
             expr'' <- parseT expr'
             return $ Call "*" [expr, expr''])
            <|>
            (do
             symbol "%"
             expr' <- parseTerminalExpr
             expr'' <- parseT expr'
             return $ Call "%" [expr, expr''])
             
             

                        
                        
parseFunCall :: Ident -> Parser Expr
parseFunCall ident = (do
                       char '.'
                       ident' <- parseIdent
                       symbol "("
                       lExprs <- parseExprs
                       symbol ")"
                       return $ Call (ident ++ "." ++ ident') lExprs
                       )
                      <|>
                      (do
                       symbol "."
                       ident' <- parseIdent
                       parseFunCall (ident ++ "." ++ ident') ) 
 

parseExprs :: Parser [Expr]
parseExprs = return []
             <|>
             (do
              expr <- parseExpr1
              lExps <- parseCommaExprs
              return $ [expr] ++ lExps)
              

parseCommaExprs :: Parser [Expr]
parseCommaExprs = return []
                  <|>
                  (do
                   symbol "," 
                   expr <- parseExpr1
                   lExps <- parseCommaExprs
                   return $ [expr] ++ lExps)

parseArrayCompr :: Parser (Maybe ArrayCompr)
parseArrayCompr = return Nothing
                  <|>
                  (do
                   symbol "if"
                   symbol "("
                   expr <- parseExpr1
                   symbol ")"
                   arrCompr <- parseArrayCompr
                   return $ Just (ArrayIf expr arrCompr) )
                  <|>
                  (do
                   symbol "for"
                   symbol "("
                   ident <- token $ parseIdent
                   symbol "of"
                   expr <- parseExpr1
                   symbol ")"
                   arrCompr <- parseArrayCompr
                   return $ Just (ArrayForCompr (ident, expr, arrCompr) ) )
                  
-- Bit complicated but I avoid looking up the '-' character with lookup function (Had to split the if - else cases because of do notation)
parseNumberExpr :: Parser Expr
parseNumberExpr = token $ do
                            y <- item
                            if y == '-' then getMinusInt
                            else getPositiveInt y
                                
                                
getMinusInt :: Parser Expr
getMinusInt = do
                x <- token $ munch1 digit
                -- x has length at least 1 due to munch1 definition
                if length x <= 8 then return $ Number (- read x)    
                else reject
                            
getPositiveInt :: Char -> Parser Expr
getPositiveInt y = do
                    if isDigit y then getPositiveInt2 y
                    else reject 

getPositiveInt2 :: Char -> Parser Expr
getPositiveInt2 y = do
                        x <- munch digit
                        -- [y]++x has length at least 1 (y has length 1)
                        if length ([y]++x) <= 8 then return $ Number $ read ([y]++x)    
                        else reject
                    
                
parseStringExpr :: Parser Expr
parseStringExpr = do 
                  symbol "'"
                  s <- munch $ satisfy isChar
                  symbol "'"
                  return $ String s

isChar :: Char -> Bool
isChar c = if c `elem` specChar then False 
           else True

-- The idea of doing these functions is taken from assignment 2 as well     
parseIdent :: Parser Ident
parseIdent = do
               c <- satisfy isIdentBegin
               c' <- munch $ satisfy isIdentChar
               if isReservedWord $ [c] ++ c' then reject
               else return $ [c] ++ c'


isIdentBegin :: Char -> Bool
isIdentBegin c = if isAlpha c || isUnderscore c then True
            else False
            
isUnderscore :: Char -> Bool
isUnderscore c = if c == '_' then True
                 else False
                 
isIdentChar :: Char -> Bool                
isIdentChar c = if isAlpha c || isDigit c || isUnderscore c then True
            else False    

isReservedWord :: String -> Bool
isReservedWord s = if s `elem` reservedWords then True
                   else False            
               
-- Functions taken from Expr.hs (Made by TA Maya Saietz in a Laboratory session)
-- Could be nicer to include them in an improved version of SimpleParse (Recall that I've used the original SimpleParse file provided in the lectures)

munch :: Parser a -> Parser [a]
munch p = munch1 p <++ return []

munch1 :: Parser a -> Parser [a]
munch1 p = do
    x  <- p
    xs <- munch p
    return $ x : xs
    
digit :: Parser Char
digit = satisfy isDigit    