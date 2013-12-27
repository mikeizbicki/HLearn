{-# LANGUAGE DataKinds, PolyKinds, TemplateHaskell #-}

module HLearn.Models.Regression.Parsing
    where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token
import HLearn.Algebra

-------------------------------------------------------------------------------
-- syntax types

data Expr = Expr [Term]

data Term
    = CVar 
    | CCon Rational
    | CMon MonOp Term
    | CBin BinOp Term Term 
    deriving (Read,Show,Eq)

data MonOp = CLog | CNeg | CSin
    deriving (Read,Show,Eq)

data BinOp = Mult | Div | Pow | Add
    deriving (Read,Show,Eq)

term1 = CMon CLog $ CBin Mult CVar $ CMon CNeg CVar
term2 = CBin Pow CVar (CCon 2)
term3 = CBin Pow (CCon 2) CVar

parseExpr :: String -> Either ParseError Term
parseExpr str = parse exprParser "(unknown)" str
-- parseExpr str = parse matchTerm "(unknown)" str

lexer = makeTokenParser $ emptyDef -- { reservedNames = ["x"] }
    { reservedOpNames 
        = ["*","/","+","-"]
        ++["log"]
    }

matchParens = parens lexer
matchWhiteSpace = whiteSpace lexer
matchReserved = reserved lexer
matchReservedOp = reservedOp lexer
matchIdentifier = identifier lexer
matchNumber = naturalOrFloat lexer

matchTerm :: Parser Term
-- matchTerm = matchWhiteSpace >> (matchParens matchTerm <|> var <|> monOp )
matchTerm = matchWhiteSpace >> (matchParens exprParser <|> var <|> matchConst )

-- matchConst = matchNumber >>= (CCon . toRational)
matchConst = do
    x <- matchNumber
    case x of
        Left y -> return $ CCon $ toRational y
        Right y -> return $ CCon $ toRational y
--     return $ toRational x

exprParser :: Parser Term
exprParser = buildExpressionParser opList matchTerm

opList = 
    [ [Prefix (matchReservedOp "-" >> return (CMon CNeg             ))          ]
    , [Prefix (matchReservedOp "log" >> return (CMon CLog)) ]
    , [Infix  (matchReservedOp "^" >> return (CBin Pow)) AssocLeft]
    , [Infix  (matchReservedOp "*" >> return (CBin Mult)) AssocLeft]
    , [Infix  (matchReservedOp "/" >> return (CBin Div  )) AssocLeft]
    , [Infix  (matchReservedOp "-" >> return (\a b -> CBin Add a (CMon CNeg b) )) AssocLeft]
    , [Infix  (matchReservedOp "+" >> return (CBin Add  )) AssocLeft]
--     , [Infix  (reservedOp "+" >> return (CBin Add     )) AssocLeft]
--     , [Infix  (reservedOp "-" >> return (CBin Subtract)) AssocLeft]
    ]

monOp = do
--     cmd <- manyTill (alphaNum <|> char '-') (char ' ')
    cmd <- matchIdentifier
    let op = case cmd of 
            "log" -> CLog
            "sin" -> CSin
            "neg" -> CNeg 
    t <- matchTerm
    return $ CMon op t

-- monOp = string "log" >> do
--     t <- term
--     return $ CMon CLog t

var = do
--     x <- matchIdentifier 
--     if x=="x"
--         then return CVar
--         else fail 
    char 'x' >> matchWhiteSpace >> return CVar


---------------------------------------

data instance Sing (f::Term) = STerm Term
data instance Sing (f::MonOp) = SMonOp MonOp 
data instance Sing (f::BinOp) = SBinOp BinOp

instance SingI CVar where 
    sing = STerm CVar
instance (SingI m, SingI t) => SingI (CMon m t) where 
    sing = STerm (CMon (fromSing (sing::Sing m)) (fromSing (sing::Sing t)))
instance (SingI m, SingI t1, SingI t2) => SingI (CBin m t1 t2) where
    sing = STerm (CBin (fromSing (sing::Sing m)) (fromSing (sing::Sing t1)) (fromSing (sing::Sing t2)))

instance SingI CLog where sing = SMonOp CLog 
instance SingI CSin where sing = SMonOp CSin
instance SingI CNeg where sing = SMonOp CNeg
instance SingI Mult where sing = SBinOp Mult
instance SingI Div where sing = SBinOp Div

instance SingE (Kind :: Term)  Term  where fromSing (STerm  f) = f
instance SingE (Kind :: MonOp) MonOp where fromSing (SMonOp f) = f
instance SingE (Kind :: BinOp) BinOp where fromSing (SBinOp f) = f

---------------------------------------

ppShowTerm :: Term -> String
ppShowTerm CVar = "x"
ppShowTerm (CCon r) = show (fromRational r :: Double)
ppShowTerm (CMon op t) = ppShowMonOp op ++ "(" ++ ppShowTerm t ++ ")"
ppShowTerm (CBin op t1 t2) = "(" ++ ppShowTerm t1 ++ ")"++ ppShowBinOp op ++ "(" ++ ppShowTerm t2 ++ ")"

ppShowMonOp :: MonOp -> String
ppShowMonOp CLog = "log"
ppShowMonOp CNeg = "-"
ppShowMonOp CSin = "sin"

ppShowBinOp :: BinOp -> String
ppShowBinOp Mult = "*"
ppShowBinOp Div = "/"
ppShowBinOp Pow = "^"

---------------------------------------

evalTerm :: Floating x => Term -> x -> x
evalTerm CVar x = x
evalTerm (CCon c) _ = fromRational c
evalTerm (CMon f t) x = (evalMonOp f)  (evalTerm t x) 
evalTerm (CBin f t1 t2) x = (evalBinOp f) (evalTerm t1 x) (evalTerm t2 x)

evalMonOp :: Floating x => MonOp -> x -> x
evalMonOp CLog = log
evalMonOp CNeg = negate
evalMonOp CSin = sin

evalBinOp :: Floating x => BinOp -> x -> x -> x
evalBinOp Mult = (*)
evalBinOp Div = (/)
evalBinOp Pow = (**)

-- train [] :: LinearRegression [expr| 1 + x + x^2 + log x ] Double
