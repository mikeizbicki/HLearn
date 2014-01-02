{-# LANGUAGE DataKinds, PolyKinds, TemplateHaskell, QuasiQuotes, TypeOperators #-}

module HLearn.Models.Regression.Parsing
    where

import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

import qualified Language.Haskell.TH as TH
-- import Language.Haskell.TH hiding (Kind)
import Language.Haskell.TH.Quote

import HLearn.Algebra

-------------------------------------------------------------------------------
-- template haskell

expr :: QuasiQuoter 
expr = QuasiQuoter 
--     { quoteType = \str -> [t| '[] |]
    { quoteType = exprType
    }

exprType str = return $ go (parseExprSimple str)
    where 
        go [] = TH.PromotedNilT
        go (x:xs) = TH.AppT (TH.AppT TH.PromotedConsT $ termType x) $ go xs

termType :: Term -> TH.Type
termType CVar = TH.PromotedT $ TH.mkName "VarT"
termType (CCon x) = 
    TH.AppT 
        (TH.PromotedT $ TH.mkName "ConT") 
        (TH.AppT 
            (TH.AppT 
                (TH.PromotedT $ TH.mkName "Frac") 
                (TH.LitT $ TH.NumTyLit 2)) 
            (TH.LitT $ TH.NumTyLit 1))

termType (CMon op t) = 
    TH.AppT 
        (TH.AppT 
            (TH.PromotedT $ TH.mkName "MonT")
            (TH.PromotedT $ TH.mkName $ show op))
        (termType t)

termType (CBin op t1 t2) = 
    TH.AppT
        (TH.AppT
            (TH.AppT
                (TH.PromotedT $ TH.mkName "BinT")
                (TH.PromotedT $ TH.mkName $ show op))
            (termType t1))
        (termType t2)
 

data DivFrac = DivFrac Nat Nat


-------------------------------------------------------------------------------
-- syntax types

type Expr = [Term]

data TermT
    = VarT
    | ConT Frac
    | MonT MonOp TermT
    | BinT BinOp TermT TermT

data Term
    = CVar 
    | CCon Rational
    | CMon MonOp Term
    | CBin BinOp Term Term 
    deriving (Read,Show,Eq,Ord)

-- instance Show Term where
--     show CVar = "CVar"
--     show (CCon _) = "CCon"
--     show (CMon op t) = "CMon "++show op++" ("++show t++show ")"
--     show (CBin op t1 t2) = "CBin "++show op++" ("++show t1++show ") ("++show t2++show ")"

data MonOp = CLog | CNeg | CSin
    deriving (Read,Show,Eq,Ord)

data BinOp = Mult | Div | Pow | Add
    deriving (Read,Show,Eq,Ord)

---------------------------------------

isCommutative :: BinOp -> Bool
isCommutative Mult = True
isCommutative Div = False
isCommutative Pow = False
isCommutative Add = True

canonicalize :: Term -> Term
canonicalize (CBin op t1 t2) = if isCommutative op
    then CBin op (min t1' t2') (max t1' t2')
    else CBin op t1' t2'
    where
        t1' = canonicalize t1
        t2' = canonicalize t2
canonicalize (CMon op t) = CMon op $ canonicalize t
canonicalize t = t 


term2expr :: Term -> Expr
term2expr t = sort $ go t
    where
        go :: Term -> [Term]
        go (CBin Add t1 t2) = go t1++go t2
        go t = [t]

---------------------------------------

parseExprSimple :: String -> Expr
parseExprSimple str = case parseExpr str of
    Right expr -> expr
    Left err -> error $ "parseExprSimple: "++show err

parseExpr :: String -> Either ParseError Expr
parseExpr str = fmap (term2expr . canonicalize) $ parse exprParser "(unknown)" str

lexer = makeTokenParser $ emptyDef 
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
    ]

matchTerm :: Parser Term
matchTerm = matchWhiteSpace >> (matchParens exprParser <|> var <|> matchConst )

var = char 'x' >> matchWhiteSpace >> return CVar

matchConst = do
    x <- matchNumber
    case x of
        Left y -> return $ CCon $ toRational y
        Right y -> return $ CCon $ toRational y

---------------------------------------

data instance Sing (f::TermT) = STerm Term
-- data instance Sing (f::Term) = STerm Term
data instance Sing (f::MonOp) = SMonOp MonOp 
data instance Sing (f::BinOp) = SBinOp BinOp

-- instance SingI CVar where 
instance SingI VarT where 
    sing = STerm CVar
instance SingI c => SingI (ConT c) where 
    sing = STerm $ CCon (fromSing (sing::Sing c))
-- instance (SingI m, SingI t) => SingI (CMon m t) where 
instance (SingI m, SingI t) => SingI (MonT m t) where 
    sing = STerm (CMon (fromSing (sing::Sing m)) (fromSing (sing::Sing t)))
-- instance (SingI m, SingI t1, SingI t2) => SingI (CBin m t1 t2) where
instance (SingI m, SingI t1, SingI t2) => SingI (BinT m t1 t2) where
    sing = STerm (CBin (fromSing (sing::Sing m)) (fromSing (sing::Sing t1)) (fromSing (sing::Sing t2)))

instance SingI CLog where sing = SMonOp CLog 
instance SingI CSin where sing = SMonOp CSin
instance SingI CNeg where sing = SMonOp CNeg
instance SingI Mult where sing = SBinOp Mult
instance SingI Pow where sing = SBinOp Pow
instance SingI Div where sing = SBinOp Div

-- instance SingE (Kind :: Term)  Term  where fromSing (STerm  f) = f
instance SingE (Kind :: TermT)  Term  where fromSing (STerm  f) = f
instance SingE (Kind :: MonOp) MonOp where fromSing (SMonOp f) = f
instance SingE (Kind :: BinOp) BinOp where fromSing (SBinOp f) = f

-------------------

data instance Sing (xs :: [TermT]) = STermL { unSTermL :: [ Term ] }

instance SingI ('[] :: [TermT]) where 
    sing = STermL []

instance 
    ( SingI t
    , SingI ts
    ) => SingI (t ': (ts :: [TermT])) 
        where 
    sing = STermL $ (fromSing (sing :: Sing t)) : (unSTermL ( sing :: Sing ts))

instance SingE (Kind :: [TermT]) [Term] where
-- instance SingE (Kind :: [a]) [Term] where
    fromSing (STermL xs) = xs

-- instance SingE (Kind :: [Nat]) [Int] where

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

evalStr :: Floating x => String -> x -> x
evalStr str x = case parseExpr str of
    Right term -> evalExpr term x
    Left str -> error $ "evalStr: " ++ show str

evalExpr :: Floating x => Expr -> x -> x
evalExpr (ts) x = sum $ map (flip evalTerm x) ts

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
evalBinOp Add = (+)

-- train [] :: LinearRegression [expr| 1 + x + x^2 + log x ] Double
