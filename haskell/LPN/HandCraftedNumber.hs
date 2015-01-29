module LPN.HandCraftedNumber where

ones = ["one","two","three","four","five","six","seven","eight","nine"]
decades = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
bases = ["hundred","thousand","million"]
epsilon = ["null"]
lexicon = ones ++ decades ++ teens ++ bases ++ epsilon


-- list of all predicates

writeHandCrafted :: FilePath -> IO ()
writeHandCrafted fp = writeFile fp allPredicates

allPredicates :: String
allPredicates = (unlines . concat) [ decadesG
                                   , teensG
                                   , onesG
                                   , prevBaseG
                                   , largerBaseG
                                   , prefixG
                                   , normalPrefixG
                                   , suffixG
                                   , numberG
                                   , numberOfBaseG
                                   , equalG
                                   , sOnesG
                                   , sTeensG
                                   , sDecadesG
                                   , succG
                                   , maxOnesG
                                   , maxDecadesG
                                   , maxPrefixG
                                   , maxForBaseG
                                   , predG
                                   , mOnesG
                                   , mTeensG
                                   , mDecadesG
                                   , moreG
                                   , lessG ]

-- Lexicon Predicate

-- not currently in use
lexiconG :: [String]
lexiconG = ["Lexicon(" ++ x ++ "," ++ y ++ ")." | x <- lexicon, y <- lexicon ]

-- Number Predicates

onesG :: [String]
onesG = ["Ones(" ++ x ++ ")." | x <- ones]

teensG :: [String]
teensG = ["Teens(" ++ x ++ ")." | x <- teens]

decadesG :: [String]
decadesG = ["Decades(" ++ x ++ ")." | x <- decades]

prevBaseG :: [String]
prevBaseG = [ "PrevBase(million, thousand)."
            , "PrevBase(thousand, hundred)."
            , "PrevBase(hundred, null)." ]

largerBaseG :: [String]
largerBaseG = [ "LargerBase(X, Z) <-- LargerBase(X, Y), LargerBase(Y, Z)."
              , "LargerBase(X, Y) <-- PrevBase(X, Y)." ]
              
prefixG :: [String]
prefixG = [ "Prefix(P,B) <-- LargerBase(B,hundred), NormalPrefix(P)."
          , "Prefix(P,hundred) <-- Ones(P)."
          , "Prefix(P, null) <-- Ones(P)."
          , "Prefix(P, null) <-- Teens(P)."
          , "Prefix(P, null) <-- Decades(P)."
          , "Prefix(P Q, null) <-- Decades(P), Ones(Q)." ]

normalPrefixG :: [String]
normalPrefixG = [ "NormalPrefix(S) <-- Suffix(thousand,S)." ]

suffixG :: [String]
suffixG = [ "Suffix(B,S) <-- LargerBase(B,C), NumberOfBase(S,C)."
          , "Suffix(null, null)." ]

numberOfBaseG :: [String]
numberOfBaseG = [ "NumberOfBase(P C S, C) <-- Prefix(P,C), Suffix(C,S)."
                , "NumberOfBase(P C, C) <-- Prefix(P,C)." ]
                
equalG :: [String]
equalG = [ "Equal(X, X) <-- Number(X)." ]

numberG :: [String]
numberG = [ "Number(P B S) <-- Prefix(P,B), Suffix(B,S)."
          , "Number(P B) <-- Prefix(P,B)." ]

-- -- not currently in use
-- baseG :: [String]
-- baseG = [ "Base(million, null) <-- Lexicon(million, null)."
--         , "Base(thousand, null) <-- Lexicon(thousand, null)."
--         , "Base(hundred, null) <-- Lexicon(hundred, null)."
--         , "Base(null, null) <-- Lexicon(null, null)." ]
-- 
-- -- not currently in use
-- nullG :: [String]
-- nullG = [ "Null(null,null) <-- Lexicon(null,null)." ]
-- 
-- -- not currently in use
-- hundredG :: [String]
-- hundredG = [ "Hundred(hundred, null) <-- Lexicon(hundred, null)." ]

-- Succession Predicates

sOnesG :: [String]
sOnesG =  ["SOnes(" ++ (ones!!x) ++ ", " ++ ones!!(x+1) ++ ")." |
           x <- [0..((length ones) - 2)] ]

sTeensG :: [String]
sTeensG = ["STeens(" ++ teens!!x ++ ", " ++ teens!!(x+1) ++ ")." |
           x <- [0..((length teens) - 2)] ]

sDecadesG :: [String]
sDecadesG = ["SDecades(" ++ decades!!x ++ ", " ++ decades!!(x+1) ++ ")." |
             x <- [0..((length decades) - 2)] ]

succG :: [String]
succG = [ "Succ(X,Y) <-- SOnes(X,Y)."
        , "Succ(nine,ten)."
        , "Succ(X,Y) <-- STeens(X,Y)."
        , "Succ(nineteen,twenty)."
        , "Succ(X,X one) <-- Decades(X)."
        , "Succ(X Y,X V) <-- Decades(X), SOnes(Y,V)."
        , "Succ(X Y,U) <-- SDecades(X,U), MaxOnes(Y)."
        , "Succ(X,one C) <-- MaxForBase(X,B), PrevBase(C,B)."
        , "Succ(P B,P B one) <-- Prefix(P,B), LargerBase(B, null)."
        , "Succ(P B S,P B T) <-- Prefix(P,B), Succ(S,T), Suffix(B,T)."
        , "Succ(P B S,Q B) <-- PrevBase(B,C), MaxForBase(S,C), Succ(P,Q), Prefix(Q,B)." ]

maxOnesG :: [String]
maxOnesG = [ "MaxOnes(nine)." ]

maxDecadesG :: [String]
maxDecadesG = [ "MaxDecades(ninety)." ]

maxPrefixG :: [String]
maxPrefixG = [ "MaxPrefix(W,U) <-- LargerBase(U,hundred), MaxForBase(W,hundred)."
             , "MaxPrefix(O,hundred) <-- MaxOnes(O)."
             , "MaxPrefix(D O, null) <-- MaxDecades(D), MaxOnes(O)." ]

maxForBaseG :: [String]
maxForBaseG = [ "MaxForBase(P B S,B) <-- MaxPrefix(P,B), PrevBase(B,C), MaxForBase(S,C)."
              , "MaxForBase(P, null) <-- MaxPrefix(P, null)." ]

predG :: [String]
predG = [ "Pred(X,Y) <-- Succ(Y,X)." ]

-- more/less predicates

mOnesG :: [String]
mOnesG =  ["MOnes(" ++ (ones!!x) ++ ", " ++ (ones!!y) ++ ")." |
           x <- [0..((length ones) - 1)], y <- [0..((length ones) - 1)], x > y ]

mTeensG :: [String]
mTeensG =  ["MTeens(" ++ (teens!!x) ++ ", " ++ (teens!!y) ++ ")." |
            x <- [0..((length teens) - 1)], y <- [0..((length teens) - 1)], x > y ]

mDecadesG :: [String]
mDecadesG =  ["MDecades(" ++ (decades!!x) ++ ", " ++ (decades!!y) ++ ")." |
              x <- [0..((length decades) - 1)], y <- [0..((length decades) - 1)], x > y ]

moreG :: [String]
moreG = [ "More(X,Y) <-- MOnes(X,Y)."
        , "More(X,Y) <-- MTeens(X,Y)."
        , "More(X,Y) <-- MDecades(X,Y)."
        , "More(X,Y) <-- Teens(X),   Ones(Y)."
        , "More(X,Y) <-- Decades(X), Ones(Y)."
        , "More(X,Y) <-- Decades(X), Teens(Y)."
        , "More(X Y,U) <-- Decades(X), Ones(Y), Ones(U)."   
        , "More(X Y,U) <-- Decades(X), Ones(Y), Teens(U)."   
        , "More(X Y,X) <-- Decades(X), Ones(Y)."
        , "More(X Y,X U) <-- Decades(X,), MOnes(Y,U)."
        , "More(X Y,U) <-- MDecades(X,U), Ones(Y)."
        , "More(X,U Y) <-- MDecades(X,U), Ones(Y)."
        , "More(X V,U Y) <-- MDecades(X,U), Ones(V), Ones(Y)."
        , "More(X,Y) <-- LargerBase(B,C), NumberOfBase(X,B), NumberOfBase(Y,C)."
        , "More(P B S,Q B T) <-- LargerBase(B,null), Prefix(P,B), Prefix(Q,B), More(P,Q), Suffix(B,S), Suffix(B,T)."
        , "More(P B S,P B T) <-- LargerBase(B,null), Prefix(P,B), Suffix(B,S), Suffix(B,T), More(S,T)."
        , "More(P B S,P B) <-- Prefix(P,B), Suffix(B,S)." ]

lessG :: [String]
lessG = [ "Less(X,Y) <-- More(Y,X)." ]
