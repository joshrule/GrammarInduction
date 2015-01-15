module LPN.HandBuiltNumber where

ones = ["one","two","three","four","five","six","seven","eight","nine"]
decades = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
bases = ["hundred","thousand","million"]
epsilon = ["null"]
lexicon = ones ++ decades ++ teens ++ bases ++ epsilon


-- list of all predicates

writeHandBuilt :: FilePath -> IO ()
writeHandBuilt fp = writeFile fp allPredicates

allPredicates :: String
allPredicates = (unlines . concat) [ equalG
                                   , numberG
                                   , baseG
                                   , prevBaseG
                                   , prefixG
                                   , suffixG
                                   , nullG
                                   , hundredG
                                   , normalBaseG
                                   , onesG
                                   , teensG
                                   , decadesG
                                   , sOnesG
                                   , sTeensG
                                   , sDecadesG
                                   , succG
                                   , startG
                                   , maxOnesG
                                   , maxDecadesG
                                   , maxPrefixG
                                   , maxForBaseG
                                   , prevG
                                   , mOnesG
                                   , mTeensG
                                   , mDecadesG
                                   , moreG
                                   , mBaseG
                                   , lessG
                                   , lexiconG ]

-- Lexicon Predicate

lexiconG :: [String]
lexiconG = ["Lexicon(" ++ x ++ "," ++ y ++ ")." | x <- lexicon, y <- lexicon ]

-- Number Predicates

equalG :: [String]
equalG = [ "Equal(X, X) <-- Number(X, B)." ]

numberG :: [String]
numberG = [ "Number(X,Y) <-- Ones(X,Y)."
          , "Number(X,Y) <-- Teens(X,Y)."
          , "Number(X,Y) <-- Decades(X,Y)."
          , "Number(X Y,U V) <-- Decades(X,U), Ones(Y,V)."
          , "Number(P B S,B) <-- Prefix(P,B), Suffix(B,S)."
          , "Number(S,B) <-- Suffix(B,S)."
          , "Number(P B,B) <-- Prefix(P,B)." ]

baseG :: [String]
baseG = [ "Base(million, null) <-- Lexicon(million, null)."
        , "Base(thousand, null) <-- Lexicon(thousand, null)."
        , "Base(hundred, null) <-- Lexicon(hundred, null)."
        , "Base(null, null) <-- Lexicon(null, null)." ]

prevBaseG :: [String]
prevBaseG = [ "PrevBase(million, thousand) <-- Lexicon(million, thousand)."
            , "PrevBase(thousand, hundred) <-- Lexicon(thousand, hundred)."
            , "PrevBase(hundred, null)     <-- Lexicon(hundred, null)." ]

prefixG :: [String]
prefixG = [ "Prefix(P,B) <-- NormalBase(B,Y), Number(P,Z), Hundred(Z,Q)."
          , "Prefix(P,B) <-- Ones(P,Y), Hundred(B,Z)."
          , "Prefix(null, null) <-- Lexicon(null, null)." ]

suffixG :: [String]
suffixG = [ "Suffix(B,S) <-- PrevBase(B,C), Number(S,C)." ]


nullG :: [String]
nullG = [ "Null(null,null) <-- Lexicon(null,null)." ]

hundredG :: [String]
hundredG = [ "Hundred(hundred, null) <-- Lexicon(hundred, null)." ]

normalBaseG :: [String]
normalBaseG = [ "NormalBase(B,X) <-- MBase(B,H), Hundred(H,X)." ]

onesG :: [String]
onesG = ["Ones(" ++ x ++ ", null) <-- Lexicon(" ++ x ++ ", null)." | x <- ones]

teensG :: [String]
teensG = ["Teens(" ++ x ++ ", null) <-- Lexicon(" ++ x ++ ", null)." | x <- teens]

decadesG :: [String]
decadesG = ["Decades(" ++ x ++ ", null) <-- Lexicon(" ++ x ++ ", null)." | x <- decades]

-- Succession Predicates

sOnesG :: [String]
sOnesG =  ["SOnes(" ++ (ones!!x) ++ ", " ++ ones!!(x+1) ++ ") <-- Lexicon("++
             ones!!(x) ++ ", " ++ ones!!(x+1) ++ ")." |
             x <- [0..((length ones) - 2)] ]

sTeensG :: [String]
sTeensG = ["STeens(" ++ teens!!x ++ ", " ++ teens!!(x+1) ++ ") <-- Lexicon("++
             teens!!(x) ++ ", " ++ teens!!(x+1) ++ ")." |
             x <- [0..((length teens) - 2)] ]

sDecadesG :: [String]
sDecadesG = ["SDecades(" ++ decades!!x ++ ", " ++ decades!!(x+1) ++ ") <-- Lexicon("++
             decades!!(x) ++ ", " ++ decades!!(x+1) ++ ")." |
             x <- [0..((length decades) - 2)] ]

succG :: [String]
succG = [ "Succ(X,Y) <-- SOnes(X,Y)."
        , "Succ(nine,ten) <-- Lexicon(nine,ten)."
        , "Succ(X,Y) <-- STeens(X,Y)."
        , "Succ(nineteen,twenty) <-- Lexicon(nineteen,twenty)."
        , "Succ(X Y,X V) <-- Decades(X,U), Start(Y,V)."
        , "Succ(X Y,X V) <-- Decades(X,U), SOnes(Y,V)."
        , "Succ(X Y,U V) <-- SDecades(X,U), MaxOnes(Y,V)."
        , "Succ(X,V C) <-- MaxForBase(X,B), Start(U,V), PrevBase(C,B)."
        , "Succ(P B S,P B T) <-- Prefix(P,B), Start(S,T)."
        , "Succ(P B S,P B T) <-- Prefix(P,B), Succ(S,T), Suffix(B,T)."
        , "Succ(P B S,Q B) <-- PrevBase(B,C), MaxForBase(S,C), Succ(P,Q), Prefix(Q,B)." ]

startG :: [String]
startG = [ "Start(null,one) <-- Lexicon(null,one)." ]

maxOnesG :: [String]
maxOnesG = [ "MaxOnes(nine,null) <-- Lexicon(nine,null)." ]

maxDecadesG :: [String]
maxDecadesG = [ "MaxDecades(ninety,null) <-- Lexicon(ninety,null)." ]

maxPrefixG :: [String]
maxPrefixG = [ "MaxPrefix(W X Z,U) <-- NormalBase(U,V), MaxOnes(W,N), Hundred(X,T), PrevBase(X,Y), MaxForBase(Z,Y)."
             , "MaxPrefix(U,X) <-- MaxOnes(U,V), Hundred(X,Y)."
             , "MaxPrefix(null, null) <-- Lexicon(null, null)." ]

maxForBaseG :: [String]
maxForBaseG = [ "MaxForBase(P B S,B) <-- MaxPrefix(P,B), PrevBase(B,C), MaxForBase(S,C)."
              , "MaxForBase(D O,X Y) <-- MaxDecades(D,X), MaxOnes(O,Y)." ]

prevG :: [String]
prevG = [ "Prev(X,Y) <-- Succ(Y,X)." ]

-- more/less predicates

mOnesG :: [String]
mOnesG =  ["MOnes(" ++ (ones!!x) ++ ", " ++ (ones!!y) ++ ") <-- Lexicon("++
             (ones!!x) ++ ", " ++ (ones!!y) ++ ")." |
             x <- [0..((length ones) - 1)], y <- [0..((length ones) - 1)], x > y ]

mTeensG :: [String]
mTeensG =  ["MTeens(" ++ (teens!!x) ++ ", " ++ (teens!!y) ++ ") <-- Lexicon("++
             (teens!!x) ++ ", " ++ (teens!!y) ++ ")." |
             x <- [0..((length teens) - 1)], y <- [0..((length teens) - 1)], x > y ]

mDecadesG :: [String]
mDecadesG =  ["MDecades(" ++ (decades!!x) ++ ", " ++ (decades!!y) ++ ") <-- Lexicon("++
             (decades!!x) ++ ", " ++ (decades!!y) ++ ")." |
             x <- [0..((length decades) - 1)], y <- [0..((length decades) - 1)], x > y ]

moreG :: [String]
moreG = [ "More(X,Y) <-- MOnes(X,Y)."
        , "More(X,Y) <-- MTeens(X,Y)."
        , "More(X,Y) <-- MDecades(X,Y)."
        , "More(X Y,U V) <-- Teens(X,Y), Ones(U,V)."
        , "More(X Y,U V) <-- Decades(X,Y), Ones(U,V)."
        , "More(X Y,U V) <-- Decades(X,Y), Teens(U,V)."
        , "More(X Y,U V) <-- Decades(X,Z), Ones(Y,T), Ones(U,V)."   
        , "More(X Y,U V) <-- Decades(X,Z), Ones(Y,T), Teens(U,V)."   
        , "More(X Y,X V) <-- Decades(X,U), Ones(Y,V)."
        , "More(X Y,X V) <-- Decades(X,U), MOnes(Y,V)."
        , "More(X Y,U V) <-- MDecades(X,U), Ones(Y,V)."
        , "More(X Y,U V) <-- MDecades(X,U), Ones(V,Y)."
        , "More(X V,U W) <-- MDecades(X,U), Ones(V,Y), Ones(W,Z)."
        , "More(P B S,T) <-- Prefix(P,B), Suffix(B,S), Null(D,D), Number(T,D)."
        , "More(P B S,Q C T) <-- MBase(B,C), Prefix(P,B), Suffix(B,S), Prefix(Q,C), Suffix(C,T)."
        , "More(P B S,Q B T) <-- Prefix(P,B), Prefix(Q,B), More(P,Q), Suffix(B,S), Suffix(B,T)."
        , "More(P B S,P B T) <-- Prefix(P,B), Suffix(B,S), Suffix(B,T), More(S,T)."
        , "More(P B S,P B) <-- Prefix(P,B), Suffix(B,S)." ]

mBaseG :: [String]
mBaseG = [ "MBase(million, thousand) <-- Lexicon(million, thousand)."
         , "MBase(million, hundred) <-- Lexicon(million, hundred)."
         , "MBase(million, null) <-- Lexicon(million, null)."
         , "MBase(thousand,hundred) <-- Lexicon(thousand,hundred)."
         , "MBase(thousand,null) <-- Lexicon(thousand, null)."
         , "Mbase(hundred, null) <-- Lexicon(hundred, null)." ]

lessG :: [String]
lessG = [ "Less(X,Y) <-- More(Y,X)." ]
