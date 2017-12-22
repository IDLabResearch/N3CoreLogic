

-- UUAGC 0.9.42.3 (attGr.ag)

{-# LINE 4 "./attGr.ag" #-}



import Data.List
import Text.Parsec.Prim
import N3grammar
import System.Directory
--import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJ

{-# LINE 17 "attGr.hs" #-}
{-# LINE 336 "./attGr.ag" #-}


--TODO: pretty print
--TODO: clean up the ugly function!
{-
main ::  IO ()
main = case parse mainparser "" ":a :b :c." of
              { Left err -> print err
              ; Right ans -> print (  show (  transformed_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )  )
              }
-}



             
parseToCoreTree :: String -> IO ()
parseToCoreTree filename = do {
                       input <- readFile filename
                       ; case runParser mainparser 0 filename input of
                       {
                       Left err -> print err
                       ; Right ans -> putStrLn $   show (  transformed_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  ) 
                       }
                       }  

              
eyeToCore :: String -> IO ()
eyeToCore fname = do {
               input <- readFile fname
              ; case runParser mainparser 0 fname input of
              { Left err -> print err
              ; Right ans -> putStrLn $ show (  eye_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )  
              }}

parseToCoreFormula :: String -> IO ()
parseToCoreFormula fname = do {
                input <- readFile fname
                ; case runParser mainparser 0 fname input of
                { Left err -> print err
                ; Right ans -> print (  formula_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  ) )
                }
              }


testp :: String -> IO ()
testp fname = do {
                  input <- readFile fname
                  ; case runParser mainparser 0 fname input of
                  { Left err -> print err
                  ; Right ans -> putStrLn $   show (  eyeVar_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  ) 
                  } 
              }


-- function for quantifiers: takes Universals and existentials as lists and the formula without quantifier and produces a quantified formula
qua :: [String] -> [String] -> CFormula -> CFormula
--qua u e f = foldr fa f u
--fa :: String ->  CFormula
--fa  u f = Quant (Forall (Var u)) f

qua [] [] f  = f
qua [] (a:as) f = Quant (Forsome (Var a)) (qua [] as f)
qua (a:as) g f = Quant (Forall (Var a)) (qua as g f)





--the qua function to generate a formula (just to see the translation as text)
quaf :: [String] -> [String] -> String -> String
quaf [] [] s = s
quaf [] (a:as) s = "ForSome " ++ a ++ ". " ++ (quaf [] as s)
quaf (a:as) g s = "ForAll " ++ a ++ ". " ++ (quaf as g s)

findDepth :: [String] -> Int -> Int -> Int
findDepth [] n m = m
findDepth set n m = max n m

deepest :: [String] -> [String]  ->[String]
deepest new [] = new
deepest new old = old

choose :: [String] -> [String] -> Int -> Int ->[String]
choose first second n m = if (n>m) then first
                          else second



imp :: Bool -> [String] -> [String]
imp False l  = l
imp f l = []

--not really pretty, but at least easy
printTree :: String -> Int -> String 
printTree ('(':b) n = "\n"++ ( concat (replicate (n+1) "|  ") ) ++ ( printTree b (n+1) )
printTree (')':b) n = ( printTree b (n-1) )
printTree (s:b)   n  =  (s    : ( printTree b n))
printTree s       n  =  s 


makeTree :: String -> String -> IO()
makeTree fname "eye" = do {
                input <- readFile fname
                ; case runParser mainparser 0 fname input of
                { Left err -> print err
                ; Right ans -> putStrLn $ printTree  (show (  eye_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )) 0
                }}
makeTree fname "cwm" = do {
                input <- readFile fname
                ; case runParser mainparser 0 fname input of
                { Left err -> print err
                ; Right ans -> putStrLn $ printTree  (show  (  transformed_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )) 0
                }
                }
makeTree fname "uni" = do {
                input <- readFile fname
                ; case runParser mainparser 0 fname input of
                { Left err -> print err
                ; Right ans -> putStrLn $ printTree  (show  (  diff_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )) 0
                }
                }


--really primitive function to compare tree strings, later that comparison should be on tree level
compareTree :: String -> String -> String -> Int -> String
compareTree (x:b) (y:c) s n 
           | (x==y) = compareTree b c (s++[x]) (n+1)
           | otherwise = "Difference at pos. "++ show(n)  ++ "\n" ++ s ++
                               "\n CWM:  " ++ (take 100 (x:b)) ++ "... \n and \n Other:  " ++ (take 100 (y:c))++ "..."
compareTree [][] s n = "no differences found"

compareB :: String -> IO()
compareB fname = do {
               input <- readFile fname
               ; case runParser mainparser 0 fname input of
                        { 
                          Left err -> print err
                          ; Right ans -> putStrLn $  compareTree (show (  transformed_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )) 
                                                              (show (  eye_Syn_S         (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  ))
                                                              []
                                                              0
                        }
               }


topUni :: String -> IO()
topUni fname = do {
               input <- readFile fname
               ; case runParser mainparser 0 fname input of
                        { 
                          Left err -> print err
                          ; Right ans -> putStrLn $ show (  n1_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  ) 
  
                        }
               }






compareU :: String -> IO()
compareU fname = do {
               input <- readFile fname
               ; case runParser mainparser 0 fname input of
                        { 
                          Left err -> print err
                          ; Right ans -> putStrLn $  compareTree (show (  transformed_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )) 
                                                              (show (  diff_Syn_S         (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  ))
                                                              []
                                                              0
                        }
               }
depth :: String -> IO()
depth fname =  do {
               input <- readFile fname
               ; case runParser mainparser 0 fname input of
                        { 
                          Left err -> print err
                          ; Right ans -> putStrLn $ show (  count_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )
                        }
               }

deepvars :: String -> IO()
deepvars fname =  do {
               input <- readFile fname
               ; case runParser mainparser 0 fname input of
                        { 
                          Left err -> print err
                          ; Right ans -> putStrLn $ show (  deep_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )
                        }
               }


compareAll :: String -> String ->  IO ()
compareAll foname target = do {
               
                files <- getDirectoryContents foname
                ; let fs = (drop 2 files)-- I guess I should solve that differently?
               -- ; let fname = head fs
               -- ;  input <- readFile (foname ++"/"++fname)
               -- ; case runParser mainparser 0 fname input of
               -- { Left err ->   writeFile "err.txt" (show err)
               -- ; Right ans ->  writeFile "out.txt" $ niceTable (read (show ans)) fname
                                
               -- }
               --this here is to only consider the files containing the symbol ?, only those can contain universals
               ;  newfs <- findFilesWithUniversals foname fs []
               ;   (values, count) <- (readAll foname newfs [] [0, 0, 0, 0])
               ; let table = makeTable newfs values count
               ; writeFile target $ ((replicate 40 ' ') ++ "cwm/es2  " ++ "cwm/eye  " ++ "eye/es2  " ++ "depth \n")++ table
               }


--readAll :: String -> [String] ->  -> IO() 
readAll foname [] list [a, b, c, d] = do { 
                            return $ ((reverse list), [a,b,c,d])
                            }
readAll foname fs list [a, b, c, d] = do { 
                         let fname = (head fs)
                         ; input <- readFile $ foname ++ "/"++ fname
                         ; print (foname ++ "/"++ fname ++ " parsed \n" )
                             
                                    ;case runParser mainparser 0 fname input of
                                    {
                                    Left  err ->   (readAll foname (tail fs) ( [2, 2, 2, 2, 2 ]:list ) [a, b, c, d] )
                                    ;Right ans -> (\res -> ((\x y z zz zzz -> (readAll foname (tail fs) (
                                                             [x, 
                                                              y, 
                                                              z,
                                                              zz,
                                                              zzz  
                                                             ]:list ) 
                                                             [(a+x), (b+y), (c+z), (d+1) ]
                                                             ) )
                                                             (cwm_and_eye res )
                                                             (cwm_and_diff res)
                                                             (eye_and_diff res)
                                                             (boolToInt ( (eyeVar_Syn_S (  wrap_S  (sem_S  res) Inh_S   ))==[]))
                                                             (  count_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )
                                                             ))
                                                             (read (show ans))
                                   }
                               
                            
                            }
                         
findFilesWithUniversals foname [] newfs = do { return (reverse newfs) }                        
findFilesWithUniversals foname fs newfs = do {
                                             let fname = (head fs)
                                             ; input <- readFile $ foname ++ "/"++ fname
                                             ; case (isInfixOf "?" input) of
                                                   {
                                                   True  -> ( findFilesWithUniversals foname (tail fs) (fname:newfs))
                                                   ;False -> ( findFilesWithUniversals foname (tail fs) newfs)
                                                   }
                                             }


                       
                                  
--todo: every parsing only once               
cwm_and_eye :: S -> Int
cwm_and_eye f = boolToInt (   ( eye_Syn_S  x ) ==  (   transformed_Syn_S x  )   ) 
           where x = (  wrap_S  (sem_S  f) Inh_S   )     
               
cwm_and_diff :: S -> Int
cwm_and_diff f = boolToInt ((  diff_Syn_S  x ) ==  (   transformed_Syn_S x  )   ) 
           where x = (  wrap_S  (sem_S  f) Inh_S   ) 

eye_and_diff :: S -> Int
eye_and_diff f = boolToInt ((   diff_Syn_S  x ) ==  (  eye_Syn_S x   ) )
           where x = (  wrap_S  (sem_S  f) Inh_S   ) 
           
           
niceTable :: S -> String -> String
niceTable f fname = (replicate 30 ' ') ++ "cwm/eye  " ++ "cwm/uni  " ++ "uni/eye \n" 
                     ++ fname ++ (replicate (30 - length(fname)) ' ')  ++ (show  ( cwm_and_eye f ))++ (replicate 8 ' ') ++ (show  ( cwm_and_diff f )) ++ (replicate 8 ' ') ++ (show  ( eye_and_diff f ))       


makeTable :: [String] -> [[Int]] -> [Int] -> String
makeTable [] values stat = lastLine stat
makeTable names ([a , b, c , 1, e]:v) [s1, s2, s3, s4] = (makeTable (tail names) v [(s1-a), (s2-b), (s3-c), (s4-1)])
makeTable names ([a , b, c , d, e]:v) stat = (head names) ++ (replicate (40 - length(head names)) ' ') ++ (concat $ fmap makeEntry [a, b, c, e]) ++ "\n"
                         ++ (makeTable (tail names) v stat)


lastLine :: [Int]-> String
lastLine [a,b,c, d] = "Absolute Number (from " 
                      ++ (show d) ++ "): " 
                      ++ (replicate(15 - (length(show d))) ' ')
                      ++ (show a) ++ (replicate (9 - (length(show a))) ' ')
                      ++ (show b) ++ (replicate (9 - (length(show b))) ' ')
                      ++ (show c) 
                      ++ "\n"
                      ++ "Percentage: "
                      ++ (show ((fromIntegral a)/(fromIntegral d)))
                      ++" "
                      ++ (show $ (fromIntegral b)/(fromIntegral d))
                      ++ " "
                      ++ (show $ (fromIntegral c)/(fromIntegral d))
 
makeEntry :: Int -> String 
makeEntry n = (show n) ++ (replicate 8 ' ')


--data TreeElem = CFormula | CTerm

-- function to compare trees
--compareTrees :: TreeElem -> TreeElem -> [(TreeElem, TreeElem)]
--compareTrees (CFormula rest)(CFormula rest2) = compareTrees rest rest2
--compareTrees (CTriple t1 t2 t3)(CTriple t11 t22 t33) = (compareTrees t1 t11)++(compareTrees t2 t22)++(compareTrees t3 t33)
--compareTrees a b = [(a,b)]

--compareTrees (Var x) (Var y) 
--            | (x == y) = []
--            | otherwise = [(x,y)]


boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

keepEx :: Bool -> [String] -> [String]
keepEx True l = l
keepEx False l = []

impExc :: CTerm -> Bool -> Bool
impExc (Con "uri_http://www.w3.org/2000/10/swap/log#implies") b = True
impExc t b = b 

exception :: Bool -> Bool -> [String] -> [String]
exception True False s = []
exception t    f s = s


boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"


{-# LINE 360 "attGr.hs" #-}
-- CExpression -------------------------------------------------
data CExpression = CBE (Bool)
                 | CFE (CFormula)
                 deriving ( Eq,Show)
-- cata
sem_CExpression :: CExpression ->
                   T_CExpression
sem_CExpression (CBE _b) =
    (sem_CExpression_CBE _b)
sem_CExpression (CFE _f) =
    (sem_CExpression_CFE (sem_CFormula _f))
-- semantic domain
type T_CExpression = ( )
data Inh_CExpression = Inh_CExpression {}
data Syn_CExpression = Syn_CExpression {}
wrap_CExpression :: T_CExpression ->
                    Inh_CExpression ->
                    Syn_CExpression
wrap_CExpression sem (Inh_CExpression) =
    (let ( ) = sem
     in  (Syn_CExpression))
sem_CExpression_CBE :: Bool ->
                       T_CExpression
sem_CExpression_CBE b_ =
    (let
     in  ( ))
sem_CExpression_CFE :: T_CFormula ->
                       T_CExpression
sem_CExpression_CFE f_ =
    (let
     in  ( ))
-- CFormula ----------------------------------------------------
data CFormula = CTriple (CTerm) (CTerm) (CTerm)
              | CImplication (CExpression) (CExpression)
              | CConjunction (CFormula) (CFormula)
              | Quant (Quant) (CFormula)
              deriving ( Eq,Show)
-- cata
sem_CFormula :: CFormula ->
                T_CFormula
sem_CFormula (CTriple _t1 _t2 _t3) =
    (sem_CFormula_CTriple (sem_CTerm _t1) (sem_CTerm _t2) (sem_CTerm _t3))
sem_CFormula (CImplication _e1 _e2) =
    (sem_CFormula_CImplication (sem_CExpression _e1) (sem_CExpression _e2))
sem_CFormula (CConjunction _f1 _f2) =
    (sem_CFormula_CConjunction (sem_CFormula _f1) (sem_CFormula _f2))
sem_CFormula (Quant _a _f) =
    (sem_CFormula_Quant (sem_Quant _a) (sem_CFormula _f))
-- semantic domain
type T_CFormula = ( )
data Inh_CFormula = Inh_CFormula {}
data Syn_CFormula = Syn_CFormula {}
wrap_CFormula :: T_CFormula ->
                 Inh_CFormula ->
                 Syn_CFormula
wrap_CFormula sem (Inh_CFormula) =
    (let ( ) = sem
     in  (Syn_CFormula))
sem_CFormula_CTriple :: T_CTerm ->
                        T_CTerm ->
                        T_CTerm ->
                        T_CFormula
sem_CFormula_CTriple t1_ t2_ t3_ =
    (let
     in  ( ))
sem_CFormula_CImplication :: T_CExpression ->
                             T_CExpression ->
                             T_CFormula
sem_CFormula_CImplication e1_ e2_ =
    (let
     in  ( ))
sem_CFormula_CConjunction :: T_CFormula ->
                             T_CFormula ->
                             T_CFormula
sem_CFormula_CConjunction f1_ f2_ =
    (let
     in  ( ))
sem_CFormula_Quant :: T_Quant ->
                      T_CFormula ->
                      T_CFormula
sem_CFormula_Quant a_ f_ =
    (let
     in  ( ))
-- CTerm -------------------------------------------------------
data CTerm = Var (String)
           | Con (String)
           | CExp (CExpression)
           | CList (CTerm) (CTerm)
           deriving ( Eq,Show)
-- cata
sem_CTerm :: CTerm ->
             T_CTerm
sem_CTerm (Var _t) =
    (sem_CTerm_Var _t)
sem_CTerm (Con _c) =
    (sem_CTerm_Con _c)
sem_CTerm (CExp _e) =
    (sem_CTerm_CExp (sem_CExpression _e))
sem_CTerm (CList _t _list) =
    (sem_CTerm_CList (sem_CTerm _t) (sem_CTerm _list))
-- semantic domain
type T_CTerm = ( )
data Inh_CTerm = Inh_CTerm {}
data Syn_CTerm = Syn_CTerm {}
wrap_CTerm :: T_CTerm ->
              Inh_CTerm ->
              Syn_CTerm
wrap_CTerm sem (Inh_CTerm) =
    (let ( ) = sem
     in  (Syn_CTerm))
sem_CTerm_Var :: String ->
                 T_CTerm
sem_CTerm_Var t_ =
    (let
     in  ( ))
sem_CTerm_Con :: String ->
                 T_CTerm
sem_CTerm_Con c_ =
    (let
     in  ( ))
sem_CTerm_CExp :: T_CExpression ->
                  T_CTerm
sem_CTerm_CExp e_ =
    (let
     in  ( ))
sem_CTerm_CList :: T_CTerm ->
                   T_CTerm ->
                   T_CTerm
sem_CTerm_CList t_ list_ =
    (let
     in  ( ))
-- Expression --------------------------------------------------
data Expression = BE (Bool)
                | FE (Formula)
                deriving ( Eq,Read,Show)
-- cata
sem_Expression :: Expression ->
                  T_Expression
sem_Expression (BE _b) =
    (sem_Expression_BE _b)
sem_Expression (FE _f) =
    (sem_Expression_FE (sem_Formula _f))
-- semantic domain
type T_Expression = Int ->
                    Bool ->
                    ([String]) ->
                    ( Int,([String]),CExpression,CExpression,([String]),([String]),String,([String]),CExpression)
data Inh_Expression = Inh_Expression {c_Inh_Expression :: Int,insideQuotation_Inh_Expression :: Bool,scope_Inh_Expression :: ([String])}
data Syn_Expression = Syn_Expression {count_Syn_Expression :: Int,deep_Syn_Expression :: ([String]),diff_Syn_Expression :: CExpression,eye_Syn_Expression :: CExpression,eyeEx_Syn_Expression :: ([String]),eyeVar_Syn_Expression :: ([String]),formula_Syn_Expression :: String,n1_Syn_Expression :: ([String]),transformed_Syn_Expression :: CExpression}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression sem (Inh_Expression _lhsIc _lhsIinsideQuotation _lhsIscope) =
    (let ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOtransformed) = sem _lhsIc _lhsIinsideQuotation _lhsIscope
     in  (Syn_Expression _lhsOcount _lhsOdeep _lhsOdiff _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOtransformed))
sem_Expression_BE :: Bool ->
                     T_Expression
sem_Expression_BE b_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope ->
         (let _lhsOn1 :: ([String])
              _lhsOtransformed :: CExpression
              _lhsOformula :: String
              _lhsOeyeVar :: ([String])
              _lhsOeye :: CExpression
              _lhsOeyeEx :: ([String])
              _lhsOdiff :: CExpression
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _lhsOn1 =
                  ({-# LINE 311 "./attGr.ag" #-}
                   []
                   {-# LINE 534 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 312 "./attGr.ag" #-}
                   CBE b_
                   {-# LINE 539 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 313 "./attGr.ag" #-}
                   if b_ == True then "<>" else "false"
                   {-# LINE 544 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 314 "./attGr.ag" #-}
                   []
                   {-# LINE 549 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 315 "./attGr.ag" #-}
                   CBE b_
                   {-# LINE 554 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 316 "./attGr.ag" #-}
                   []
                   {-# LINE 559 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 317 "./attGr.ag" #-}
                   CBE b_
                   {-# LINE 564 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 318 "./attGr.ag" #-}
                   0
                   {-# LINE 569 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 319 "./attGr.ag" #-}
                   []
                   {-# LINE 574 "attGr.hs" #-}
                   )
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOtransformed)))
sem_Expression_FE :: T_Formula ->
                     T_Expression
sem_Expression_FE f_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope ->
         (let _lhsOn1 :: ([String])
              _fOscope :: ([String])
              _lhsOtransformed :: CExpression
              _lhsOformula :: String
              _fOuni :: ([String])
              _lhsOeye :: CExpression
              _lhsOeyeEx :: ([String])
              _lhsOdiff :: CExpression
              _fOinsideQuotation :: Bool
              _fOc :: Int
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _lhsOeyeVar :: ([String])
              _fIcount :: Int
              _fIdeep :: ([String])
              _fIdiff :: CFormula
              _fIex :: ([String])
              _fIeye :: CFormula
              _fIeyeEx :: ([String])
              _fIeyeVar :: ([String])
              _fIformula :: String
              _fIn1 :: ([String])
              _fIn2 :: ([String])
              _fItransformed :: CFormula
              _lhsOn1 =
                  ({-# LINE 321 "./attGr.ag" #-}
                   _fIn2
                   {-# LINE 610 "attGr.hs" #-}
                   )
              _fOscope =
                  ({-# LINE 322 "./attGr.ag" #-}
                   _lhsIscope `union` _fIn1
                   {-# LINE 615 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 323 "./attGr.ag" #-}
                   CFE (qua (_fIn1 \\ _lhsIscope) _fIex _fItransformed)
                   {-# LINE 620 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 324 "./attGr.ag" #-}
                   "<"++ (quaf (_fIn1 \\ _lhsIscope) _fIex _fIformula) ++">"
                   {-# LINE 625 "attGr.hs" #-}
                   )
              _fOuni =
                  ({-# LINE 325 "./attGr.ag" #-}
                   _fIn1 \\ _lhsIscope
                   {-# LINE 630 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 326 "./attGr.ag" #-}
                   CFE (qua [] (imp _lhsIinsideQuotation _fIeyeEx) _fIeye )
                   {-# LINE 635 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 327 "./attGr.ag" #-}
                   _fIeyeEx
                   {-# LINE 640 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 328 "./attGr.ag" #-}
                   CFE (qua [] _fIex _fIdiff)
                   {-# LINE 645 "attGr.hs" #-}
                   )
              _fOinsideQuotation =
                  ({-# LINE 329 "./attGr.ag" #-}
                   True
                   {-# LINE 650 "attGr.hs" #-}
                   )
              _fOc =
                  ({-# LINE 330 "./attGr.ag" #-}
                   _lhsIc+1
                   {-# LINE 655 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 331 "./attGr.ag" #-}
                   findDepth (_fIn1 \\ _lhsIscope) (_lhsIc +1) _fIcount
                   {-# LINE 660 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 332 "./attGr.ag" #-}
                   deepest (_fIn1 \\ _lhsIscope) _fIdeep
                   {-# LINE 665 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 145 "./attGr.ag" #-}
                   _fIeyeVar
                   {-# LINE 670 "attGr.hs" #-}
                   )
              ( _fIcount,_fIdeep,_fIdiff,_fIex,_fIeye,_fIeyeEx,_fIeyeVar,_fIformula,_fIn1,_fIn2,_fItransformed) =
                  f_ _fOc _fOinsideQuotation _fOscope _fOuni
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOtransformed)))
-- Formula -----------------------------------------------------
data Formula = Triple (Term) (Term) (Term)
             | Implication (Expression) (Expression)
             | Conjunction (Formula) (Formula)
             deriving ( Eq,Read,Show)
-- cata
sem_Formula :: Formula ->
               T_Formula
sem_Formula (Triple _s _p _o) =
    (sem_Formula_Triple (sem_Term _s) (sem_Term _p) (sem_Term _o))
sem_Formula (Implication _e1 _e2) =
    (sem_Formula_Implication (sem_Expression _e1) (sem_Expression _e2))
sem_Formula (Conjunction _c1 _c2) =
    (sem_Formula_Conjunction (sem_Formula _c1) (sem_Formula _c2))
-- semantic domain
type T_Formula = Int ->
                 Bool ->
                 ([String]) ->
                 ([String]) ->
                 ( Int,([String]),CFormula,([String]),CFormula,([String]),([String]),String,([String]),([String]),CFormula)
data Inh_Formula = Inh_Formula {c_Inh_Formula :: Int,insideQuotation_Inh_Formula :: Bool,scope_Inh_Formula :: ([String]),uni_Inh_Formula :: ([String])}
data Syn_Formula = Syn_Formula {count_Syn_Formula :: Int,deep_Syn_Formula :: ([String]),diff_Syn_Formula :: CFormula,ex_Syn_Formula :: ([String]),eye_Syn_Formula :: CFormula,eyeEx_Syn_Formula :: ([String]),eyeVar_Syn_Formula :: ([String]),formula_Syn_Formula :: String,n1_Syn_Formula :: ([String]),n2_Syn_Formula :: ([String]),transformed_Syn_Formula :: CFormula}
wrap_Formula :: T_Formula ->
                Inh_Formula ->
                Syn_Formula
wrap_Formula sem (Inh_Formula _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIuni) =
    (let ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed) = sem _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIuni
     in  (Syn_Formula _lhsOcount _lhsOdeep _lhsOdiff _lhsOex _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOn2 _lhsOtransformed))
sem_Formula_Triple :: T_Term ->
                      T_Term ->
                      T_Term ->
                      T_Formula
sem_Formula_Triple s_ p_ o_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope
       _lhsIuni ->
         (let _lhsOn2 :: ([String])
              _lhsOn1 :: ([String])
              _lhsOex :: ([String])
              _lhsOtransformed :: CFormula
              _lhsOformula :: String
              _lhsOeye :: CFormula
              _lhsOeyeVar :: ([String])
              _lhsOeyeEx :: ([String])
              _sOinsideQuotation :: Bool
              _pOinsideQuotation :: Bool
              _oOinsideQuotation :: Bool
              _sOvarInImpException :: Bool
              _pOvarInImpException :: Bool
              _oOvarInImpException :: Bool
              _lhsOdiff :: CFormula
              _sOc :: Int
              _pOc :: Int
              _oOc :: Int
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _sOscope :: ([String])
              _pOscope :: ([String])
              _oOscope :: ([String])
              _sIcount :: Int
              _sIdeep :: ([String])
              _sIdiff :: CTerm
              _sIex :: ([String])
              _sIeye :: CTerm
              _sIeyeEx :: ([String])
              _sIeyeVar :: ([String])
              _sIformula :: String
              _sIn1 :: ([String])
              _sIn2 :: ([String])
              _sItransformed :: CTerm
              _pIcount :: Int
              _pIdeep :: ([String])
              _pIdiff :: CTerm
              _pIex :: ([String])
              _pIeye :: CTerm
              _pIeyeEx :: ([String])
              _pIeyeVar :: ([String])
              _pIformula :: String
              _pIn1 :: ([String])
              _pIn2 :: ([String])
              _pItransformed :: CTerm
              _oIcount :: Int
              _oIdeep :: ([String])
              _oIdiff :: CTerm
              _oIex :: ([String])
              _oIeye :: CTerm
              _oIeyeEx :: ([String])
              _oIeyeVar :: ([String])
              _oIformula :: String
              _oIn1 :: ([String])
              _oIn2 :: ([String])
              _oItransformed :: CTerm
              _lhsOn2 =
                  ({-# LINE 175 "./attGr.ag" #-}
                   (_sIn2 `union` _pIn2) `union` _oIn2
                   {-# LINE 771 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 176 "./attGr.ag" #-}
                   (_sIn1 `union` _pIn1) `union` _oIn1
                   {-# LINE 776 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 177 "./attGr.ag" #-}
                   (_sIex `union` _pIex) `union` _oIex
                   {-# LINE 781 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 178 "./attGr.ag" #-}
                   CTriple _sItransformed _pItransformed _oItransformed
                   {-# LINE 786 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 179 "./attGr.ag" #-}
                   _sIformula ++ " " ++ _pIformula ++ " " ++ _oIformula
                   {-# LINE 791 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 180 "./attGr.ag" #-}
                   CTriple _sIeye _pIeye _oIeye
                   {-# LINE 796 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 181 "./attGr.ag" #-}
                   (_sIeyeVar `union` _pIeyeVar) `union` _oIeyeVar
                   {-# LINE 801 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 182 "./attGr.ag" #-}
                   (_sIeyeEx `union` _pIeyeEx) `union` _oIeyeEx
                   {-# LINE 806 "attGr.hs" #-}
                   )
              _sOinsideQuotation =
                  ({-# LINE 183 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 811 "attGr.hs" #-}
                   )
              _pOinsideQuotation =
                  ({-# LINE 184 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 816 "attGr.hs" #-}
                   )
              _oOinsideQuotation =
                  ({-# LINE 185 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 821 "attGr.hs" #-}
                   )
              _sOvarInImpException =
                  ({-# LINE 186 "./attGr.ag" #-}
                   impExc _pIeye _lhsIinsideQuotation
                   {-# LINE 826 "attGr.hs" #-}
                   )
              _pOvarInImpException =
                  ({-# LINE 187 "./attGr.ag" #-}
                   False
                   {-# LINE 831 "attGr.hs" #-}
                   )
              _oOvarInImpException =
                  ({-# LINE 188 "./attGr.ag" #-}
                   impExc _pIeye _lhsIinsideQuotation
                   {-# LINE 836 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 189 "./attGr.ag" #-}
                   CTriple _sIdiff _pIdiff _oIdiff
                   {-# LINE 841 "attGr.hs" #-}
                   )
              _sOc =
                  ({-# LINE 190 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 846 "attGr.hs" #-}
                   )
              _pOc =
                  ({-# LINE 191 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 851 "attGr.hs" #-}
                   )
              _oOc =
                  ({-# LINE 192 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 856 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 193 "./attGr.ag" #-}
                   max _sIcount (max _pIcount _oIcount)
                   {-# LINE 861 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 194 "./attGr.ag" #-}
                   choose _oIdeep  (choose _sIdeep _pIdeep _sIcount _pIcount) _oIcount (max _sIcount _pIcount)
                   {-# LINE 866 "attGr.hs" #-}
                   )
              _sOscope =
                  ({-# LINE 119 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 871 "attGr.hs" #-}
                   )
              _pOscope =
                  ({-# LINE 119 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 876 "attGr.hs" #-}
                   )
              _oOscope =
                  ({-# LINE 119 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 881 "attGr.hs" #-}
                   )
              ( _sIcount,_sIdeep,_sIdiff,_sIex,_sIeye,_sIeyeEx,_sIeyeVar,_sIformula,_sIn1,_sIn2,_sItransformed) =
                  s_ _sOc _sOinsideQuotation _sOscope _sOvarInImpException
              ( _pIcount,_pIdeep,_pIdiff,_pIex,_pIeye,_pIeyeEx,_pIeyeVar,_pIformula,_pIn1,_pIn2,_pItransformed) =
                  p_ _pOc _pOinsideQuotation _pOscope _pOvarInImpException
              ( _oIcount,_oIdeep,_oIdiff,_oIex,_oIeye,_oIeyeEx,_oIeyeVar,_oIformula,_oIn1,_oIn2,_oItransformed) =
                  o_ _oOc _oOinsideQuotation _oOscope _oOvarInImpException
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
sem_Formula_Implication :: T_Expression ->
                           T_Expression ->
                           T_Formula
sem_Formula_Implication e1_ e2_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope
       _lhsIuni ->
         (let _lhsOn2 :: ([String])
              _lhsOn1 :: ([String])
              _lhsOex :: ([String])
              _lhsOtransformed :: CFormula
              _lhsOformula :: String
              _lhsOeye :: CFormula
              _lhsOeyeVar :: ([String])
              _lhsOeyeEx :: ([String])
              _e1OinsideQuotation :: Bool
              _e2OinsideQuotation :: Bool
              _lhsOdiff :: CFormula
              _e1Oc :: Int
              _e2Oc :: Int
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _e1Oscope :: ([String])
              _e2Oscope :: ([String])
              _e1Icount :: Int
              _e1Ideep :: ([String])
              _e1Idiff :: CExpression
              _e1Ieye :: CExpression
              _e1IeyeEx :: ([String])
              _e1IeyeVar :: ([String])
              _e1Iformula :: String
              _e1In1 :: ([String])
              _e1Itransformed :: CExpression
              _e2Icount :: Int
              _e2Ideep :: ([String])
              _e2Idiff :: CExpression
              _e2Ieye :: CExpression
              _e2IeyeEx :: ([String])
              _e2IeyeVar :: ([String])
              _e2Iformula :: String
              _e2In1 :: ([String])
              _e2Itransformed :: CExpression
              _lhsOn2 =
                  ({-# LINE 197 "./attGr.ag" #-}
                   []
                   {-# LINE 936 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 198 "./attGr.ag" #-}
                   _e1In1 `union` _e2In1
                   {-# LINE 941 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 199 "./attGr.ag" #-}
                   []
                   {-# LINE 946 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 200 "./attGr.ag" #-}
                   CImplication _e1Itransformed _e2Itransformed
                   {-# LINE 951 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 201 "./attGr.ag" #-}
                   _e1Iformula ++ " -> " ++ _e2Iformula
                   {-# LINE 956 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 202 "./attGr.ag" #-}
                   CImplication _e1Ieye _e2Ieye
                   {-# LINE 961 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 203 "./attGr.ag" #-}
                   _e1IeyeVar `union` _e2IeyeVar
                   {-# LINE 966 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 204 "./attGr.ag" #-}
                   keepEx _lhsIinsideQuotation (_e1IeyeEx `union` _e2IeyeEx)
                   {-# LINE 971 "attGr.hs" #-}
                   )
              _e1OinsideQuotation =
                  ({-# LINE 205 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 976 "attGr.hs" #-}
                   )
              _e2OinsideQuotation =
                  ({-# LINE 206 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 981 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 207 "./attGr.ag" #-}
                   CImplication _e1Idiff _e2Idiff
                   {-# LINE 986 "attGr.hs" #-}
                   )
              _e1Oc =
                  ({-# LINE 208 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 991 "attGr.hs" #-}
                   )
              _e2Oc =
                  ({-# LINE 209 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 996 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 210 "./attGr.ag" #-}
                   max _e1Icount _e2Icount
                   {-# LINE 1001 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 211 "./attGr.ag" #-}
                   choose _e1Ideep _e2Ideep _e1Icount _e2Icount
                   {-# LINE 1006 "attGr.hs" #-}
                   )
              _e1Oscope =
                  ({-# LINE 140 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1011 "attGr.hs" #-}
                   )
              _e2Oscope =
                  ({-# LINE 140 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1016 "attGr.hs" #-}
                   )
              ( _e1Icount,_e1Ideep,_e1Idiff,_e1Ieye,_e1IeyeEx,_e1IeyeVar,_e1Iformula,_e1In1,_e1Itransformed) =
                  e1_ _e1Oc _e1OinsideQuotation _e1Oscope
              ( _e2Icount,_e2Ideep,_e2Idiff,_e2Ieye,_e2IeyeEx,_e2IeyeVar,_e2Iformula,_e2In1,_e2Itransformed) =
                  e2_ _e2Oc _e2OinsideQuotation _e2Oscope
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
sem_Formula_Conjunction :: T_Formula ->
                           T_Formula ->
                           T_Formula
sem_Formula_Conjunction c1_ c2_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope
       _lhsIuni ->
         (let _lhsOn2 :: ([String])
              _lhsOn1 :: ([String])
              _lhsOex :: ([String])
              _c1Oscope :: ([String])
              _c2Oscope :: ([String])
              _lhsOtransformed :: CFormula
              _lhsOformula :: String
              _lhsOeye :: CFormula
              _lhsOeyeVar :: ([String])
              _lhsOeyeEx :: ([String])
              _c1OinsideQuotation :: Bool
              _c2OinsideQuotation :: Bool
              _lhsOdiff :: CFormula
              _c1Oc :: Int
              _c2Oc :: Int
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _c1Ouni :: ([String])
              _c2Ouni :: ([String])
              _c1Icount :: Int
              _c1Ideep :: ([String])
              _c1Idiff :: CFormula
              _c1Iex :: ([String])
              _c1Ieye :: CFormula
              _c1IeyeEx :: ([String])
              _c1IeyeVar :: ([String])
              _c1Iformula :: String
              _c1In1 :: ([String])
              _c1In2 :: ([String])
              _c1Itransformed :: CFormula
              _c2Icount :: Int
              _c2Ideep :: ([String])
              _c2Idiff :: CFormula
              _c2Iex :: ([String])
              _c2Ieye :: CFormula
              _c2IeyeEx :: ([String])
              _c2IeyeVar :: ([String])
              _c2Iformula :: String
              _c2In1 :: ([String])
              _c2In2 :: ([String])
              _c2Itransformed :: CFormula
              _lhsOn2 =
                  ({-# LINE 213 "./attGr.ag" #-}
                   _c2In2 `union` _c1In2
                   {-# LINE 1075 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 214 "./attGr.ag" #-}
                   _c2In1 `union` _c1In1
                   {-# LINE 1080 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 215 "./attGr.ag" #-}
                   _c2Iex `union` _c1Iex
                   {-# LINE 1085 "attGr.hs" #-}
                   )
              _c1Oscope =
                  ({-# LINE 216 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1090 "attGr.hs" #-}
                   )
              _c2Oscope =
                  ({-# LINE 217 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1095 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 218 "./attGr.ag" #-}
                   CConjunction _c1Itransformed _c2Itransformed
                   {-# LINE 1100 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 219 "./attGr.ag" #-}
                   _c1Iformula ++ " ^ " ++ _c2Iformula
                   {-# LINE 1105 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 220 "./attGr.ag" #-}
                   CConjunction _c1Ieye _c2Ieye
                   {-# LINE 1110 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 221 "./attGr.ag" #-}
                   _c2IeyeVar `union` _c1IeyeVar
                   {-# LINE 1115 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 222 "./attGr.ag" #-}
                   _c2IeyeEx `union` _c1IeyeEx
                   {-# LINE 1120 "attGr.hs" #-}
                   )
              _c1OinsideQuotation =
                  ({-# LINE 223 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1125 "attGr.hs" #-}
                   )
              _c2OinsideQuotation =
                  ({-# LINE 224 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1130 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 225 "./attGr.ag" #-}
                   CConjunction _c1Idiff _c2Idiff
                   {-# LINE 1135 "attGr.hs" #-}
                   )
              _c1Oc =
                  ({-# LINE 226 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1140 "attGr.hs" #-}
                   )
              _c2Oc =
                  ({-# LINE 227 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1145 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 228 "./attGr.ag" #-}
                   max _c1Icount _c2Icount
                   {-# LINE 1150 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 229 "./attGr.ag" #-}
                   choose _c1Ideep _c2Ideep _c1Icount _c2Icount
                   {-# LINE 1155 "attGr.hs" #-}
                   )
              _c1Ouni =
                  ({-# LINE 98 "./attGr.ag" #-}
                   _lhsIuni
                   {-# LINE 1160 "attGr.hs" #-}
                   )
              _c2Ouni =
                  ({-# LINE 98 "./attGr.ag" #-}
                   _lhsIuni
                   {-# LINE 1165 "attGr.hs" #-}
                   )
              ( _c1Icount,_c1Ideep,_c1Idiff,_c1Iex,_c1Ieye,_c1IeyeEx,_c1IeyeVar,_c1Iformula,_c1In1,_c1In2,_c1Itransformed) =
                  c1_ _c1Oc _c1OinsideQuotation _c1Oscope _c1Ouni
              ( _c2Icount,_c2Ideep,_c2Idiff,_c2Iex,_c2Ieye,_c2IeyeEx,_c2IeyeVar,_c2Iformula,_c2In1,_c2In2,_c2Itransformed) =
                  c2_ _c2Oc _c2OinsideQuotation _c2Oscope _c2Ouni
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
-- Quant -------------------------------------------------------
data Quant = Forall (CTerm)
           | Forsome (CTerm)
           deriving ( Eq,Show)
-- cata
sem_Quant :: Quant ->
             T_Quant
sem_Quant (Forall _v) =
    (sem_Quant_Forall (sem_CTerm _v))
sem_Quant (Forsome _e) =
    (sem_Quant_Forsome (sem_CTerm _e))
-- semantic domain
type T_Quant = ( )
data Inh_Quant = Inh_Quant {}
data Syn_Quant = Syn_Quant {}
wrap_Quant :: T_Quant ->
              Inh_Quant ->
              Syn_Quant
wrap_Quant sem (Inh_Quant) =
    (let ( ) = sem
     in  (Syn_Quant))
sem_Quant_Forall :: T_CTerm ->
                    T_Quant
sem_Quant_Forall v_ =
    (let
     in  ( ))
sem_Quant_Forsome :: T_CTerm ->
                     T_Quant
sem_Quant_Forsome e_ =
    (let
     in  ( ))
-- S -----------------------------------------------------------
data S = Mainformula (Formula)
       deriving ( Eq,Read,Show)
-- cata
sem_S :: S ->
         T_S
sem_S (Mainformula _f) =
    (sem_S_Mainformula (sem_Formula _f))
-- semantic domain
type T_S = ( Int,([String]),CFormula,([String]),CFormula,([String]),([String]),String,([String]),([String]),CFormula)
data Inh_S = Inh_S {}
data Syn_S = Syn_S {count_Syn_S :: Int,deep_Syn_S :: ([String]),diff_Syn_S :: CFormula,ex_Syn_S :: ([String]),eye_Syn_S :: CFormula,eyeEx_Syn_S :: ([String]),eyeVar_Syn_S :: ([String]),formula_Syn_S :: String,n1_Syn_S :: ([String]),n2_Syn_S :: ([String]),transformed_Syn_S :: CFormula}
wrap_S :: T_S ->
          Inh_S ->
          Syn_S
wrap_S sem (Inh_S) =
    (let ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed) = sem
     in  (Syn_S _lhsOcount _lhsOdeep _lhsOdiff _lhsOex _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOn2 _lhsOtransformed))
sem_S_Mainformula :: T_Formula ->
                     T_S
sem_S_Mainformula f_ =
    (let _fOscope :: ([String])
         _fOuni :: ([String])
         _lhsOex :: ([String])
         _lhsOtransformed :: CFormula
         _lhsOformula :: String
         _lhsOeye :: CFormula
         _lhsOeyeEx :: ([String])
         _lhsOeyeVar :: ([String])
         _fOinsideQuotation :: Bool
         _lhsOdiff :: CFormula
         _fOc :: Int
         _lhsOcount :: Int
         _lhsOdeep :: ([String])
         _lhsOn1 :: ([String])
         _lhsOn2 :: ([String])
         _fIcount :: Int
         _fIdeep :: ([String])
         _fIdiff :: CFormula
         _fIex :: ([String])
         _fIeye :: CFormula
         _fIeyeEx :: ([String])
         _fIeyeVar :: ([String])
         _fIformula :: String
         _fIn1 :: ([String])
         _fIn2 :: ([String])
         _fItransformed :: CFormula
         _fOscope =
             ({-# LINE 160 "./attGr.ag" #-}
              _fIn1  `union` _fIn2
              {-# LINE 1253 "attGr.hs" #-}
              )
         _fOuni =
             ({-# LINE 161 "./attGr.ag" #-}
              _fIn1  `union` _fIn2
              {-# LINE 1258 "attGr.hs" #-}
              )
         _lhsOex =
             ({-# LINE 162 "./attGr.ag" #-}
              _fIex
              {-# LINE 1263 "attGr.hs" #-}
              )
         _lhsOtransformed =
             ({-# LINE 163 "./attGr.ag" #-}
              qua (sort (_fIn1  `union` _fIn2)) _fIex _fItransformed
              {-# LINE 1268 "attGr.hs" #-}
              )
         _lhsOformula =
             ({-# LINE 164 "./attGr.ag" #-}
              quaf (sort  (_fIn1  `union` _fIn2)) _fIex   _fIformula
              {-# LINE 1273 "attGr.hs" #-}
              )
         _lhsOeye =
             ({-# LINE 165 "./attGr.ag" #-}
              qua (sort _fIeyeVar ) _fIeyeEx _fIeye
              {-# LINE 1278 "attGr.hs" #-}
              )
         _lhsOeyeEx =
             ({-# LINE 166 "./attGr.ag" #-}
              _fIeyeEx
              {-# LINE 1283 "attGr.hs" #-}
              )
         _lhsOeyeVar =
             ({-# LINE 167 "./attGr.ag" #-}
              _fIeyeVar
              {-# LINE 1288 "attGr.hs" #-}
              )
         _fOinsideQuotation =
             ({-# LINE 168 "./attGr.ag" #-}
              False
              {-# LINE 1293 "attGr.hs" #-}
              )
         _lhsOdiff =
             ({-# LINE 169 "./attGr.ag" #-}
              qua (sort  _fIeyeVar) _fIex _fIdiff
              {-# LINE 1298 "attGr.hs" #-}
              )
         _fOc =
             ({-# LINE 170 "./attGr.ag" #-}
              1
              {-# LINE 1303 "attGr.hs" #-}
              )
         _lhsOcount =
             ({-# LINE 171 "./attGr.ag" #-}
              findDepth _fIn1 1 _fIcount
              {-# LINE 1308 "attGr.hs" #-}
              )
         _lhsOdeep =
             ({-# LINE 172 "./attGr.ag" #-}
              deepest _fIn1 _fIdeep
              {-# LINE 1313 "attGr.hs" #-}
              )
         _lhsOn1 =
             ({-# LINE 76 "./attGr.ag" #-}
              _fIn1
              {-# LINE 1318 "attGr.hs" #-}
              )
         _lhsOn2 =
             ({-# LINE 77 "./attGr.ag" #-}
              _fIn2
              {-# LINE 1323 "attGr.hs" #-}
              )
         ( _fIcount,_fIdeep,_fIdiff,_fIex,_fIeye,_fIeyeEx,_fIeyeVar,_fIformula,_fIn1,_fIn2,_fItransformed) =
             f_ _fOc _fOinsideQuotation _fOscope _fOuni
     in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed))
-- Term --------------------------------------------------------
data Term = Universal (String)
          | Existential (String)
          | URI (String)
          | Literal (String)
          | Exp (Expression)
          | List (Term) (Term)
          deriving ( Eq,Read,Show)
-- cata
sem_Term :: Term ->
            T_Term
sem_Term (Universal _u) =
    (sem_Term_Universal _u)
sem_Term (Existential _ex) =
    (sem_Term_Existential _ex)
sem_Term (URI _uri) =
    (sem_Term_URI _uri)
sem_Term (Literal _l) =
    (sem_Term_Literal _l)
sem_Term (Exp _e) =
    (sem_Term_Exp (sem_Expression _e))
sem_Term (List _term _list) =
    (sem_Term_List (sem_Term _term) (sem_Term _list))
-- semantic domain
type T_Term = Int ->
              Bool ->
              ([String]) ->
              Bool ->
              ( Int,([String]),CTerm,([String]),CTerm,([String]),([String]),String,([String]),([String]),CTerm)
data Inh_Term = Inh_Term {c_Inh_Term :: Int,insideQuotation_Inh_Term :: Bool,scope_Inh_Term :: ([String]),varInImpException_Inh_Term :: Bool}
data Syn_Term = Syn_Term {count_Syn_Term :: Int,deep_Syn_Term :: ([String]),diff_Syn_Term :: CTerm,ex_Syn_Term :: ([String]),eye_Syn_Term :: CTerm,eyeEx_Syn_Term :: ([String]),eyeVar_Syn_Term :: ([String]),formula_Syn_Term :: String,n1_Syn_Term :: ([String]),n2_Syn_Term :: ([String]),transformed_Syn_Term :: CTerm}
wrap_Term :: T_Term ->
             Inh_Term ->
             Syn_Term
wrap_Term sem (Inh_Term _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIvarInImpException) =
    (let ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed) = sem _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIvarInImpException
     in  (Syn_Term _lhsOcount _lhsOdeep _lhsOdiff _lhsOex _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOn2 _lhsOtransformed))
sem_Term_Universal :: String ->
                      T_Term
sem_Term_Universal u_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope
       _lhsIvarInImpException ->
         (let _lhsOn2 :: ([String])
              _lhsOn1 :: ([String])
              _lhsOex :: ([String])
              _lhsOtransformed :: CTerm
              _lhsOformula :: String
              _lhsOeyeVar :: ([String])
              _lhsOeye :: CTerm
              _lhsOeyeEx :: ([String])
              _lhsOdiff :: CTerm
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _lhsOn2 =
                  ({-# LINE 232 "./attGr.ag" #-}
                   ["U"++ u_]
                   {-# LINE 1386 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 233 "./attGr.ag" #-}
                   []
                   {-# LINE 1391 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 234 "./attGr.ag" #-}
                   []
                   {-# LINE 1396 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 235 "./attGr.ag" #-}
                   Var  ("U"++ u_)
                   {-# LINE 1401 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 236 "./attGr.ag" #-}
                   "U"++ u_
                   {-# LINE 1406 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 237 "./attGr.ag" #-}
                   ["U" ++ u_]
                   {-# LINE 1411 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 238 "./attGr.ag" #-}
                   Var  ("U"++ u_)
                   {-# LINE 1416 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 239 "./attGr.ag" #-}
                   []
                   {-# LINE 1421 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 240 "./attGr.ag" #-}
                   Var  ("U"++ u_)
                   {-# LINE 1426 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 241 "./attGr.ag" #-}
                   0
                   {-# LINE 1431 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 242 "./attGr.ag" #-}
                   []
                   {-# LINE 1436 "attGr.hs" #-}
                   )
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
sem_Term_Existential :: String ->
                        T_Term
sem_Term_Existential ex_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope
       _lhsIvarInImpException ->
         (let _lhsOn2 :: ([String])
              _lhsOn1 :: ([String])
              _lhsOex :: ([String])
              _lhsOtransformed :: CTerm
              _lhsOformula :: String
              _lhsOeyeVar :: ([String])
              _lhsOeye :: CTerm
              _lhsOeyeEx :: ([String])
              _lhsOdiff :: CTerm
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _lhsOn2 =
                  ({-# LINE 245 "./attGr.ag" #-}
                   []
                   {-# LINE 1460 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 246 "./attGr.ag" #-}
                   []
                   {-# LINE 1465 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 247 "./attGr.ag" #-}
                   ["E"++ ex_]
                   {-# LINE 1470 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 248 "./attGr.ag" #-}
                   Var ("E"++ ex_)
                   {-# LINE 1475 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 249 "./attGr.ag" #-}
                   "E"++ ex_
                   {-# LINE 1480 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 250 "./attGr.ag" #-}
                   []
                   {-# LINE 1485 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 251 "./attGr.ag" #-}
                   Var ("E"++ ex_)
                   {-# LINE 1490 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 252 "./attGr.ag" #-}
                   ["E"++ ex_]
                   {-# LINE 1495 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 253 "./attGr.ag" #-}
                   Var ("E"++ ex_)
                   {-# LINE 1500 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 254 "./attGr.ag" #-}
                   0
                   {-# LINE 1505 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 255 "./attGr.ag" #-}
                   []
                   {-# LINE 1510 "attGr.hs" #-}
                   )
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
sem_Term_URI :: String ->
                T_Term
sem_Term_URI uri_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope
       _lhsIvarInImpException ->
         (let _lhsOn2 :: ([String])
              _lhsOn1 :: ([String])
              _lhsOex :: ([String])
              _lhsOtransformed :: CTerm
              _lhsOformula :: String
              _lhsOeyeVar :: ([String])
              _lhsOeye :: CTerm
              _lhsOeyeEx :: ([String])
              _lhsOdiff :: CTerm
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _lhsOn2 =
                  ({-# LINE 257 "./attGr.ag" #-}
                   []
                   {-# LINE 1534 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 258 "./attGr.ag" #-}
                   []
                   {-# LINE 1539 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 259 "./attGr.ag" #-}
                   []
                   {-# LINE 1544 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 260 "./attGr.ag" #-}
                   Con ("uri_"++ uri_)
                   {-# LINE 1549 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 261 "./attGr.ag" #-}
                   "uri_"++ uri_
                   {-# LINE 1554 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 262 "./attGr.ag" #-}
                   []
                   {-# LINE 1559 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 263 "./attGr.ag" #-}
                   Con ("uri_" ++ uri_)
                   {-# LINE 1564 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 264 "./attGr.ag" #-}
                   []
                   {-# LINE 1569 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 265 "./attGr.ag" #-}
                   Con ("uri_" ++ uri_)
                   {-# LINE 1574 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 266 "./attGr.ag" #-}
                   0
                   {-# LINE 1579 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 267 "./attGr.ag" #-}
                   []
                   {-# LINE 1584 "attGr.hs" #-}
                   )
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
sem_Term_Literal :: String ->
                    T_Term
sem_Term_Literal l_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope
       _lhsIvarInImpException ->
         (let _lhsOn2 :: ([String])
              _lhsOn1 :: ([String])
              _lhsOex :: ([String])
              _lhsOtransformed :: CTerm
              _lhsOformula :: String
              _lhsOeyeVar :: ([String])
              _lhsOeye :: CTerm
              _lhsOeyeEx :: ([String])
              _lhsOdiff :: CTerm
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _lhsOn2 =
                  ({-# LINE 269 "./attGr.ag" #-}
                   []
                   {-# LINE 1608 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 270 "./attGr.ag" #-}
                   []
                   {-# LINE 1613 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 271 "./attGr.ag" #-}
                   []
                   {-# LINE 1618 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 272 "./attGr.ag" #-}
                   Con ("l_"++ l_)
                   {-# LINE 1623 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 273 "./attGr.ag" #-}
                   "l_"++ l_
                   {-# LINE 1628 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 274 "./attGr.ag" #-}
                   []
                   {-# LINE 1633 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 275 "./attGr.ag" #-}
                   Con ("l_"++ l_)
                   {-# LINE 1638 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 276 "./attGr.ag" #-}
                   []
                   {-# LINE 1643 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 277 "./attGr.ag" #-}
                   Con ("l_"++ l_)
                   {-# LINE 1648 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 278 "./attGr.ag" #-}
                   0
                   {-# LINE 1653 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 279 "./attGr.ag" #-}
                   []
                   {-# LINE 1658 "attGr.hs" #-}
                   )
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
sem_Term_Exp :: T_Expression ->
                T_Term
sem_Term_Exp e_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope
       _lhsIvarInImpException ->
         (let _lhsOn2 :: ([String])
              _lhsOn1 :: ([String])
              _lhsOex :: ([String])
              _lhsOtransformed :: CTerm
              _lhsOformula :: String
              _lhsOeye :: CTerm
              _lhsOeyeEx :: ([String])
              _eOinsideQuotation :: Bool
              _lhsOdiff :: CTerm
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _lhsOeyeVar :: ([String])
              _eOc :: Int
              _eOscope :: ([String])
              _eIcount :: Int
              _eIdeep :: ([String])
              _eIdiff :: CExpression
              _eIeye :: CExpression
              _eIeyeEx :: ([String])
              _eIeyeVar :: ([String])
              _eIformula :: String
              _eIn1 :: ([String])
              _eItransformed :: CExpression
              _lhsOn2 =
                  ({-# LINE 281 "./attGr.ag" #-}
                   []
                   {-# LINE 1694 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 282 "./attGr.ag" #-}
                   _eIn1
                   {-# LINE 1699 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 283 "./attGr.ag" #-}
                   []
                   {-# LINE 1704 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 284 "./attGr.ag" #-}
                   CExp _eItransformed
                   {-# LINE 1709 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 285 "./attGr.ag" #-}
                   _eIformula
                   {-# LINE 1714 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 286 "./attGr.ag" #-}
                   CExp _eIeye
                   {-# LINE 1719 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 287 "./attGr.ag" #-}
                   exception _lhsIvarInImpException _lhsIinsideQuotation _eIeyeEx
                   {-# LINE 1724 "attGr.hs" #-}
                   )
              _eOinsideQuotation =
                  ({-# LINE 288 "./attGr.ag" #-}
                   True
                   {-# LINE 1729 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 289 "./attGr.ag" #-}
                   CExp _eIdiff
                   {-# LINE 1734 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 290 "./attGr.ag" #-}
                   _eIcount
                   {-# LINE 1739 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 291 "./attGr.ag" #-}
                   _eIdeep
                   {-# LINE 1744 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 125 "./attGr.ag" #-}
                   _eIeyeVar
                   {-# LINE 1749 "attGr.hs" #-}
                   )
              _eOc =
                  ({-# LINE 152 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1754 "attGr.hs" #-}
                   )
              _eOscope =
                  ({-# LINE 140 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1759 "attGr.hs" #-}
                   )
              ( _eIcount,_eIdeep,_eIdiff,_eIeye,_eIeyeEx,_eIeyeVar,_eIformula,_eIn1,_eItransformed) =
                  e_ _eOc _eOinsideQuotation _eOscope
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
sem_Term_List :: T_Term ->
                 T_Term ->
                 T_Term
sem_Term_List term_ list_ =
    (\ _lhsIc
       _lhsIinsideQuotation
       _lhsIscope
       _lhsIvarInImpException ->
         (let _lhsOn2 :: ([String])
              _lhsOn1 :: ([String])
              _lhsOex :: ([String])
              _lhsOtransformed :: CTerm
              _lhsOformula :: String
              _lhsOeyeVar :: ([String])
              _lhsOeye :: CTerm
              _lhsOeyeEx :: ([String])
              _lhsOdiff :: CTerm
              _termOc :: Int
              _listOc :: Int
              _lhsOcount :: Int
              _lhsOdeep :: ([String])
              _termOinsideQuotation :: Bool
              _termOscope :: ([String])
              _termOvarInImpException :: Bool
              _listOinsideQuotation :: Bool
              _listOscope :: ([String])
              _listOvarInImpException :: Bool
              _termIcount :: Int
              _termIdeep :: ([String])
              _termIdiff :: CTerm
              _termIex :: ([String])
              _termIeye :: CTerm
              _termIeyeEx :: ([String])
              _termIeyeVar :: ([String])
              _termIformula :: String
              _termIn1 :: ([String])
              _termIn2 :: ([String])
              _termItransformed :: CTerm
              _listIcount :: Int
              _listIdeep :: ([String])
              _listIdiff :: CTerm
              _listIex :: ([String])
              _listIeye :: CTerm
              _listIeyeEx :: ([String])
              _listIeyeVar :: ([String])
              _listIformula :: String
              _listIn1 :: ([String])
              _listIn2 :: ([String])
              _listItransformed :: CTerm
              _lhsOn2 =
                  ({-# LINE 294 "./attGr.ag" #-}
                   _termIn2 `union` _listIn2
                   {-# LINE 1816 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 295 "./attGr.ag" #-}
                   _termIn1 `union` _listIn1
                   {-# LINE 1821 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 296 "./attGr.ag" #-}
                   _termIex `union` _listIex
                   {-# LINE 1826 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 297 "./attGr.ag" #-}
                   CList _termItransformed _listItransformed
                   {-# LINE 1831 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 298 "./attGr.ag" #-}
                   "(" ++ _termIformula ++ "," ++ _listIformula ++ ")"
                   {-# LINE 1836 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 299 "./attGr.ag" #-}
                   _termIeyeVar ++ _listIeyeVar
                   {-# LINE 1841 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 300 "./attGr.ag" #-}
                   CList _termItransformed _listItransformed
                   {-# LINE 1846 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 301 "./attGr.ag" #-}
                   _termIeyeEx ++ _listIeyeEx
                   {-# LINE 1851 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 302 "./attGr.ag" #-}
                   CList _termIdiff _listIdiff
                   {-# LINE 1856 "attGr.hs" #-}
                   )
              _termOc =
                  ({-# LINE 303 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1861 "attGr.hs" #-}
                   )
              _listOc =
                  ({-# LINE 304 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1866 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 305 "./attGr.ag" #-}
                   max _termIcount _listIcount
                   {-# LINE 1871 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 306 "./attGr.ag" #-}
                   _termIdeep `union` _listIdeep
                   {-# LINE 1876 "attGr.hs" #-}
                   )
              _termOinsideQuotation =
                  ({-# LINE 127 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1881 "attGr.hs" #-}
                   )
              _termOscope =
                  ({-# LINE 119 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1886 "attGr.hs" #-}
                   )
              _termOvarInImpException =
                  ({-# LINE 128 "./attGr.ag" #-}
                   _lhsIvarInImpException
                   {-# LINE 1891 "attGr.hs" #-}
                   )
              _listOinsideQuotation =
                  ({-# LINE 127 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1896 "attGr.hs" #-}
                   )
              _listOscope =
                  ({-# LINE 119 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1901 "attGr.hs" #-}
                   )
              _listOvarInImpException =
                  ({-# LINE 128 "./attGr.ag" #-}
                   _lhsIvarInImpException
                   {-# LINE 1906 "attGr.hs" #-}
                   )
              ( _termIcount,_termIdeep,_termIdiff,_termIex,_termIeye,_termIeyeEx,_termIeyeVar,_termIformula,_termIn1,_termIn2,_termItransformed) =
                  term_ _termOc _termOinsideQuotation _termOscope _termOvarInImpException
              ( _listIcount,_listIdeep,_listIdiff,_listIex,_listIeye,_listIeyeEx,_listIeyeVar,_listIformula,_listIn1,_listIn2,_listItransformed) =
                  list_ _listOc _listOinsideQuotation _listOscope _listOvarInImpException
          in  ( _lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))