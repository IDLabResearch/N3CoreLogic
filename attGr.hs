

-- UUAGC 0.9.42.3 (attGr.ag)

{-# LINE 4 "./attGr.ag" #-}



import Data.List
import Text.Parsec.Prim
import N3grammar
import System.Directory
import Data.Text (pack, count)

{-# LINE 16 "attGr.hs" #-}
{-# LINE 355 "./attGr.ag" #-}





             
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
makeTree fname "uni" = do {
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
makeTree fname "eye" = do {
                input <- readFile fname
                ; case runParser mainparser 0 fname input of
                { Left err -> print err
                ; Right ans -> putStrLn $ printTree  (show  (  diff_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )) 0
                }
                }


--simple function to compare tree strings, later that comparison should be on tree level
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


--hasBuiltins :: String -> Int
hasBuiltins foname [] result = do { return (reverse result) } 
hasBuiltins foname fs result =
                    do {
                        let fname = (head fs)
                        ; input <- readFile $ foname ++ "/"++ fname
                        ; case   ( (count (pack ("e:")) (pack input) ) > 1) 
                         || ( (count (pack ("prolog:")) (pack input) ) > 1) 
                         || ( (count (pack ("fn:")) (pack input) ) > 1)  
                         || ( (count (pack ("crypto:")) (pack input) ) > 1) 
                         || ( (count (pack ("list:")) (pack input) ) > 1) 
                         || ( (count (pack ("log:")) (pack input) ) > 1)  
                         || ( (count (pack ("math:")) (pack input) ) > 1) 
                         || (isInfixOf " rdf:first" input) 
                         || (isInfixOf " rdf:rest" input) 
                         || ( (count (pack ("string:")) (pack input) ) > 1) 
                         || ( (count (pack ("time:")) (pack input) ) > 1) 
                         || ( (count (pack ("func:")) (pack input) ) > 1) 
                         || ( (count (pack ("pred:")) (pack input) ) > 1) 
                        of
                        {
                         True -> (hasBuiltins foname (tail fs) (1:result))
                         ; False -> (hasBuiltins foname (tail fs) (0:result))
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
               --; builtins <- hasBuiltins foname newfs []
               ;   (values, count) <- (readAll foname newfs [] [0, 0, 0, 0, 0])
               ; let table = makeTable newfs values count
               ; writeFile target $ ((replicate 40 ' ') ++ "cwm/es2  " ++ "cwm/eye  " ++ "eye/es2  " ++ "builtins  "++ "depth \n" )++ table
               }


--readAll :: String -> [String] ->  -> IO() 
readAll foname [] list [a, b, c, d, e] = do { 
                            return $ ((reverse list), [a,b,c,d, e])
                            }
readAll foname fs list [a, b, c, d, e]  = do { 
                         let fname = (head fs)
                         ; input <- readFile $ foname ++ "/"++ fname
                         ; print (foname ++ "/"++ fname ++ " parsed \n" )  
                                    ;case runParser mainparser 0 fname input of
                                    {
                                    Left  err ->   (readAll foname (tail fs) ( [2, 2, 2, 2, 2, 2 ]:list ) [a, b, c, d, e] )
                                    ;Right ans -> (\res -> ((\x y z zz zzzz zzz -> (readAll foname (tail fs) (
                                                             [x, 
                                                              y, 
                                                              z,
                                                              zz,
                                                              zzzz,
                                                              zzz  
                                                             ]:list ) 
                                                             [(a+x), (b+y), (c+z), (d+1), (e+zzzz) ]
                                                             ) )
                                                             (cwm_and_eye res )
                                                             (cwm_and_diff res)
                                                             (eye_and_diff res)
                                                             (boolToInt ( (eyeVar_Syn_S (  wrap_S  (sem_S  res) Inh_S   ))==[]))
                                                             (be_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  ))
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
makeTable names ([a , b, c , 1, e1, e]:v) [s1, s2, s3, s4, s5] = (makeTable (tail names) v [(s1-a), (s2-b), (s3-c), (s4-1),(s5-e1)])
makeTable names ([a , b, c , d, e1, e]:v) stat = (head names) ++ (replicate (40 - length(head names)) ' ') ++ (concat $ fmap makeEntry [a, b, c, e1,  e ]) ++ "\n"
                         ++ (makeTable (tail names) v stat)


lastLine :: [Int]-> String
lastLine [a,b,c, d, e] = "Absolute Number (from " 
                      ++ (show d) ++ "): " 
                      ++ (replicate(15 - (length(show d))) ' ')
                      ++ (show a) ++ (replicate (9 - (length(show a))) ' ')
                      ++ (show b) ++ (replicate (9 - (length(show b))) ' ')
                      ++ (show c) ++ (replicate (9 - (length(show c))) ' ')
                      ++ (show e) 
                      ++ "\n"
                      ++ "Percentage: "
                      ++ (show ((fromIntegral a)/(fromIntegral d)))
                      ++" "
                      ++ (show $ (fromIntegral b)/(fromIntegral d))
                      ++ " "
                      ++ (show $ (fromIntegral c)/(fromIntegral d))
                      ++ " "
                      ++ (show $ (fromIntegral e)/(fromIntegral d))
 
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

isBuiltIn :: String -> Int
isBuiltIn string 
          | elem string builtinlist = 1
          | isPrefixOf "prolog:" string = 1
          | otherwise = 0

builtinlist = ["e:avg",
                "e:becomes",
                "e:biconditional", 
                "e:binaryEntropy", 
                "e:calculate", 
                "e:call", 
                "e:cartesianProduct", 
                "e:compoundTerm", 
                "e:cov", 
                "e:csvTuple", 
                "e:derive", 
                "e:fail",
                "e:finalize",
                "e:findall",
                "e:firstRest",
                "e:format",
                "e:graphCopy",
                "e:graphDifference",
                "e:graphIntersection",
                "e:graphList",
                "e:graphMember",
                "e:graphPair",
                "e:hmac-sha",
                "e:ignore",
                "e:label",
                "e:labelvars",
                "e:length",
                "e:match",
                "e:max",
                "e:min",
                "e:multisetEqualTo",
                "e:multisetNotEqualTo",
                "e:notLabel",
                "e:numeral",
                "e:optional",
                "e:pcc",
                "e:prefix",
                "e:propertyChainExtension",
                "e:random",
                "e:relabel",
                "e:rms",
                "e:roc",
                "e:roots",
                "e:sha",
                "e:sigmoid",
                "e:skolem",
                "e:sort",
                "e:std",
                "e:stringEscape",
                "e:stringReverse",
                "e:stringSplit",
                "e:subsequence",
                "e:trace",
                "e:transaction",
                "e:transpose",
                "e:tripleList",
                "e:tuple",
                "e:unique",
                "e:whenGround",
                "e:wwwFormEncode",
                "crypto:sha",
                "list:append",
                "list:first",
                "list:in",
                "list:last",
                "list:member",
                "list:rest",
                "log:conclusion",
                "log:conjunction",
                "log:dtlit",
                "log:equalTo",
                "log:implies",
                "log:includes",
                "log:notEqualTo",
                "log:notIncludes",
                "log:outputString",
                "log:rawType",
                "log:semantics",
                "log:uri",
                "math:absoluteValue",
                "math:atan2",
                "math:cos",
                "math:cosh",
                "math:degrees",
                "math:difference",
                "math:equalTo",
                "math:exponentiation",
                "math:greaterThan",
                "math:integerQuotient",
                "math:lessThan",
                "math:memberCount",
                "math:negation",
                "math:notEqualTo",
                "math:notGreaterThan",
                "math:notLessThan",
                "math:product",
                "math:quotient",
                "math:remainder",
                "math:rounded",
                "math:sin",
                "math:sinh",
                "math:sum",
                "math:tan",
                "math:tanh",
                "rdf:first",
                "rdf:rest",
                "string:concatenation",
                "string:contains",
                "string:containsIgnoringCase",
                "string:endsWith",
                "string:equalIgnoringCase",
                "string:greaterThan",
                "string:lessThan",
                "string:matches",
                "string:notEqualIgnoringCase",
                "string:notGreaterThan",
                "string:notLessThan",
                "string:notMatches",
                "string:replace",
                "string:scrape",
                "string:search",
                "string:startsWith",
                "time:day",
                "time:month",
                "time:year",
                "pred:literal-not-identical",
                "pred:iri-string",
                "pred:numeric-equal",
                "pred:numeric-less-than",
                "pred:numeric-greater-than",
                "pred:numeric-not-equal",
                "pred:numeric-less-than-or-equal",
                "pred:numeric-greater-than-or-equal",
                "func:not",
                "pred:boolean-equal",
                "pred:boolean-less-than",
                "pred:boolean-greater-than",
                "func:compare",
                "func:concat",
                "func:string-join",
                "func:substring",
                "func:string-length",
                "func:upper-case",
                "func:lower-case",
                "func:encode-for-uri",
                "func:substring-before",
                "func:substring-after",
                "pred:contains",
                "pred:starts-with",
                "pred:ends-with",
                "pred:matches",
                "func:year-from-dateTime",
                "func:month-from-dateTime",
                "func:day-from-dateTime",
                "func:hours-from-dateTime",
                "func:minutes-from-dateTime",
                "func:seconds-from-dateTime",
                "func:year-from-date",
                "func:month-from-date",
                "func:day-from-date",
                "func:hours-from-time",
                "func:minutes-from-time",
                "func:seconds-from-time",
                "func:years-from-duration",
                "func:months-from-duration",
                "func:days-from-duration",
                "func:hours-from-duration",
                "func:minutes-from-duration",
                "func:seconds-from-duration",
                "func:timezone-from-dateTime",
                "func:timezone-from-date",
                "func:timezone-from-time",
                "func:subtract-dateTimes",
                "func:subtract-dates",
                "func:subtract-times",
                "func:add-yearMonthDurations",
                "func:subtract-yearMonthDurations",
                "func:multiply-yearMonthDuration",
                "func:divide-yearMonthDuration",
                "func:divide-yearMonthDuration-by-yearMonthDuration",
                "func:add-dayTimeDurations",
                "func:subtract-dayTimeDurations",
                "func:multiply-dayTimeDuration",
                "func:divide-dayTimeDuration",
                "func:divide-dayTimeDuration-by-dayTimeDuration",
                "func:add-yearMonthDuration-to-dateTime",
                "func:add-yearMonthDuration-to-date",
                "func:add-dayTimeDuration-to-dateTime",
                "func:add-dayTimeDuration-to-date",
                "func:add-dayTimeDuration-to-time",
                "func:subtract-yearMonthDuration-from-dateTime",
                "func:subtract-yearMonthDuration-from-date",
                "func:subtract-dayTimeDuration-from-dateTime",
                "func:subtract-dayTimeDuration-from-date",
                "func:subtract-dayTimeDuration-from-time",
                "pred:dateTime-equal",
                "pred:dateTime-less-than",
                "pred:dateTime-greater-than",
                "pred:date-equal",
                "pred:date-less-than",
                "pred:date-greater-than",
                "pred:time-equal",
                "pred:time-less-than",
                "pred:time-greater-than",
                "pred:duration-equal",
                "pred:dayTimeDuration-less-than",
                "pred:dayTimeDuration-greater-than",
                "pred:yearMonthDuration-less-than",
                "pred:yearMonthDuration-greater-than",
                "pred:dateTime-not-equal",
                "pred:dateTime-less-than-or-equal",
                "pred:dateTime-greater-than-or-equal",
                "pred:date-not-equal",
                "pred:date-less-than-or-equal",
                "pred:date-greater-than-or-equal",
                "pred:time-not-equal",
                "pred:time-less-than-or-equal",
                "pred:time-greater-than-or-equal",
                "pred:duration-not-equal",
                "pred:dayTimeDuration-less-than-or-equal",
                "pred:dayTimeDuration-greater-than-or-equal",
                "pred:yearMonthDuration-less-than-or-equal",
                "pred:yearMonthDuration-greater-than-or-equal",
                "func:PlainLiteral-from-string-lang",
                "func:string-from-PlainLiteral",
                "func:lang-from-PlainLiteral",
                "func:PlainLiteral-compare",
                "func:PlainLiteral-length",
                "pred:matches-language-range",
                "pred:is-list",
                "pred:list-contains",
                "func:make-list",
                "func:count",
                "func:get",
                "func:sublist",
                "func:append",
                "func:concatenate",
                "func:insert-before",
                "func:remove",
                "func:reverse",
                "func:index-of",
                "func:union",
                "func:distinct-values",
                "func:intersect",
                "func:except"
                ]

{-# LINE 635 "attGr.hs" #-}
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
                    ( Int,Int,([String]),CExpression,CExpression,([String]),([String]),String,([String]),CExpression)
data Inh_Expression = Inh_Expression {c_Inh_Expression :: Int,insideQuotation_Inh_Expression :: Bool,scope_Inh_Expression :: ([String])}
data Syn_Expression = Syn_Expression {be_Syn_Expression :: Int,count_Syn_Expression :: Int,deep_Syn_Expression :: ([String]),diff_Syn_Expression :: CExpression,eye_Syn_Expression :: CExpression,eyeEx_Syn_Expression :: ([String]),eyeVar_Syn_Expression :: ([String]),formula_Syn_Expression :: String,n1_Syn_Expression :: ([String]),transformed_Syn_Expression :: CExpression}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression sem (Inh_Expression _lhsIc _lhsIinsideQuotation _lhsIscope) =
    (let ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOtransformed) = sem _lhsIc _lhsIinsideQuotation _lhsIscope
     in  (Syn_Expression _lhsObe _lhsOcount _lhsOdeep _lhsOdiff _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOtransformed))
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
              _lhsObe :: Int
              _lhsOn1 =
                  ({-# LINE 328 "./attGr.ag" #-}
                   []
                   {-# LINE 810 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 329 "./attGr.ag" #-}
                   CBE b_
                   {-# LINE 815 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 330 "./attGr.ag" #-}
                   if b_ == True then "<>" else "false"
                   {-# LINE 820 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 331 "./attGr.ag" #-}
                   []
                   {-# LINE 825 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 332 "./attGr.ag" #-}
                   CBE b_
                   {-# LINE 830 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 333 "./attGr.ag" #-}
                   []
                   {-# LINE 835 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 334 "./attGr.ag" #-}
                   CBE b_
                   {-# LINE 840 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 335 "./attGr.ag" #-}
                   0
                   {-# LINE 845 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 336 "./attGr.ag" #-}
                   []
                   {-# LINE 850 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 337 "./attGr.ag" #-}
                   0
                   {-# LINE 855 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOtransformed)))
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
              _lhsObe :: Int
              _lhsOeyeVar :: ([String])
              _fIbe :: Int
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
                  ({-# LINE 339 "./attGr.ag" #-}
                   _fIn2
                   {-# LINE 893 "attGr.hs" #-}
                   )
              _fOscope =
                  ({-# LINE 340 "./attGr.ag" #-}
                   _lhsIscope `union` _fIn1
                   {-# LINE 898 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 341 "./attGr.ag" #-}
                   CFE (qua (_fIn1 \\ _lhsIscope) _fIex _fItransformed)
                   {-# LINE 903 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 342 "./attGr.ag" #-}
                   "<"++ (quaf (_fIn1 \\ _lhsIscope) _fIex _fIformula) ++">"
                   {-# LINE 908 "attGr.hs" #-}
                   )
              _fOuni =
                  ({-# LINE 343 "./attGr.ag" #-}
                   _fIn1 \\ _lhsIscope
                   {-# LINE 913 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 344 "./attGr.ag" #-}
                   CFE (qua [] (imp _lhsIinsideQuotation _fIeyeEx) _fIeye )
                   {-# LINE 918 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 345 "./attGr.ag" #-}
                   _fIeyeEx
                   {-# LINE 923 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 346 "./attGr.ag" #-}
                   CFE (qua [] _fIex _fIdiff)
                   {-# LINE 928 "attGr.hs" #-}
                   )
              _fOinsideQuotation =
                  ({-# LINE 347 "./attGr.ag" #-}
                   True
                   {-# LINE 933 "attGr.hs" #-}
                   )
              _fOc =
                  ({-# LINE 348 "./attGr.ag" #-}
                   _lhsIc+1
                   {-# LINE 938 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 349 "./attGr.ag" #-}
                   findDepth (_fIn1 \\ _lhsIscope) (_lhsIc +1) _fIcount
                   {-# LINE 943 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 350 "./attGr.ag" #-}
                   deepest (_fIn1 \\ _lhsIscope) _fIdeep
                   {-# LINE 948 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 351 "./attGr.ag" #-}
                   _fIbe
                   {-# LINE 953 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 150 "./attGr.ag" #-}
                   _fIeyeVar
                   {-# LINE 958 "attGr.hs" #-}
                   )
              ( _fIbe,_fIcount,_fIdeep,_fIdiff,_fIex,_fIeye,_fIeyeEx,_fIeyeVar,_fIformula,_fIn1,_fIn2,_fItransformed) =
                  f_ _fOc _fOinsideQuotation _fOscope _fOuni
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOtransformed)))
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
                 ( Int,Int,([String]),CFormula,([String]),CFormula,([String]),([String]),String,([String]),([String]),CFormula)
data Inh_Formula = Inh_Formula {c_Inh_Formula :: Int,insideQuotation_Inh_Formula :: Bool,scope_Inh_Formula :: ([String]),uni_Inh_Formula :: ([String])}
data Syn_Formula = Syn_Formula {be_Syn_Formula :: Int,count_Syn_Formula :: Int,deep_Syn_Formula :: ([String]),diff_Syn_Formula :: CFormula,ex_Syn_Formula :: ([String]),eye_Syn_Formula :: CFormula,eyeEx_Syn_Formula :: ([String]),eyeVar_Syn_Formula :: ([String]),formula_Syn_Formula :: String,n1_Syn_Formula :: ([String]),n2_Syn_Formula :: ([String]),transformed_Syn_Formula :: CFormula}
wrap_Formula :: T_Formula ->
                Inh_Formula ->
                Syn_Formula
wrap_Formula sem (Inh_Formula _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIuni) =
    (let ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed) = sem _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIuni
     in  (Syn_Formula _lhsObe _lhsOcount _lhsOdeep _lhsOdiff _lhsOex _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOn2 _lhsOtransformed))
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
              _lhsObe :: Int
              _sOscope :: ([String])
              _pOscope :: ([String])
              _oOscope :: ([String])
              _sIbe :: Int
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
              _pIbe :: Int
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
              _oIbe :: Int
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
                  ({-# LINE 183 "./attGr.ag" #-}
                   (_sIn2 `union` _pIn2) `union` _oIn2
                   {-# LINE 1063 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 184 "./attGr.ag" #-}
                   (_sIn1 `union` _pIn1) `union` _oIn1
                   {-# LINE 1068 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 185 "./attGr.ag" #-}
                   (_sIex `union` _pIex) `union` _oIex
                   {-# LINE 1073 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 186 "./attGr.ag" #-}
                   CTriple _sItransformed _pItransformed _oItransformed
                   {-# LINE 1078 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 187 "./attGr.ag" #-}
                   _sIformula ++ " " ++ _pIformula ++ " " ++ _oIformula
                   {-# LINE 1083 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 188 "./attGr.ag" #-}
                   CTriple _sIeye _pIeye _oIeye
                   {-# LINE 1088 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 189 "./attGr.ag" #-}
                   (_sIeyeVar `union` _pIeyeVar) `union` _oIeyeVar
                   {-# LINE 1093 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 190 "./attGr.ag" #-}
                   (_sIeyeEx `union` _pIeyeEx) `union` _oIeyeEx
                   {-# LINE 1098 "attGr.hs" #-}
                   )
              _sOinsideQuotation =
                  ({-# LINE 191 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1103 "attGr.hs" #-}
                   )
              _pOinsideQuotation =
                  ({-# LINE 192 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1108 "attGr.hs" #-}
                   )
              _oOinsideQuotation =
                  ({-# LINE 193 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1113 "attGr.hs" #-}
                   )
              _sOvarInImpException =
                  ({-# LINE 194 "./attGr.ag" #-}
                   impExc _pIeye _lhsIinsideQuotation
                   {-# LINE 1118 "attGr.hs" #-}
                   )
              _pOvarInImpException =
                  ({-# LINE 195 "./attGr.ag" #-}
                   False
                   {-# LINE 1123 "attGr.hs" #-}
                   )
              _oOvarInImpException =
                  ({-# LINE 196 "./attGr.ag" #-}
                   impExc _pIeye _lhsIinsideQuotation
                   {-# LINE 1128 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 197 "./attGr.ag" #-}
                   CTriple _sIdiff _pIdiff _oIdiff
                   {-# LINE 1133 "attGr.hs" #-}
                   )
              _sOc =
                  ({-# LINE 198 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1138 "attGr.hs" #-}
                   )
              _pOc =
                  ({-# LINE 199 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1143 "attGr.hs" #-}
                   )
              _oOc =
                  ({-# LINE 200 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1148 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 201 "./attGr.ag" #-}
                   max _sIcount (max _pIcount _oIcount)
                   {-# LINE 1153 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 202 "./attGr.ag" #-}
                   choose _oIdeep  (choose _sIdeep _pIdeep _sIcount _pIcount) _oIcount (max _sIcount _pIcount)
                   {-# LINE 1158 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 203 "./attGr.ag" #-}
                   maximum [_sIbe, _pIbe, _oIbe]
                   {-# LINE 1163 "attGr.hs" #-}
                   )
              _sOscope =
                  ({-# LINE 122 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1168 "attGr.hs" #-}
                   )
              _pOscope =
                  ({-# LINE 122 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1173 "attGr.hs" #-}
                   )
              _oOscope =
                  ({-# LINE 122 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1178 "attGr.hs" #-}
                   )
              ( _sIbe,_sIcount,_sIdeep,_sIdiff,_sIex,_sIeye,_sIeyeEx,_sIeyeVar,_sIformula,_sIn1,_sIn2,_sItransformed) =
                  s_ _sOc _sOinsideQuotation _sOscope _sOvarInImpException
              ( _pIbe,_pIcount,_pIdeep,_pIdiff,_pIex,_pIeye,_pIeyeEx,_pIeyeVar,_pIformula,_pIn1,_pIn2,_pItransformed) =
                  p_ _pOc _pOinsideQuotation _pOscope _pOvarInImpException
              ( _oIbe,_oIcount,_oIdeep,_oIdiff,_oIex,_oIeye,_oIeyeEx,_oIeyeVar,_oIformula,_oIn1,_oIn2,_oItransformed) =
                  o_ _oOc _oOinsideQuotation _oOscope _oOvarInImpException
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
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
              _lhsObe :: Int
              _e1Oscope :: ([String])
              _e2Oscope :: ([String])
              _e1Ibe :: Int
              _e1Icount :: Int
              _e1Ideep :: ([String])
              _e1Idiff :: CExpression
              _e1Ieye :: CExpression
              _e1IeyeEx :: ([String])
              _e1IeyeVar :: ([String])
              _e1Iformula :: String
              _e1In1 :: ([String])
              _e1Itransformed :: CExpression
              _e2Ibe :: Int
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
                  ({-# LINE 206 "./attGr.ag" #-}
                   []
                   {-# LINE 1236 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 207 "./attGr.ag" #-}
                   _e1In1 `union` _e2In1
                   {-# LINE 1241 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 208 "./attGr.ag" #-}
                   []
                   {-# LINE 1246 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 209 "./attGr.ag" #-}
                   CImplication _e1Itransformed _e2Itransformed
                   {-# LINE 1251 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 210 "./attGr.ag" #-}
                   _e1Iformula ++ " -> " ++ _e2Iformula
                   {-# LINE 1256 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 211 "./attGr.ag" #-}
                   CImplication _e1Ieye _e2Ieye
                   {-# LINE 1261 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 212 "./attGr.ag" #-}
                   _e1IeyeVar `union` _e2IeyeVar
                   {-# LINE 1266 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 213 "./attGr.ag" #-}
                   keepEx _lhsIinsideQuotation (_e1IeyeEx `union` _e2IeyeEx)
                   {-# LINE 1271 "attGr.hs" #-}
                   )
              _e1OinsideQuotation =
                  ({-# LINE 214 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1276 "attGr.hs" #-}
                   )
              _e2OinsideQuotation =
                  ({-# LINE 215 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1281 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 216 "./attGr.ag" #-}
                   CImplication _e1Idiff _e2Idiff
                   {-# LINE 1286 "attGr.hs" #-}
                   )
              _e1Oc =
                  ({-# LINE 217 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1291 "attGr.hs" #-}
                   )
              _e2Oc =
                  ({-# LINE 218 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1296 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 219 "./attGr.ag" #-}
                   max _e1Icount _e2Icount
                   {-# LINE 1301 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 220 "./attGr.ag" #-}
                   choose _e1Ideep _e2Ideep _e1Icount _e2Icount
                   {-# LINE 1306 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 221 "./attGr.ag" #-}
                   maximum [_e1Ibe, _e2Ibe]
                   {-# LINE 1311 "attGr.hs" #-}
                   )
              _e1Oscope =
                  ({-# LINE 145 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1316 "attGr.hs" #-}
                   )
              _e2Oscope =
                  ({-# LINE 145 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1321 "attGr.hs" #-}
                   )
              ( _e1Ibe,_e1Icount,_e1Ideep,_e1Idiff,_e1Ieye,_e1IeyeEx,_e1IeyeVar,_e1Iformula,_e1In1,_e1Itransformed) =
                  e1_ _e1Oc _e1OinsideQuotation _e1Oscope
              ( _e2Ibe,_e2Icount,_e2Ideep,_e2Idiff,_e2Ieye,_e2IeyeEx,_e2IeyeVar,_e2Iformula,_e2In1,_e2Itransformed) =
                  e2_ _e2Oc _e2OinsideQuotation _e2Oscope
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
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
              _lhsObe :: Int
              _c1Ouni :: ([String])
              _c2Ouni :: ([String])
              _c1Ibe :: Int
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
              _c2Ibe :: Int
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
                  ({-# LINE 223 "./attGr.ag" #-}
                   _c2In2 `union` _c1In2
                   {-# LINE 1383 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 224 "./attGr.ag" #-}
                   _c2In1 `union` _c1In1
                   {-# LINE 1388 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 225 "./attGr.ag" #-}
                   _c2Iex `union` _c1Iex
                   {-# LINE 1393 "attGr.hs" #-}
                   )
              _c1Oscope =
                  ({-# LINE 226 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1398 "attGr.hs" #-}
                   )
              _c2Oscope =
                  ({-# LINE 227 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1403 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 228 "./attGr.ag" #-}
                   CConjunction _c1Itransformed _c2Itransformed
                   {-# LINE 1408 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 229 "./attGr.ag" #-}
                   _c1Iformula ++ " ^ " ++ _c2Iformula
                   {-# LINE 1413 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 230 "./attGr.ag" #-}
                   CConjunction _c1Ieye _c2Ieye
                   {-# LINE 1418 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 231 "./attGr.ag" #-}
                   _c2IeyeVar `union` _c1IeyeVar
                   {-# LINE 1423 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 232 "./attGr.ag" #-}
                   _c2IeyeEx `union` _c1IeyeEx
                   {-# LINE 1428 "attGr.hs" #-}
                   )
              _c1OinsideQuotation =
                  ({-# LINE 233 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1433 "attGr.hs" #-}
                   )
              _c2OinsideQuotation =
                  ({-# LINE 234 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1438 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 235 "./attGr.ag" #-}
                   CConjunction _c1Idiff _c2Idiff
                   {-# LINE 1443 "attGr.hs" #-}
                   )
              _c1Oc =
                  ({-# LINE 236 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1448 "attGr.hs" #-}
                   )
              _c2Oc =
                  ({-# LINE 237 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1453 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 238 "./attGr.ag" #-}
                   max _c1Icount _c2Icount
                   {-# LINE 1458 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 239 "./attGr.ag" #-}
                   choose _c1Ideep _c2Ideep _c1Icount _c2Icount
                   {-# LINE 1463 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 240 "./attGr.ag" #-}
                   maximum [_c1Ibe, _c2Ibe]
                   {-# LINE 1468 "attGr.hs" #-}
                   )
              _c1Ouni =
                  ({-# LINE 99 "./attGr.ag" #-}
                   _lhsIuni
                   {-# LINE 1473 "attGr.hs" #-}
                   )
              _c2Ouni =
                  ({-# LINE 99 "./attGr.ag" #-}
                   _lhsIuni
                   {-# LINE 1478 "attGr.hs" #-}
                   )
              ( _c1Ibe,_c1Icount,_c1Ideep,_c1Idiff,_c1Iex,_c1Ieye,_c1IeyeEx,_c1IeyeVar,_c1Iformula,_c1In1,_c1In2,_c1Itransformed) =
                  c1_ _c1Oc _c1OinsideQuotation _c1Oscope _c1Ouni
              ( _c2Ibe,_c2Icount,_c2Ideep,_c2Idiff,_c2Iex,_c2Ieye,_c2IeyeEx,_c2IeyeVar,_c2Iformula,_c2In1,_c2In2,_c2Itransformed) =
                  c2_ _c2Oc _c2OinsideQuotation _c2Oscope _c2Ouni
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
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
type T_S = ( Int,Int,([String]),CFormula,([String]),CFormula,([String]),([String]),String,([String]),([String]),CFormula)
data Inh_S = Inh_S {}
data Syn_S = Syn_S {be_Syn_S :: Int,count_Syn_S :: Int,deep_Syn_S :: ([String]),diff_Syn_S :: CFormula,ex_Syn_S :: ([String]),eye_Syn_S :: CFormula,eyeEx_Syn_S :: ([String]),eyeVar_Syn_S :: ([String]),formula_Syn_S :: String,n1_Syn_S :: ([String]),n2_Syn_S :: ([String]),transformed_Syn_S :: CFormula}
wrap_S :: T_S ->
          Inh_S ->
          Syn_S
wrap_S sem (Inh_S) =
    (let ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed) = sem
     in  (Syn_S _lhsObe _lhsOcount _lhsOdeep _lhsOdiff _lhsOex _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOn2 _lhsOtransformed))
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
         _lhsObe :: Int
         _lhsOn1 :: ([String])
         _lhsOn2 :: ([String])
         _fIbe :: Int
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
             ({-# LINE 167 "./attGr.ag" #-}
              _fIn1  `union` _fIn2
              {-# LINE 1568 "attGr.hs" #-}
              )
         _fOuni =
             ({-# LINE 168 "./attGr.ag" #-}
              _fIn1  `union` _fIn2
              {-# LINE 1573 "attGr.hs" #-}
              )
         _lhsOex =
             ({-# LINE 169 "./attGr.ag" #-}
              _fIex
              {-# LINE 1578 "attGr.hs" #-}
              )
         _lhsOtransformed =
             ({-# LINE 170 "./attGr.ag" #-}
              qua (sort (_fIn1  `union` _fIn2)) _fIex _fItransformed
              {-# LINE 1583 "attGr.hs" #-}
              )
         _lhsOformula =
             ({-# LINE 171 "./attGr.ag" #-}
              quaf (sort  (_fIn1  `union` _fIn2)) _fIex   _fIformula
              {-# LINE 1588 "attGr.hs" #-}
              )
         _lhsOeye =
             ({-# LINE 172 "./attGr.ag" #-}
              qua (sort _fIeyeVar ) _fIeyeEx _fIeye
              {-# LINE 1593 "attGr.hs" #-}
              )
         _lhsOeyeEx =
             ({-# LINE 173 "./attGr.ag" #-}
              _fIeyeEx
              {-# LINE 1598 "attGr.hs" #-}
              )
         _lhsOeyeVar =
             ({-# LINE 174 "./attGr.ag" #-}
              _fIeyeVar
              {-# LINE 1603 "attGr.hs" #-}
              )
         _fOinsideQuotation =
             ({-# LINE 175 "./attGr.ag" #-}
              False
              {-# LINE 1608 "attGr.hs" #-}
              )
         _lhsOdiff =
             ({-# LINE 176 "./attGr.ag" #-}
              qua (sort  _fIeyeVar) _fIex _fIdiff
              {-# LINE 1613 "attGr.hs" #-}
              )
         _fOc =
             ({-# LINE 177 "./attGr.ag" #-}
              1
              {-# LINE 1618 "attGr.hs" #-}
              )
         _lhsOcount =
             ({-# LINE 178 "./attGr.ag" #-}
              findDepth _fIn1 1 _fIcount
              {-# LINE 1623 "attGr.hs" #-}
              )
         _lhsOdeep =
             ({-# LINE 179 "./attGr.ag" #-}
              deepest _fIn1 _fIdeep
              {-# LINE 1628 "attGr.hs" #-}
              )
         _lhsObe =
             ({-# LINE 180 "./attGr.ag" #-}
              _fIbe
              {-# LINE 1633 "attGr.hs" #-}
              )
         _lhsOn1 =
             ({-# LINE 75 "./attGr.ag" #-}
              _fIn1
              {-# LINE 1638 "attGr.hs" #-}
              )
         _lhsOn2 =
             ({-# LINE 76 "./attGr.ag" #-}
              _fIn2
              {-# LINE 1643 "attGr.hs" #-}
              )
         ( _fIbe,_fIcount,_fIdeep,_fIdiff,_fIex,_fIeye,_fIeyeEx,_fIeyeVar,_fIformula,_fIn1,_fIn2,_fItransformed) =
             f_ _fOc _fOinsideQuotation _fOscope _fOuni
     in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed))
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
              ( Int,Int,([String]),CTerm,([String]),CTerm,([String]),([String]),String,([String]),([String]),CTerm)
data Inh_Term = Inh_Term {c_Inh_Term :: Int,insideQuotation_Inh_Term :: Bool,scope_Inh_Term :: ([String]),varInImpException_Inh_Term :: Bool}
data Syn_Term = Syn_Term {be_Syn_Term :: Int,count_Syn_Term :: Int,deep_Syn_Term :: ([String]),diff_Syn_Term :: CTerm,ex_Syn_Term :: ([String]),eye_Syn_Term :: CTerm,eyeEx_Syn_Term :: ([String]),eyeVar_Syn_Term :: ([String]),formula_Syn_Term :: String,n1_Syn_Term :: ([String]),n2_Syn_Term :: ([String]),transformed_Syn_Term :: CTerm}
wrap_Term :: T_Term ->
             Inh_Term ->
             Syn_Term
wrap_Term sem (Inh_Term _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIvarInImpException) =
    (let ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed) = sem _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIvarInImpException
     in  (Syn_Term _lhsObe _lhsOcount _lhsOdeep _lhsOdiff _lhsOex _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOn2 _lhsOtransformed))
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
              _lhsObe :: Int
              _lhsOn2 =
                  ({-# LINE 243 "./attGr.ag" #-}
                   ["U"++ u_]
                   {-# LINE 1707 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 244 "./attGr.ag" #-}
                   []
                   {-# LINE 1712 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 245 "./attGr.ag" #-}
                   []
                   {-# LINE 1717 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 246 "./attGr.ag" #-}
                   Var  ("U"++ u_)
                   {-# LINE 1722 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 247 "./attGr.ag" #-}
                   "U"++ u_
                   {-# LINE 1727 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 248 "./attGr.ag" #-}
                   ["U" ++ u_]
                   {-# LINE 1732 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 249 "./attGr.ag" #-}
                   Var  ("U"++ u_)
                   {-# LINE 1737 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 250 "./attGr.ag" #-}
                   []
                   {-# LINE 1742 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 251 "./attGr.ag" #-}
                   Var  ("U"++ u_)
                   {-# LINE 1747 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 252 "./attGr.ag" #-}
                   0
                   {-# LINE 1752 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 253 "./attGr.ag" #-}
                   []
                   {-# LINE 1757 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 254 "./attGr.ag" #-}
                   0
                   {-# LINE 1762 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
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
              _lhsObe :: Int
              _lhsOn2 =
                  ({-# LINE 257 "./attGr.ag" #-}
                   []
                   {-# LINE 1787 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 258 "./attGr.ag" #-}
                   []
                   {-# LINE 1792 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 259 "./attGr.ag" #-}
                   ["E"++ ex_]
                   {-# LINE 1797 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 260 "./attGr.ag" #-}
                   Var ("E"++ ex_)
                   {-# LINE 1802 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 261 "./attGr.ag" #-}
                   "E"++ ex_
                   {-# LINE 1807 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 262 "./attGr.ag" #-}
                   []
                   {-# LINE 1812 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 263 "./attGr.ag" #-}
                   Var ("E"++ ex_)
                   {-# LINE 1817 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 264 "./attGr.ag" #-}
                   ["E"++ ex_]
                   {-# LINE 1822 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 265 "./attGr.ag" #-}
                   Var ("E"++ ex_)
                   {-# LINE 1827 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 266 "./attGr.ag" #-}
                   0
                   {-# LINE 1832 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 267 "./attGr.ag" #-}
                   []
                   {-# LINE 1837 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 268 "./attGr.ag" #-}
                   0
                   {-# LINE 1842 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
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
              _lhsObe :: Int
              _lhsOn2 =
                  ({-# LINE 270 "./attGr.ag" #-}
                   []
                   {-# LINE 1867 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 271 "./attGr.ag" #-}
                   []
                   {-# LINE 1872 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 272 "./attGr.ag" #-}
                   []
                   {-# LINE 1877 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 273 "./attGr.ag" #-}
                   Con ("uri_"++ uri_)
                   {-# LINE 1882 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 274 "./attGr.ag" #-}
                   "uri_"++ uri_
                   {-# LINE 1887 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 275 "./attGr.ag" #-}
                   []
                   {-# LINE 1892 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 276 "./attGr.ag" #-}
                   Con ("uri_" ++ uri_)
                   {-# LINE 1897 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 277 "./attGr.ag" #-}
                   []
                   {-# LINE 1902 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 278 "./attGr.ag" #-}
                   Con ("uri_" ++ uri_)
                   {-# LINE 1907 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 279 "./attGr.ag" #-}
                   0
                   {-# LINE 1912 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 280 "./attGr.ag" #-}
                   []
                   {-# LINE 1917 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 281 "./attGr.ag" #-}
                   isBuiltIn uri_
                   {-# LINE 1922 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
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
              _lhsObe :: Int
              _lhsOn2 =
                  ({-# LINE 283 "./attGr.ag" #-}
                   []
                   {-# LINE 1947 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 284 "./attGr.ag" #-}
                   []
                   {-# LINE 1952 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 285 "./attGr.ag" #-}
                   []
                   {-# LINE 1957 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 286 "./attGr.ag" #-}
                   Con ("l_"++ l_)
                   {-# LINE 1962 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 287 "./attGr.ag" #-}
                   "l_"++ l_
                   {-# LINE 1967 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 288 "./attGr.ag" #-}
                   []
                   {-# LINE 1972 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 289 "./attGr.ag" #-}
                   Con ("l_"++ l_)
                   {-# LINE 1977 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 290 "./attGr.ag" #-}
                   []
                   {-# LINE 1982 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 291 "./attGr.ag" #-}
                   Con ("l_"++ l_)
                   {-# LINE 1987 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 292 "./attGr.ag" #-}
                   0
                   {-# LINE 1992 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 293 "./attGr.ag" #-}
                   []
                   {-# LINE 1997 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 294 "./attGr.ag" #-}
                   0
                   {-# LINE 2002 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
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
              _lhsObe :: Int
              _lhsOeyeVar :: ([String])
              _eOc :: Int
              _eOscope :: ([String])
              _eIbe :: Int
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
                  ({-# LINE 296 "./attGr.ag" #-}
                   []
                   {-# LINE 2040 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 297 "./attGr.ag" #-}
                   _eIn1
                   {-# LINE 2045 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 298 "./attGr.ag" #-}
                   []
                   {-# LINE 2050 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 299 "./attGr.ag" #-}
                   CExp _eItransformed
                   {-# LINE 2055 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 300 "./attGr.ag" #-}
                   _eIformula
                   {-# LINE 2060 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 301 "./attGr.ag" #-}
                   CExp _eIeye
                   {-# LINE 2065 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 302 "./attGr.ag" #-}
                   exception _lhsIvarInImpException _lhsIinsideQuotation _eIeyeEx
                   {-# LINE 2070 "attGr.hs" #-}
                   )
              _eOinsideQuotation =
                  ({-# LINE 303 "./attGr.ag" #-}
                   True
                   {-# LINE 2075 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 304 "./attGr.ag" #-}
                   CExp _eIdiff
                   {-# LINE 2080 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 305 "./attGr.ag" #-}
                   _eIcount
                   {-# LINE 2085 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 306 "./attGr.ag" #-}
                   _eIdeep
                   {-# LINE 2090 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 307 "./attGr.ag" #-}
                   _eIbe
                   {-# LINE 2095 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 128 "./attGr.ag" #-}
                   _eIeyeVar
                   {-# LINE 2100 "attGr.hs" #-}
                   )
              _eOc =
                  ({-# LINE 157 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 2105 "attGr.hs" #-}
                   )
              _eOscope =
                  ({-# LINE 145 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 2110 "attGr.hs" #-}
                   )
              ( _eIbe,_eIcount,_eIdeep,_eIdiff,_eIeye,_eIeyeEx,_eIeyeVar,_eIformula,_eIn1,_eItransformed) =
                  e_ _eOc _eOinsideQuotation _eOscope
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))
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
              _lhsObe :: Int
              _termOinsideQuotation :: Bool
              _termOscope :: ([String])
              _termOvarInImpException :: Bool
              _listOinsideQuotation :: Bool
              _listOscope :: ([String])
              _listOvarInImpException :: Bool
              _termIbe :: Int
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
              _listIbe :: Int
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
                  ({-# LINE 310 "./attGr.ag" #-}
                   _termIn2 `union` _listIn2
                   {-# LINE 2170 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 311 "./attGr.ag" #-}
                   _termIn1 `union` _listIn1
                   {-# LINE 2175 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 312 "./attGr.ag" #-}
                   _termIex `union` _listIex
                   {-# LINE 2180 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 313 "./attGr.ag" #-}
                   CList _termItransformed _listItransformed
                   {-# LINE 2185 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 314 "./attGr.ag" #-}
                   "(" ++ _termIformula ++ "," ++ _listIformula ++ ")"
                   {-# LINE 2190 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 315 "./attGr.ag" #-}
                   _termIeyeVar ++ _listIeyeVar
                   {-# LINE 2195 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 316 "./attGr.ag" #-}
                   CList _termItransformed _listItransformed
                   {-# LINE 2200 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 317 "./attGr.ag" #-}
                   _termIeyeEx ++ _listIeyeEx
                   {-# LINE 2205 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 318 "./attGr.ag" #-}
                   CList _termIdiff _listIdiff
                   {-# LINE 2210 "attGr.hs" #-}
                   )
              _termOc =
                  ({-# LINE 319 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 2215 "attGr.hs" #-}
                   )
              _listOc =
                  ({-# LINE 320 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 2220 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 321 "./attGr.ag" #-}
                   max _termIcount _listIcount
                   {-# LINE 2225 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 322 "./attGr.ag" #-}
                   _termIdeep `union` _listIdeep
                   {-# LINE 2230 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 323 "./attGr.ag" #-}
                   0
                   {-# LINE 2235 "attGr.hs" #-}
                   )
              _termOinsideQuotation =
                  ({-# LINE 130 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 2240 "attGr.hs" #-}
                   )
              _termOscope =
                  ({-# LINE 122 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 2245 "attGr.hs" #-}
                   )
              _termOvarInImpException =
                  ({-# LINE 131 "./attGr.ag" #-}
                   _lhsIvarInImpException
                   {-# LINE 2250 "attGr.hs" #-}
                   )
              _listOinsideQuotation =
                  ({-# LINE 130 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 2255 "attGr.hs" #-}
                   )
              _listOscope =
                  ({-# LINE 122 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 2260 "attGr.hs" #-}
                   )
              _listOvarInImpException =
                  ({-# LINE 131 "./attGr.ag" #-}
                   _lhsIvarInImpException
                   {-# LINE 2265 "attGr.hs" #-}
                   )
              ( _termIbe,_termIcount,_termIdeep,_termIdiff,_termIex,_termIeye,_termIeyeEx,_termIeyeVar,_termIformula,_termIn1,_termIn2,_termItransformed) =
                  term_ _termOc _termOinsideQuotation _termOscope _termOvarInImpException
              ( _listIbe,_listIcount,_listIdeep,_listIdiff,_listIex,_listIeye,_listIeyeEx,_listIeyeVar,_listIformula,_listIn1,_listIn2,_listItransformed) =
                  list_ _listOc _listOinsideQuotation _listOscope _listOvarInImpException
          in  ( _lhsObe,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOtransformed)))