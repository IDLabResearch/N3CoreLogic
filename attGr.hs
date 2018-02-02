

-- UUAGC 0.9.42.3 (attGr.ag)

{-# LINE 4 "./attGr.ag" #-}



import Data.List
import Text.Parsec.Prim
import N3grammar
import System.Directory
import Data.Text (pack, count)


{-# LINE 17 "attGr.hs" #-}
{-# LINE 434 "./attGr.ag" #-}





             
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

--not used any more
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
                ; let fs = (filter (isSuffixOf ".n3") files)
               -- ; let fname = head fs
               -- ;  input <- readFile (foname ++"/"++fname)
               -- ; case runParser mainparser 0 fname input of
               -- { Left err ->   writeFile "err.txt" (show err)
               -- ; Right ans ->  writeFile "out.txt" $ niceTable (read (show ans)) fname
                                
               -- }
               --this here is to only consider the files containing the symbol ?, only those can contain universals
               ;  newfs <- findFilesWithUniversals foname fs []
               --; builtins <- hasBuiltins foname newfs []
               ;   (values, count) <- (readAll foname newfs [] [0, 0, 0, 0, 0,0,0,0, 0, 0])
               ; let table = makeTable newfs values count
               ; writeFile target $ ((replicate 40 ' ') ++ "cwm/es2  " ++ "cwm/eye  " ++ "eye/es2  " ++ "builtin " ++ "proof  "++ "nested "++ "depth  " ++ "reason \n" )++ table
               }


--readAll :: String -> [String] ->  -> IO() 
readAll foname [] list [a, b, c, d, e, r1, r2, r3, ne, pc ] = do { 
                            return $ ((reverse list), [a,b,c,d, e, r1 , r2, r3, ne, pc])
                            }
readAll foname fs list [a, b, c, d, e, r1, r2, r3, ne, pc]  = do { 
                         let fname = (head fs)
                         ; input <- readFile $ foname ++ "/"++ fname
                         ; print (foname ++ "/"++ fname ++ " parsed \n" )  
                                    ;case runParser mainparser 0 fname input of
                                    {
                                    Left  err ->   (readAll foname (tail fs) ( [9,9,9,9,9,9,9,9, 9 ]:list ) [a, b, c, d, e, r1, r2, r3, ne, pc] )
                                    ;Right ans -> case ((  eyeVar_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )== []) of
                                                    {
                                                     True -> (readAll foname (tail fs) ( [9,9,9,9,9,9,9,9, 9]:list ) [a, b, c, d, e, r1, r2, r3, ne, pc] )
                                                     ; False ->
                                                         (\res -> ((\x y z zz zzzz proof zzz ff r  -> (readAll foname (tail fs) (
                                                             [x, 
                                                              y, 
                                                              z,
                                                              zz,
                                                              zzzz,
                                                              proof,
                                                              ff,
                                                              zzz ,
                                                              r
                                                             ]:list ) 
                                                             [(a+x), (b+y), (c+z), (d+1), (e+zzzz), (countR1 r1 r), (countR2 r2 r), ( countR3 r3 r), (countBI ne ff), pc + proof ]
                                                             ) )
                                                             (cwm_and_eye res )
                                                             (cwm_and_diff res)
                                                             (eye_and_diff res)
                                                             (boolToInt ( (eyeVar_Syn_S (  wrap_S  (sem_S  res) Inh_S   ))==[]))
                                                             (be_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  ))
                                                             (proof_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  ))
                                                             (  count_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )
                                                             (  cb_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )
                                                             )
                                                           --r
                                                           (reason (cwm_and_diff res) (  proof_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  )  (  count_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  ) (  bwfe_Syn_S (  wrap_S  (sem_S  (read (show ans))) Inh_S  )  ) ) 
                                                           )
                                                             (read (show ans))
                                      }
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
makeTable names ([a , b, c , 1, e1, e, f, r, p]:v) [s1, s2, s3, s4, s5, s6 , s7] = (makeTable (tail names) v [(s1-a), (s2-b), (s3-c), (s4-1),(s5-e1)])
makeTable names ([a , b, c , d, e1, e, f, r, p]:v) stat = (head names) ++ (replicate (40 - length(head names)) ' ') ++ (concat $ fmap makeEntry [a, b, c, e1,  e, f , r, p]) ++ "\n"
                         ++ (makeTable (tail names) v stat)


lastLine :: [Int]-> String
lastLine [a,b,c, d, e, r1, r2, r3, bi, pc] = "----------------------------------------------------------------------------------------------------" ++ "\n"
                      ++ "Absolute Number (from " 
                      ++ (show d) ++ "): " 
                      ++ (replicate(15 - (length(show d))) ' ')
                      ++ (show a) ++ (replicate (9 - (length(show a))) ' ')
                      ++ (show b) ++ (replicate (9 - (length(show b))) ' ')
                      ++ (show c) ++ (replicate (9 - (length(show c))) ' ')
                      ++ (show e) ++ (replicate (9 - (length(show e))) ' ')
                      ++ (show pc) ++ (replicate (9 - (length(show pc))) ' ')
                      ++ (show bi) ++ (replicate (9 - (length(show bi))) ' ')
                      ++ "\n"
                      ++ "Percentage: "
                      ++ (show ((fromIntegral a)/(fromIntegral d)))
                      ++" "
                      ++ (show $ (fromIntegral b)/(fromIntegral d))
                      ++ " "
                      ++ (show $ (fromIntegral c)/(fromIntegral d))
                      ++ " "
                      ++ (show $ (fromIntegral e)/(fromIntegral d))
                      ++ " "
                      ++ (show $ (fromIntegral pc)/(fromIntegral d))
                      ++ " "
                      ++ (show $ (fromIntegral bi)/(fromIntegral d))
                      ++ "\n" ++ "\n"
                      ++ "Distribution of Errors in EYE vs. CWM" ++ "\n"
                      ++ "Nesting:  " ++ (show r1) ++ " (absolut) "
                      ++ (show $ (fromIntegral r1)/(fromIntegral d)) ++ " (from all files) " ++ (show $ (fromIntegral r1)/(fromIntegral (d - b) )) ++ " (from differences) " ++ "\n"
                      ++ "Proof:  " ++ (show r2) ++ " (absolut) "
                      ++ (show $ (fromIntegral r2)/(fromIntegral d)) ++ " (from all files) " ++ (show $ (fromIntegral r2)/(fromIntegral (d - b) )) ++ " (from differences) " ++ "\n"
                      ++ "Builtins:  " ++ (show r3) ++ " (absolut) "
                      ++ (show $ (fromIntegral r3)/(fromIntegral d)) ++ " (from all files) " ++ (show $ (fromIntegral r3)/(fromIntegral (d - b) )) ++ " (from differences) " ++ "\n"
 
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


isProof :: String -> Int
isProof string
        |string == "r:Proof" = 1
        | otherwise = 0

isBuiltIn :: String -> Int
isBuiltIn string 
          | elem string builtinlist = 1
          | isPrefixOf "prolog:" string = 1
          | otherwise = 0

builtinlist = [ "e:avg",
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

reason :: Int -> Int -> Int -> Int -> Int
-- "no problem" = 0,  "nesting" = 1, "proof" = 2, "builtin" = 3
reason 1 n1 n2 n3 = 0
reason 0 n1 n2 n3
       | (n3 > 0)  = 3
       | (n1 == 1) && (n2 > 2) = 1
       | (n1 == 1) = 2
       | otherwise = 1
 


b :: Int -> [String] -> Int
b 0 [] = 0
b n  set = 1 


-- universalsInBuiltins @p.be @lhs.n2 ((@s.biscope `union` @p.biscope) `union` @o.biscope)
universalsInBuiltins :: Int -> [String] -> [String] -> [String]
universalsInBuiltins 0 s1 s2 = s2
universalsInBuiltins 1 s1 s2 = (s1 `union` s2) 

countR1 :: Int -> Int -> Int
countR1 r1 1 = r1 + 1
countR1 r1 r = r1

countR2 :: Int -> Int -> Int
countR2 r2 2 = r2 + 1
countR2 r2 r = r2

countR3 :: Int -> Int -> Int
countR3 r3 3 = r3 + 1
countR3 r3 r = r3

--this is to count nesting
countBI :: Int -> Int -> Int
countBI ne 1 = ne
countBI ne ff = ne +1
{-# LINE 707 "attGr.hs" #-}
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
                    ( Int,([String]),Int,Int,Int,([String]),CExpression,CExpression,([String]),([String]),String,([String]),Int,CExpression)
data Inh_Expression = Inh_Expression {c_Inh_Expression :: Int,insideQuotation_Inh_Expression :: Bool,scope_Inh_Expression :: ([String])}
data Syn_Expression = Syn_Expression {be_Syn_Expression :: Int,biscope_Syn_Expression :: ([String]),bwfe_Syn_Expression :: Int,cb_Syn_Expression :: Int,count_Syn_Expression :: Int,deep_Syn_Expression :: ([String]),diff_Syn_Expression :: CExpression,eye_Syn_Expression :: CExpression,eyeEx_Syn_Expression :: ([String]),eyeVar_Syn_Expression :: ([String]),formula_Syn_Expression :: String,n1_Syn_Expression :: ([String]),proof_Syn_Expression :: Int,transformed_Syn_Expression :: CExpression}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression sem (Inh_Expression _lhsIc _lhsIinsideQuotation _lhsIscope) =
    (let ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOproof,_lhsOtransformed) = sem _lhsIc _lhsIinsideQuotation _lhsIscope
     in  (Syn_Expression _lhsObe _lhsObiscope _lhsObwfe _lhsOcb _lhsOcount _lhsOdeep _lhsOdiff _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOproof _lhsOtransformed))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _lhsOn1 =
                  ({-# LINE 399 "./attGr.ag" #-}
                   []
                   {-# LINE 886 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 400 "./attGr.ag" #-}
                   CBE b_
                   {-# LINE 891 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 401 "./attGr.ag" #-}
                   if b_ == True then "<>" else "false"
                   {-# LINE 896 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 402 "./attGr.ag" #-}
                   []
                   {-# LINE 901 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 403 "./attGr.ag" #-}
                   CBE b_
                   {-# LINE 906 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 404 "./attGr.ag" #-}
                   []
                   {-# LINE 911 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 405 "./attGr.ag" #-}
                   CBE b_
                   {-# LINE 916 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 406 "./attGr.ag" #-}
                   0
                   {-# LINE 921 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 407 "./attGr.ag" #-}
                   []
                   {-# LINE 926 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 408 "./attGr.ag" #-}
                   0
                   {-# LINE 931 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 409 "./attGr.ag" #-}
                   0
                   {-# LINE 936 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 410 "./attGr.ag" #-}
                   0
                   {-# LINE 941 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 411 "./attGr.ag" #-}
                   0
                   {-# LINE 946 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 412 "./attGr.ag" #-}
                   []
                   {-# LINE 951 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOproof,_lhsOtransformed)))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _lhsOeyeVar :: ([String])
              _fIbe :: Int
              _fIbiscope :: ([String])
              _fIbwfe :: Int
              _fIcb :: Int
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
              _fIproof :: Int
              _fItransformed :: CFormula
              _lhsOn1 =
                  ({-# LINE 414 "./attGr.ag" #-}
                   _fIn2
                   {-# LINE 997 "attGr.hs" #-}
                   )
              _fOscope =
                  ({-# LINE 415 "./attGr.ag" #-}
                   _lhsIscope `union` _fIn1
                   {-# LINE 1002 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 416 "./attGr.ag" #-}
                   CFE (qua (_fIn1 \\ _lhsIscope) _fIex _fItransformed)
                   {-# LINE 1007 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 417 "./attGr.ag" #-}
                   "<"++ (quaf (_fIn1 \\ _lhsIscope) _fIex _fIformula) ++">"
                   {-# LINE 1012 "attGr.hs" #-}
                   )
              _fOuni =
                  ({-# LINE 418 "./attGr.ag" #-}
                   _fIn1 \\ _lhsIscope
                   {-# LINE 1017 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 419 "./attGr.ag" #-}
                   CFE (qua [] (imp _lhsIinsideQuotation _fIeyeEx) _fIeye )
                   {-# LINE 1022 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 420 "./attGr.ag" #-}
                   _fIeyeEx
                   {-# LINE 1027 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 421 "./attGr.ag" #-}
                   CFE (qua [] _fIex _fIdiff)
                   {-# LINE 1032 "attGr.hs" #-}
                   )
              _fOinsideQuotation =
                  ({-# LINE 422 "./attGr.ag" #-}
                   True
                   {-# LINE 1037 "attGr.hs" #-}
                   )
              _fOc =
                  ({-# LINE 423 "./attGr.ag" #-}
                   _lhsIc+1
                   {-# LINE 1042 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 424 "./attGr.ag" #-}
                   findDepth (_fIn1 \\ _lhsIscope) (_lhsIc +1) _fIcount
                   {-# LINE 1047 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 425 "./attGr.ag" #-}
                   deepest (_fIn1 \\ _lhsIscope) _fIdeep
                   {-# LINE 1052 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 426 "./attGr.ag" #-}
                   _fIbe
                   {-# LINE 1057 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 427 "./attGr.ag" #-}
                   _fIcb + 1
                   {-# LINE 1062 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 428 "./attGr.ag" #-}
                   0
                   {-# LINE 1067 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 429 "./attGr.ag" #-}
                   b _fIbwfe (_fIbiscope \\ _lhsIscope)
                   {-# LINE 1072 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 430 "./attGr.ag" #-}
                   []
                   {-# LINE 1077 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 174 "./attGr.ag" #-}
                   _fIeyeVar
                   {-# LINE 1082 "attGr.hs" #-}
                   )
              ( _fIbe,_fIbiscope,_fIbwfe,_fIcb,_fIcount,_fIdeep,_fIdiff,_fIex,_fIeye,_fIeyeEx,_fIeyeVar,_fIformula,_fIn1,_fIn2,_fIproof,_fItransformed) =
                  f_ _fOc _fOinsideQuotation _fOscope _fOuni
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOproof,_lhsOtransformed)))
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
                 ( Int,([String]),Int,Int,Int,([String]),CFormula,([String]),CFormula,([String]),([String]),String,([String]),([String]),Int,CFormula)
data Inh_Formula = Inh_Formula {c_Inh_Formula :: Int,insideQuotation_Inh_Formula :: Bool,scope_Inh_Formula :: ([String]),uni_Inh_Formula :: ([String])}
data Syn_Formula = Syn_Formula {be_Syn_Formula :: Int,biscope_Syn_Formula :: ([String]),bwfe_Syn_Formula :: Int,cb_Syn_Formula :: Int,count_Syn_Formula :: Int,deep_Syn_Formula :: ([String]),diff_Syn_Formula :: CFormula,ex_Syn_Formula :: ([String]),eye_Syn_Formula :: CFormula,eyeEx_Syn_Formula :: ([String]),eyeVar_Syn_Formula :: ([String]),formula_Syn_Formula :: String,n1_Syn_Formula :: ([String]),n2_Syn_Formula :: ([String]),proof_Syn_Formula :: Int,transformed_Syn_Formula :: CFormula}
wrap_Formula :: T_Formula ->
                Inh_Formula ->
                Syn_Formula
wrap_Formula sem (Inh_Formula _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIuni) =
    (let ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed) = sem _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIuni
     in  (Syn_Formula _lhsObe _lhsObiscope _lhsObwfe _lhsOcb _lhsOcount _lhsOdeep _lhsOdiff _lhsOex _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOn2 _lhsOproof _lhsOtransformed))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _sOscope :: ([String])
              _pOscope :: ([String])
              _oOscope :: ([String])
              _sIbe :: Int
              _sIbiscope :: ([String])
              _sIbwfe :: Int
              _sIcb :: Int
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
              _sIproof :: Int
              _sItransformed :: CTerm
              _pIbe :: Int
              _pIbiscope :: ([String])
              _pIbwfe :: Int
              _pIcb :: Int
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
              _pIproof :: Int
              _pItransformed :: CTerm
              _oIbe :: Int
              _oIbiscope :: ([String])
              _oIbwfe :: Int
              _oIcb :: Int
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
              _oIproof :: Int
              _oItransformed :: CTerm
              _lhsOn2 =
                  ({-# LINE 219 "./attGr.ag" #-}
                   (_sIn2 `union` _pIn2) `union` _oIn2
                   {-# LINE 1203 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 220 "./attGr.ag" #-}
                   (_sIn1 `union` _pIn1) `union` _oIn1
                   {-# LINE 1208 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 221 "./attGr.ag" #-}
                   (_sIex `union` _pIex) `union` _oIex
                   {-# LINE 1213 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 222 "./attGr.ag" #-}
                   CTriple _sItransformed _pItransformed _oItransformed
                   {-# LINE 1218 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 223 "./attGr.ag" #-}
                   _sIformula ++ " " ++ _pIformula ++ " " ++ _oIformula
                   {-# LINE 1223 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 224 "./attGr.ag" #-}
                   CTriple _sIeye _pIeye _oIeye
                   {-# LINE 1228 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 225 "./attGr.ag" #-}
                   (_sIeyeVar `union` _pIeyeVar) `union` _oIeyeVar
                   {-# LINE 1233 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 226 "./attGr.ag" #-}
                   (_sIeyeEx `union` _pIeyeEx) `union` _oIeyeEx
                   {-# LINE 1238 "attGr.hs" #-}
                   )
              _sOinsideQuotation =
                  ({-# LINE 227 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1243 "attGr.hs" #-}
                   )
              _pOinsideQuotation =
                  ({-# LINE 228 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1248 "attGr.hs" #-}
                   )
              _oOinsideQuotation =
                  ({-# LINE 229 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1253 "attGr.hs" #-}
                   )
              _sOvarInImpException =
                  ({-# LINE 230 "./attGr.ag" #-}
                   impExc _pIeye _lhsIinsideQuotation
                   {-# LINE 1258 "attGr.hs" #-}
                   )
              _pOvarInImpException =
                  ({-# LINE 231 "./attGr.ag" #-}
                   False
                   {-# LINE 1263 "attGr.hs" #-}
                   )
              _oOvarInImpException =
                  ({-# LINE 232 "./attGr.ag" #-}
                   impExc _pIeye _lhsIinsideQuotation
                   {-# LINE 1268 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 233 "./attGr.ag" #-}
                   CTriple _sIdiff _pIdiff _oIdiff
                   {-# LINE 1273 "attGr.hs" #-}
                   )
              _sOc =
                  ({-# LINE 234 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1278 "attGr.hs" #-}
                   )
              _pOc =
                  ({-# LINE 235 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1283 "attGr.hs" #-}
                   )
              _oOc =
                  ({-# LINE 236 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1288 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 237 "./attGr.ag" #-}
                   max _sIcount (max _pIcount _oIcount)
                   {-# LINE 1293 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 238 "./attGr.ag" #-}
                   choose _oIdeep  (choose _sIdeep _pIdeep _sIcount _pIcount) _oIcount (max _sIcount _pIcount)
                   {-# LINE 1298 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 239 "./attGr.ag" #-}
                   maximum [_sIbe, _pIbe, _oIbe]
                   {-# LINE 1303 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 240 "./attGr.ag" #-}
                   maximum [_sIcb, _pIcb, _oIcb]
                   {-# LINE 1308 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 241 "./attGr.ag" #-}
                   _oIproof
                   {-# LINE 1313 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 242 "./attGr.ag" #-}
                   maximum [ _pIbwfe, _sIbwfe, _oIbwfe]
                   {-# LINE 1318 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 243 "./attGr.ag" #-}
                   universalsInBuiltins _pIbe (_sIn1  `union` _oIn1) ((_sIbiscope `union` _pIbiscope) `union` _oIbiscope)
                   {-# LINE 1323 "attGr.hs" #-}
                   )
              _sOscope =
                  ({-# LINE 138 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1328 "attGr.hs" #-}
                   )
              _pOscope =
                  ({-# LINE 138 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1333 "attGr.hs" #-}
                   )
              _oOscope =
                  ({-# LINE 138 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1338 "attGr.hs" #-}
                   )
              ( _sIbe,_sIbiscope,_sIbwfe,_sIcb,_sIcount,_sIdeep,_sIdiff,_sIex,_sIeye,_sIeyeEx,_sIeyeVar,_sIformula,_sIn1,_sIn2,_sIproof,_sItransformed) =
                  s_ _sOc _sOinsideQuotation _sOscope _sOvarInImpException
              ( _pIbe,_pIbiscope,_pIbwfe,_pIcb,_pIcount,_pIdeep,_pIdiff,_pIex,_pIeye,_pIeyeEx,_pIeyeVar,_pIformula,_pIn1,_pIn2,_pIproof,_pItransformed) =
                  p_ _pOc _pOinsideQuotation _pOscope _pOvarInImpException
              ( _oIbe,_oIbiscope,_oIbwfe,_oIcb,_oIcount,_oIdeep,_oIdiff,_oIex,_oIeye,_oIeyeEx,_oIeyeVar,_oIformula,_oIn1,_oIn2,_oIproof,_oItransformed) =
                  o_ _oOc _oOinsideQuotation _oOscope _oOvarInImpException
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed)))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _e1Oscope :: ([String])
              _e2Oscope :: ([String])
              _e1Ibe :: Int
              _e1Ibiscope :: ([String])
              _e1Ibwfe :: Int
              _e1Icb :: Int
              _e1Icount :: Int
              _e1Ideep :: ([String])
              _e1Idiff :: CExpression
              _e1Ieye :: CExpression
              _e1IeyeEx :: ([String])
              _e1IeyeVar :: ([String])
              _e1Iformula :: String
              _e1In1 :: ([String])
              _e1Iproof :: Int
              _e1Itransformed :: CExpression
              _e2Ibe :: Int
              _e2Ibiscope :: ([String])
              _e2Ibwfe :: Int
              _e2Icb :: Int
              _e2Icount :: Int
              _e2Ideep :: ([String])
              _e2Idiff :: CExpression
              _e2Ieye :: CExpression
              _e2IeyeEx :: ([String])
              _e2IeyeVar :: ([String])
              _e2Iformula :: String
              _e2In1 :: ([String])
              _e2Iproof :: Int
              _e2Itransformed :: CExpression
              _lhsOn2 =
                  ({-# LINE 246 "./attGr.ag" #-}
                   []
                   {-# LINE 1408 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 247 "./attGr.ag" #-}
                   _e1In1 `union` _e2In1
                   {-# LINE 1413 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 248 "./attGr.ag" #-}
                   []
                   {-# LINE 1418 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 249 "./attGr.ag" #-}
                   CImplication _e1Itransformed _e2Itransformed
                   {-# LINE 1423 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 250 "./attGr.ag" #-}
                   _e1Iformula ++ " -> " ++ _e2Iformula
                   {-# LINE 1428 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 251 "./attGr.ag" #-}
                   CImplication _e1Ieye _e2Ieye
                   {-# LINE 1433 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 252 "./attGr.ag" #-}
                   _e1IeyeVar `union` _e2IeyeVar
                   {-# LINE 1438 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 253 "./attGr.ag" #-}
                   keepEx _lhsIinsideQuotation (_e1IeyeEx `union` _e2IeyeEx)
                   {-# LINE 1443 "attGr.hs" #-}
                   )
              _e1OinsideQuotation =
                  ({-# LINE 254 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1448 "attGr.hs" #-}
                   )
              _e2OinsideQuotation =
                  ({-# LINE 255 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1453 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 256 "./attGr.ag" #-}
                   CImplication _e1Idiff _e2Idiff
                   {-# LINE 1458 "attGr.hs" #-}
                   )
              _e1Oc =
                  ({-# LINE 257 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1463 "attGr.hs" #-}
                   )
              _e2Oc =
                  ({-# LINE 258 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1468 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 259 "./attGr.ag" #-}
                   max _e1Icount _e2Icount
                   {-# LINE 1473 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 260 "./attGr.ag" #-}
                   choose _e1Ideep _e2Ideep _e1Icount _e2Icount
                   {-# LINE 1478 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 261 "./attGr.ag" #-}
                   maximum [_e1Ibe, _e2Ibe]
                   {-# LINE 1483 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 262 "./attGr.ag" #-}
                   maximum [_e1Icb, _e2Icb]
                   {-# LINE 1488 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 263 "./attGr.ag" #-}
                   0
                   {-# LINE 1493 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 264 "./attGr.ag" #-}
                   maximum [_e1Ibwfe, _e2Ibwfe ]
                   {-# LINE 1498 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 265 "./attGr.ag" #-}
                   _e2Ibiscope `union` _e1Ibiscope
                   {-# LINE 1503 "attGr.hs" #-}
                   )
              _e1Oscope =
                  ({-# LINE 169 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1508 "attGr.hs" #-}
                   )
              _e2Oscope =
                  ({-# LINE 169 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1513 "attGr.hs" #-}
                   )
              ( _e1Ibe,_e1Ibiscope,_e1Ibwfe,_e1Icb,_e1Icount,_e1Ideep,_e1Idiff,_e1Ieye,_e1IeyeEx,_e1IeyeVar,_e1Iformula,_e1In1,_e1Iproof,_e1Itransformed) =
                  e1_ _e1Oc _e1OinsideQuotation _e1Oscope
              ( _e2Ibe,_e2Ibiscope,_e2Ibwfe,_e2Icb,_e2Icount,_e2Ideep,_e2Idiff,_e2Ieye,_e2IeyeEx,_e2IeyeVar,_e2Iformula,_e2In1,_e2Iproof,_e2Itransformed) =
                  e2_ _e2Oc _e2OinsideQuotation _e2Oscope
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed)))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _c1Ouni :: ([String])
              _c2Ouni :: ([String])
              _c1Ibe :: Int
              _c1Ibiscope :: ([String])
              _c1Ibwfe :: Int
              _c1Icb :: Int
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
              _c1Iproof :: Int
              _c1Itransformed :: CFormula
              _c2Ibe :: Int
              _c2Ibiscope :: ([String])
              _c2Ibwfe :: Int
              _c2Icb :: Int
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
              _c2Iproof :: Int
              _c2Itransformed :: CFormula
              _lhsOn2 =
                  ({-# LINE 267 "./attGr.ag" #-}
                   _c2In2 `union` _c1In2
                   {-# LINE 1587 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 268 "./attGr.ag" #-}
                   _c2In1 `union` _c1In1
                   {-# LINE 1592 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 269 "./attGr.ag" #-}
                   _c2Iex `union` _c1Iex
                   {-# LINE 1597 "attGr.hs" #-}
                   )
              _c1Oscope =
                  ({-# LINE 270 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1602 "attGr.hs" #-}
                   )
              _c2Oscope =
                  ({-# LINE 271 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 1607 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 272 "./attGr.ag" #-}
                   CConjunction _c1Itransformed _c2Itransformed
                   {-# LINE 1612 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 273 "./attGr.ag" #-}
                   _c1Iformula ++ " ^ " ++ _c2Iformula
                   {-# LINE 1617 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 274 "./attGr.ag" #-}
                   CConjunction _c1Ieye _c2Ieye
                   {-# LINE 1622 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 275 "./attGr.ag" #-}
                   _c2IeyeVar `union` _c1IeyeVar
                   {-# LINE 1627 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 276 "./attGr.ag" #-}
                   _c2IeyeEx `union` _c1IeyeEx
                   {-# LINE 1632 "attGr.hs" #-}
                   )
              _c1OinsideQuotation =
                  ({-# LINE 277 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1637 "attGr.hs" #-}
                   )
              _c2OinsideQuotation =
                  ({-# LINE 278 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 1642 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 279 "./attGr.ag" #-}
                   CConjunction _c1Idiff _c2Idiff
                   {-# LINE 1647 "attGr.hs" #-}
                   )
              _c1Oc =
                  ({-# LINE 280 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1652 "attGr.hs" #-}
                   )
              _c2Oc =
                  ({-# LINE 281 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 1657 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 282 "./attGr.ag" #-}
                   max _c1Icount _c2Icount
                   {-# LINE 1662 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 283 "./attGr.ag" #-}
                   choose _c1Ideep _c2Ideep _c1Icount _c2Icount
                   {-# LINE 1667 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 284 "./attGr.ag" #-}
                   maximum [_c1Ibe, _c2Ibe]
                   {-# LINE 1672 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 285 "./attGr.ag" #-}
                   maximum [_c1Icb, _c2Icb]
                   {-# LINE 1677 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 286 "./attGr.ag" #-}
                   maximum [_c1Iproof, _c2Iproof]
                   {-# LINE 1682 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 287 "./attGr.ag" #-}
                   maximum [_c1Ibwfe, _c2Ibwfe ]
                   {-# LINE 1687 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 288 "./attGr.ag" #-}
                   _c2Ibiscope `union` _c1Ibiscope
                   {-# LINE 1692 "attGr.hs" #-}
                   )
              _c1Ouni =
                  ({-# LINE 107 "./attGr.ag" #-}
                   _lhsIuni
                   {-# LINE 1697 "attGr.hs" #-}
                   )
              _c2Ouni =
                  ({-# LINE 107 "./attGr.ag" #-}
                   _lhsIuni
                   {-# LINE 1702 "attGr.hs" #-}
                   )
              ( _c1Ibe,_c1Ibiscope,_c1Ibwfe,_c1Icb,_c1Icount,_c1Ideep,_c1Idiff,_c1Iex,_c1Ieye,_c1IeyeEx,_c1IeyeVar,_c1Iformula,_c1In1,_c1In2,_c1Iproof,_c1Itransformed) =
                  c1_ _c1Oc _c1OinsideQuotation _c1Oscope _c1Ouni
              ( _c2Ibe,_c2Ibiscope,_c2Ibwfe,_c2Icb,_c2Icount,_c2Ideep,_c2Idiff,_c2Iex,_c2Ieye,_c2IeyeEx,_c2IeyeVar,_c2Iformula,_c2In1,_c2In2,_c2Iproof,_c2Itransformed) =
                  c2_ _c2Oc _c2OinsideQuotation _c2Oscope _c2Ouni
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed)))
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
type T_S = ( Int,([String]),Int,Int,Int,([String]),CFormula,([String]),CFormula,([String]),([String]),String,([String]),([String]),Int,CFormula)
data Inh_S = Inh_S {}
data Syn_S = Syn_S {be_Syn_S :: Int,biscope_Syn_S :: ([String]),bwfe_Syn_S :: Int,cb_Syn_S :: Int,count_Syn_S :: Int,deep_Syn_S :: ([String]),diff_Syn_S :: CFormula,ex_Syn_S :: ([String]),eye_Syn_S :: CFormula,eyeEx_Syn_S :: ([String]),eyeVar_Syn_S :: ([String]),formula_Syn_S :: String,n1_Syn_S :: ([String]),n2_Syn_S :: ([String]),proof_Syn_S :: Int,transformed_Syn_S :: CFormula}
wrap_S :: T_S ->
          Inh_S ->
          Syn_S
wrap_S sem (Inh_S) =
    (let ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed) = sem
     in  (Syn_S _lhsObe _lhsObiscope _lhsObwfe _lhsOcb _lhsOcount _lhsOdeep _lhsOdiff _lhsOex _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOn2 _lhsOproof _lhsOtransformed))
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
         _lhsOcb :: Int
         _lhsOproof :: Int
         _lhsObwfe :: Int
         _lhsObiscope :: ([String])
         _lhsOn1 :: ([String])
         _lhsOn2 :: ([String])
         _fIbe :: Int
         _fIbiscope :: ([String])
         _fIbwfe :: Int
         _fIcb :: Int
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
         _fIproof :: Int
         _fItransformed :: CFormula
         _fOscope =
             ({-# LINE 199 "./attGr.ag" #-}
              _fIn1  `union` _fIn2
              {-# LINE 1800 "attGr.hs" #-}
              )
         _fOuni =
             ({-# LINE 200 "./attGr.ag" #-}
              _fIn1  `union` _fIn2
              {-# LINE 1805 "attGr.hs" #-}
              )
         _lhsOex =
             ({-# LINE 201 "./attGr.ag" #-}
              _fIex
              {-# LINE 1810 "attGr.hs" #-}
              )
         _lhsOtransformed =
             ({-# LINE 202 "./attGr.ag" #-}
              qua (sort (_fIn1  `union` _fIn2)) _fIex _fItransformed
              {-# LINE 1815 "attGr.hs" #-}
              )
         _lhsOformula =
             ({-# LINE 203 "./attGr.ag" #-}
              quaf (sort  (_fIn1  `union` _fIn2)) _fIex   _fIformula
              {-# LINE 1820 "attGr.hs" #-}
              )
         _lhsOeye =
             ({-# LINE 204 "./attGr.ag" #-}
              qua (sort _fIeyeVar ) _fIeyeEx _fIeye
              {-# LINE 1825 "attGr.hs" #-}
              )
         _lhsOeyeEx =
             ({-# LINE 205 "./attGr.ag" #-}
              _fIeyeEx
              {-# LINE 1830 "attGr.hs" #-}
              )
         _lhsOeyeVar =
             ({-# LINE 206 "./attGr.ag" #-}
              _fIeyeVar
              {-# LINE 1835 "attGr.hs" #-}
              )
         _fOinsideQuotation =
             ({-# LINE 207 "./attGr.ag" #-}
              False
              {-# LINE 1840 "attGr.hs" #-}
              )
         _lhsOdiff =
             ({-# LINE 208 "./attGr.ag" #-}
              qua (sort  _fIeyeVar) _fIex _fIdiff
              {-# LINE 1845 "attGr.hs" #-}
              )
         _fOc =
             ({-# LINE 209 "./attGr.ag" #-}
              1
              {-# LINE 1850 "attGr.hs" #-}
              )
         _lhsOcount =
             ({-# LINE 210 "./attGr.ag" #-}
              findDepth _fIn1 1 _fIcount
              {-# LINE 1855 "attGr.hs" #-}
              )
         _lhsOdeep =
             ({-# LINE 211 "./attGr.ag" #-}
              deepest _fIn1 _fIdeep
              {-# LINE 1860 "attGr.hs" #-}
              )
         _lhsObe =
             ({-# LINE 212 "./attGr.ag" #-}
              _fIbe
              {-# LINE 1865 "attGr.hs" #-}
              )
         _lhsOcb =
             ({-# LINE 213 "./attGr.ag" #-}
              _fIcb
              {-# LINE 1870 "attGr.hs" #-}
              )
         _lhsOproof =
             ({-# LINE 214 "./attGr.ag" #-}
              _fIproof
              {-# LINE 1875 "attGr.hs" #-}
              )
         _lhsObwfe =
             ({-# LINE 215 "./attGr.ag" #-}
              _fIbwfe
              {-# LINE 1880 "attGr.hs" #-}
              )
         _lhsObiscope =
             ({-# LINE 216 "./attGr.ag" #-}
              []
              {-# LINE 1885 "attGr.hs" #-}
              )
         _lhsOn1 =
             ({-# LINE 76 "./attGr.ag" #-}
              _fIn1
              {-# LINE 1890 "attGr.hs" #-}
              )
         _lhsOn2 =
             ({-# LINE 77 "./attGr.ag" #-}
              _fIn2
              {-# LINE 1895 "attGr.hs" #-}
              )
         ( _fIbe,_fIbiscope,_fIbwfe,_fIcb,_fIcount,_fIdeep,_fIdiff,_fIex,_fIeye,_fIeyeEx,_fIeyeVar,_fIformula,_fIn1,_fIn2,_fIproof,_fItransformed) =
             f_ _fOc _fOinsideQuotation _fOscope _fOuni
     in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed))
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
              ( Int,([String]),Int,Int,Int,([String]),CTerm,([String]),CTerm,([String]),([String]),String,([String]),([String]),Int,CTerm)
data Inh_Term = Inh_Term {c_Inh_Term :: Int,insideQuotation_Inh_Term :: Bool,scope_Inh_Term :: ([String]),varInImpException_Inh_Term :: Bool}
data Syn_Term = Syn_Term {be_Syn_Term :: Int,biscope_Syn_Term :: ([String]),bwfe_Syn_Term :: Int,cb_Syn_Term :: Int,count_Syn_Term :: Int,deep_Syn_Term :: ([String]),diff_Syn_Term :: CTerm,ex_Syn_Term :: ([String]),eye_Syn_Term :: CTerm,eyeEx_Syn_Term :: ([String]),eyeVar_Syn_Term :: ([String]),formula_Syn_Term :: String,n1_Syn_Term :: ([String]),n2_Syn_Term :: ([String]),proof_Syn_Term :: Int,transformed_Syn_Term :: CTerm}
wrap_Term :: T_Term ->
             Inh_Term ->
             Syn_Term
wrap_Term sem (Inh_Term _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIvarInImpException) =
    (let ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed) = sem _lhsIc _lhsIinsideQuotation _lhsIscope _lhsIvarInImpException
     in  (Syn_Term _lhsObe _lhsObiscope _lhsObwfe _lhsOcb _lhsOcount _lhsOdeep _lhsOdiff _lhsOex _lhsOeye _lhsOeyeEx _lhsOeyeVar _lhsOformula _lhsOn1 _lhsOn2 _lhsOproof _lhsOtransformed))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _lhsOn2 =
                  ({-# LINE 291 "./attGr.ag" #-}
                   ["U"++ u_]
                   {-# LINE 1963 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 292 "./attGr.ag" #-}
                   []
                   {-# LINE 1968 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 293 "./attGr.ag" #-}
                   []
                   {-# LINE 1973 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 294 "./attGr.ag" #-}
                   Var  ("U"++ u_)
                   {-# LINE 1978 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 295 "./attGr.ag" #-}
                   "U"++ u_
                   {-# LINE 1983 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 296 "./attGr.ag" #-}
                   ["U" ++ u_]
                   {-# LINE 1988 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 297 "./attGr.ag" #-}
                   Var  ("U"++ u_)
                   {-# LINE 1993 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 298 "./attGr.ag" #-}
                   []
                   {-# LINE 1998 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 299 "./attGr.ag" #-}
                   Var  ("U"++ u_)
                   {-# LINE 2003 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 300 "./attGr.ag" #-}
                   0
                   {-# LINE 2008 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 301 "./attGr.ag" #-}
                   []
                   {-# LINE 2013 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 302 "./attGr.ag" #-}
                   0
                   {-# LINE 2018 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 303 "./attGr.ag" #-}
                   0
                   {-# LINE 2023 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 304 "./attGr.ag" #-}
                   0
                   {-# LINE 2028 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 305 "./attGr.ag" #-}
                   0
                   {-# LINE 2033 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 306 "./attGr.ag" #-}
                   []
                   {-# LINE 2038 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed)))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _lhsOn2 =
                  ({-# LINE 309 "./attGr.ag" #-}
                   []
                   {-# LINE 2067 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 310 "./attGr.ag" #-}
                   []
                   {-# LINE 2072 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 311 "./attGr.ag" #-}
                   ["E"++ ex_]
                   {-# LINE 2077 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 312 "./attGr.ag" #-}
                   Var ("E"++ ex_)
                   {-# LINE 2082 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 313 "./attGr.ag" #-}
                   "E"++ ex_
                   {-# LINE 2087 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 314 "./attGr.ag" #-}
                   []
                   {-# LINE 2092 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 315 "./attGr.ag" #-}
                   Var ("E"++ ex_)
                   {-# LINE 2097 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 316 "./attGr.ag" #-}
                   ["E"++ ex_]
                   {-# LINE 2102 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 317 "./attGr.ag" #-}
                   Var ("E"++ ex_)
                   {-# LINE 2107 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 318 "./attGr.ag" #-}
                   0
                   {-# LINE 2112 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 319 "./attGr.ag" #-}
                   []
                   {-# LINE 2117 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 320 "./attGr.ag" #-}
                   0
                   {-# LINE 2122 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 321 "./attGr.ag" #-}
                   0
                   {-# LINE 2127 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 322 "./attGr.ag" #-}
                   0
                   {-# LINE 2132 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 323 "./attGr.ag" #-}
                   0
                   {-# LINE 2137 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 324 "./attGr.ag" #-}
                   []
                   {-# LINE 2142 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed)))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _lhsOn2 =
                  ({-# LINE 326 "./attGr.ag" #-}
                   []
                   {-# LINE 2171 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 327 "./attGr.ag" #-}
                   []
                   {-# LINE 2176 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 328 "./attGr.ag" #-}
                   []
                   {-# LINE 2181 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 329 "./attGr.ag" #-}
                   Con ("uri_"++ uri_)
                   {-# LINE 2186 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 330 "./attGr.ag" #-}
                   "uri_"++ uri_
                   {-# LINE 2191 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 331 "./attGr.ag" #-}
                   []
                   {-# LINE 2196 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 332 "./attGr.ag" #-}
                   Con ("uri_" ++ uri_)
                   {-# LINE 2201 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 333 "./attGr.ag" #-}
                   []
                   {-# LINE 2206 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 334 "./attGr.ag" #-}
                   Con ("uri_" ++ uri_)
                   {-# LINE 2211 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 335 "./attGr.ag" #-}
                   0
                   {-# LINE 2216 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 336 "./attGr.ag" #-}
                   []
                   {-# LINE 2221 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 337 "./attGr.ag" #-}
                   isBuiltIn uri_
                   {-# LINE 2226 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 338 "./attGr.ag" #-}
                   0
                   {-# LINE 2231 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 339 "./attGr.ag" #-}
                   isProof uri_
                   {-# LINE 2236 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 340 "./attGr.ag" #-}
                   0
                   {-# LINE 2241 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 341 "./attGr.ag" #-}
                   []
                   {-# LINE 2246 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed)))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _lhsOn2 =
                  ({-# LINE 343 "./attGr.ag" #-}
                   []
                   {-# LINE 2275 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 344 "./attGr.ag" #-}
                   []
                   {-# LINE 2280 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 345 "./attGr.ag" #-}
                   []
                   {-# LINE 2285 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 346 "./attGr.ag" #-}
                   Con ("l_"++ l_)
                   {-# LINE 2290 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 347 "./attGr.ag" #-}
                   "l_"++ l_
                   {-# LINE 2295 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 348 "./attGr.ag" #-}
                   []
                   {-# LINE 2300 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 349 "./attGr.ag" #-}
                   Con ("l_"++ l_)
                   {-# LINE 2305 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 350 "./attGr.ag" #-}
                   []
                   {-# LINE 2310 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 351 "./attGr.ag" #-}
                   Con ("l_"++ l_)
                   {-# LINE 2315 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 352 "./attGr.ag" #-}
                   0
                   {-# LINE 2320 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 353 "./attGr.ag" #-}
                   []
                   {-# LINE 2325 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 354 "./attGr.ag" #-}
                   0
                   {-# LINE 2330 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 355 "./attGr.ag" #-}
                   0
                   {-# LINE 2335 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 356 "./attGr.ag" #-}
                   0
                   {-# LINE 2340 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 357 "./attGr.ag" #-}
                   0
                   {-# LINE 2345 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 358 "./attGr.ag" #-}
                   []
                   {-# LINE 2350 "attGr.hs" #-}
                   )
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed)))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _lhsOeyeVar :: ([String])
              _eOc :: Int
              _eOscope :: ([String])
              _eIbe :: Int
              _eIbiscope :: ([String])
              _eIbwfe :: Int
              _eIcb :: Int
              _eIcount :: Int
              _eIdeep :: ([String])
              _eIdiff :: CExpression
              _eIeye :: CExpression
              _eIeyeEx :: ([String])
              _eIeyeVar :: ([String])
              _eIformula :: String
              _eIn1 :: ([String])
              _eIproof :: Int
              _eItransformed :: CExpression
              _lhsOn2 =
                  ({-# LINE 360 "./attGr.ag" #-}
                   []
                   {-# LINE 2396 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 361 "./attGr.ag" #-}
                   _eIn1
                   {-# LINE 2401 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 362 "./attGr.ag" #-}
                   []
                   {-# LINE 2406 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 363 "./attGr.ag" #-}
                   CExp _eItransformed
                   {-# LINE 2411 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 364 "./attGr.ag" #-}
                   _eIformula
                   {-# LINE 2416 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 365 "./attGr.ag" #-}
                   CExp _eIeye
                   {-# LINE 2421 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 366 "./attGr.ag" #-}
                   exception _lhsIvarInImpException _lhsIinsideQuotation _eIeyeEx
                   {-# LINE 2426 "attGr.hs" #-}
                   )
              _eOinsideQuotation =
                  ({-# LINE 367 "./attGr.ag" #-}
                   True
                   {-# LINE 2431 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 368 "./attGr.ag" #-}
                   CExp _eIdiff
                   {-# LINE 2436 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 369 "./attGr.ag" #-}
                   _eIcount
                   {-# LINE 2441 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 370 "./attGr.ag" #-}
                   _eIdeep
                   {-# LINE 2446 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 371 "./attGr.ag" #-}
                   _eIbe
                   {-# LINE 2451 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 372 "./attGr.ag" #-}
                   _eIcb
                   {-# LINE 2456 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 373 "./attGr.ag" #-}
                   0
                   {-# LINE 2461 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 374 "./attGr.ag" #-}
                   _eIbwfe
                   {-# LINE 2466 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 375 "./attGr.ag" #-}
                   _eIbiscope
                   {-# LINE 2471 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 144 "./attGr.ag" #-}
                   _eIeyeVar
                   {-# LINE 2476 "attGr.hs" #-}
                   )
              _eOc =
                  ({-# LINE 181 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 2481 "attGr.hs" #-}
                   )
              _eOscope =
                  ({-# LINE 169 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 2486 "attGr.hs" #-}
                   )
              ( _eIbe,_eIbiscope,_eIbwfe,_eIcb,_eIcount,_eIdeep,_eIdiff,_eIeye,_eIeyeEx,_eIeyeVar,_eIformula,_eIn1,_eIproof,_eItransformed) =
                  e_ _eOc _eOinsideQuotation _eOscope
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed)))
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
              _lhsOcb :: Int
              _lhsOproof :: Int
              _lhsObwfe :: Int
              _lhsObiscope :: ([String])
              _termOinsideQuotation :: Bool
              _termOscope :: ([String])
              _termOvarInImpException :: Bool
              _listOinsideQuotation :: Bool
              _listOscope :: ([String])
              _listOvarInImpException :: Bool
              _termIbe :: Int
              _termIbiscope :: ([String])
              _termIbwfe :: Int
              _termIcb :: Int
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
              _termIproof :: Int
              _termItransformed :: CTerm
              _listIbe :: Int
              _listIbiscope :: ([String])
              _listIbwfe :: Int
              _listIcb :: Int
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
              _listIproof :: Int
              _listItransformed :: CTerm
              _lhsOn2 =
                  ({-# LINE 378 "./attGr.ag" #-}
                   _termIn2 `union` _listIn2
                   {-# LINE 2558 "attGr.hs" #-}
                   )
              _lhsOn1 =
                  ({-# LINE 379 "./attGr.ag" #-}
                   _termIn1 `union` _listIn1
                   {-# LINE 2563 "attGr.hs" #-}
                   )
              _lhsOex =
                  ({-# LINE 380 "./attGr.ag" #-}
                   _termIex `union` _listIex
                   {-# LINE 2568 "attGr.hs" #-}
                   )
              _lhsOtransformed =
                  ({-# LINE 381 "./attGr.ag" #-}
                   CList _termItransformed _listItransformed
                   {-# LINE 2573 "attGr.hs" #-}
                   )
              _lhsOformula =
                  ({-# LINE 382 "./attGr.ag" #-}
                   "(" ++ _termIformula ++ "," ++ _listIformula ++ ")"
                   {-# LINE 2578 "attGr.hs" #-}
                   )
              _lhsOeyeVar =
                  ({-# LINE 383 "./attGr.ag" #-}
                   _termIeyeVar ++ _listIeyeVar
                   {-# LINE 2583 "attGr.hs" #-}
                   )
              _lhsOeye =
                  ({-# LINE 384 "./attGr.ag" #-}
                   CList _termItransformed _listItransformed
                   {-# LINE 2588 "attGr.hs" #-}
                   )
              _lhsOeyeEx =
                  ({-# LINE 385 "./attGr.ag" #-}
                   _termIeyeEx ++ _listIeyeEx
                   {-# LINE 2593 "attGr.hs" #-}
                   )
              _lhsOdiff =
                  ({-# LINE 386 "./attGr.ag" #-}
                   CList _termIdiff _listIdiff
                   {-# LINE 2598 "attGr.hs" #-}
                   )
              _termOc =
                  ({-# LINE 387 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 2603 "attGr.hs" #-}
                   )
              _listOc =
                  ({-# LINE 388 "./attGr.ag" #-}
                   _lhsIc
                   {-# LINE 2608 "attGr.hs" #-}
                   )
              _lhsOcount =
                  ({-# LINE 389 "./attGr.ag" #-}
                   max _termIcount _listIcount
                   {-# LINE 2613 "attGr.hs" #-}
                   )
              _lhsOdeep =
                  ({-# LINE 390 "./attGr.ag" #-}
                   _termIdeep `union` _listIdeep
                   {-# LINE 2618 "attGr.hs" #-}
                   )
              _lhsObe =
                  ({-# LINE 391 "./attGr.ag" #-}
                   0
                   {-# LINE 2623 "attGr.hs" #-}
                   )
              _lhsOcb =
                  ({-# LINE 392 "./attGr.ag" #-}
                   maximum [_termIcb, _listIcb]
                   {-# LINE 2628 "attGr.hs" #-}
                   )
              _lhsOproof =
                  ({-# LINE 393 "./attGr.ag" #-}
                   0
                   {-# LINE 2633 "attGr.hs" #-}
                   )
              _lhsObwfe =
                  ({-# LINE 394 "./attGr.ag" #-}
                   maximum [_termIbwfe, _listIbwfe]
                   {-# LINE 2638 "attGr.hs" #-}
                   )
              _lhsObiscope =
                  ({-# LINE 395 "./attGr.ag" #-}
                   _termIbiscope `union` _listIbiscope
                   {-# LINE 2643 "attGr.hs" #-}
                   )
              _termOinsideQuotation =
                  ({-# LINE 146 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 2648 "attGr.hs" #-}
                   )
              _termOscope =
                  ({-# LINE 138 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 2653 "attGr.hs" #-}
                   )
              _termOvarInImpException =
                  ({-# LINE 147 "./attGr.ag" #-}
                   _lhsIvarInImpException
                   {-# LINE 2658 "attGr.hs" #-}
                   )
              _listOinsideQuotation =
                  ({-# LINE 146 "./attGr.ag" #-}
                   _lhsIinsideQuotation
                   {-# LINE 2663 "attGr.hs" #-}
                   )
              _listOscope =
                  ({-# LINE 138 "./attGr.ag" #-}
                   _lhsIscope
                   {-# LINE 2668 "attGr.hs" #-}
                   )
              _listOvarInImpException =
                  ({-# LINE 147 "./attGr.ag" #-}
                   _lhsIvarInImpException
                   {-# LINE 2673 "attGr.hs" #-}
                   )
              ( _termIbe,_termIbiscope,_termIbwfe,_termIcb,_termIcount,_termIdeep,_termIdiff,_termIex,_termIeye,_termIeyeEx,_termIeyeVar,_termIformula,_termIn1,_termIn2,_termIproof,_termItransformed) =
                  term_ _termOc _termOinsideQuotation _termOscope _termOvarInImpException
              ( _listIbe,_listIbiscope,_listIbwfe,_listIcb,_listIcount,_listIdeep,_listIdiff,_listIex,_listIeye,_listIeyeEx,_listIeyeVar,_listIformula,_listIn1,_listIn2,_listIproof,_listItransformed) =
                  list_ _listOc _listOinsideQuotation _listOscope _listOvarInImpException
          in  ( _lhsObe,_lhsObiscope,_lhsObwfe,_lhsOcb,_lhsOcount,_lhsOdeep,_lhsOdiff,_lhsOex,_lhsOeye,_lhsOeyeEx,_lhsOeyeVar,_lhsOformula,_lhsOn1,_lhsOn2,_lhsOproof,_lhsOtransformed)))