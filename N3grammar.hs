{-# LANGUAGE FlexibleContexts #-}

module N3grammar
(
mainparser
, parseN3
, parseFF
--, Formula
--, Term
)where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Data.Char
import Numeric
import Debug.Trace
import Data.Typeable

data S = Mainformula Formula deriving Show
data Term =  URI String 
           | Universal String 
           | Existential String 
           | Literal String 
           | Exp Expression 
           | Objectlist Term Term 
           | PredicateObjectList Term Term Term 
           | InversePredicateObjectList Term Term Term 
           | BlankConstruct Term Term Term Term
--TODO I did not find out how the attribute grammar can work on lists, if that is solved I would prefer to use haskell lists here
           | List Term Term
           deriving Show 
data Expression = FE Formula | BE Bool deriving Show
data Formula = Triple Term Term Term | Conjunction Formula Formula | Implication Expression Expression deriving Show 

--TODO ignore :a.

--TODO @keywords
--TODO separate input grammer from pure grammar then translate to core
--TODO I don't like the empty non-triples. maybe I can put that out again?


def = emptyDef{ commentLine = "#"
              , opStart = oneOf "_:;,.=><"
              , opLetter = oneOf "; ,.=><"
              , reservedOpNames = [";", "<=", ".", "=>"]
              , reservedNames = ["!", "^", "\"", "<", ">", "(", ")", "[", "]", ",", ";", "{}", "{", "}"]
              }

TokenParser{ identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved 
           , whiteSpace = m_space
           , lexeme = m_lex
           } = makeTokenParser def


formulaparser :: Parsec [Char] Int Formula
formulaparser = do {
                      f1 <- psimpleformula
                      ; skipMany $ char ';'
                      ; spaces
                      ;try(do {
                             m_reserved "."
                             ; f2 <- formulaparser
                             ; return (Conjunction f1 f2)
                          })
                      <|> do {
                         ; skipMany $ char ';'
                         ; spaces
                         ; return f1}
                     }


                    
psimpleformula =   try(do {
                        f <-simpleformula
                        ; return f
                        })
                 <|> try (do {
                        b <- blankconstruct
                        ; return $ triples $ fromBlank b
                        }
                        )  
                 <|> try (do {
                          b <- annoBlank
                          ; return $ triples $fromBlank b
                          })     
                 <|> try (do {
                        begin
                        ; m_space
                        ; f <- psimpleformula
                        ; return f       
                        })

                

simpleformula = try (do {
                     e1 <- exparser
                     ; m_space
                     ; do{
                               ; string "=>"
                               ; m_space
                               ; e2 <- objectparser
                               ; return (implications "=>" e1 e2)
                              }

                      <|>  do {
                               try( m_reserved "<=")
                               ; m_space
                               ; e2 <- objectparser
                               ; return (implications "<=" e1 e2 )
                               }
                      <|> do  {
                              p <- termparser
                              ;o <- objectparser
                              ; return (triples (Triple (Exp e1) p o)) 
                              }
                      <|> do {
                              string "."
                              ; f <- psimpleformula
                              ; return f
                              }
                     })
                    <|> try (do {
                      s <- pretermparser
                     ; try( do{
                         string "<="
                         ; spaces
                         ; o <- objectparser
                         ; return (triples (Triple o (URI "http://www.w3.org/2000/10/swap/log#implies") s))
                         })
                     <|> do{                      
                              ;p <- predicateparser
                              ;o <- objectparser
                              ; return (triples (Triple s p o)) 
                             }
                      <|> do{
                              ; spaces
                              ; string "is"
                              ; spaces
                              ;p <- termparser
                              ; spaces
                              ; string "of" 
                              ; spaces
                              ;o <- objectparser
                              ; return (triples (Triple o p s))
                             }
                     }) 

                

                
                

                     
                     

--TODO: simplify

objectparser = try (do{ o <- pretermparser
                    ; try (do{
                              ; m_reserved ","
                              ; o2 <- objectparser
                              ; skipMany $ (char ';'>> spaces)
                              ; return (Objectlist o o2) 
                              } )
                    <|> try (do{ 
                              char ';'
                              ; spaces
                              ; p2 <- predicateparser
                              ; o2 <- objectparser
                              ; skipMany $ (char ';' >> spaces)
                              ; return (PredicateObjectList o p2 o2) 
                            } )
                     <|> try (do{ 
                              char ';'
                              ; spaces
                              ; string "is"
                              ; spaces
                              ; p2 <- termparser
                              ; spaces
                              ; string "of"
                              ; spaces
                              ; o2 <- objectparser
                              ; skipMany $ (char ';'>> spaces)
                              ; return (InversePredicateObjectList o p2 o2) 
                             } )
                      <|> try (do{ 
                              skipMany $ (char ';' >>spaces)
                              ; return o })
                     })

implications :: String -> Expression -> Term -> Formula

implications "=>" e1 (Exp e2)   = (Implication e1 e2)
implications "=>" e1 (Objectlist (Exp e2) o2) = (Conjunction (Implication e1 e2) (implications "=>" e1 o2))
implications "=>" e1 (Objectlist e2 o2) = (Conjunction (Triple (Exp e1) (URI "http://www.w3.org/2000/10/swap/log#implies") e2) (implications "=>" e1 o2))
implications "=>" e1 (PredicateObjectList (Exp e2) p o2) = (Conjunction (Implication e1 e2) (triples (Triple (Exp e1) p o2 )))
implications "=>" e1 (PredicateObjectList e2 p o2) = (Conjunction (Triple (Exp e1) (URI "http://www.w3.org/2000/10/swap/log#implies") e2) (triples (Triple (Exp e1) p o2 )))
implications "=>" e1 e2   = (Triple (Exp e1) (URI "http://www.w3.org/2000/10/swap/log#implies")  e2)

implications "<=" e1 (Exp e2)    = (Implication e2 e1)
implications "<=" e1 (Objectlist (Exp e2) o2) = (Conjunction (Implication  e2 e1) (implications "<=" e1 o2))
implications "<=" e1 (Objectlist e2 o2) = (Conjunction (Triple  e2 (URI "http://www.w3.org/2000/10/swap/log#implies") (Exp e1)) (implications "<=" e1 o2))
implications "<=" e1 (PredicateObjectList (Exp e2) p o2) = (Conjunction (Implication e2 e1) (triples (Triple (Exp e1) p o2 )))
implications "<=" e1 (PredicateObjectList e2 p o2) = (Conjunction (Triple e2 (URI "http://www.w3.org/2000/10/swap/log#implies") (Exp e1)) (triples (Triple (Exp e1) p o2 )))
implications "<=" e1 e2   = (Triple e2 (URI "http://www.w3.org/2000/10/swap/log#implies")  (Exp e1))





pretermparser = try (do {
                        s <- blankconstruct
                        ; return s
                        }
                        )
                 <|>  do {
                      s <- termparser
                      ; return s
                      }  


blankconstruct = do {
                        s <- termparser
                        ; m_space
                        ;try( do{
                                string "!"
                               ; m_space
                               ; p <- pretermparser
                               ; m_space
                               ; l <- getState
                               ; updateState (+1)
                               ; return (BlankConstruct s p (Existential $".b_" ++  show(l)) (Existential $".b_" ++  show(l)) )
                                 }
                            )
                       <|> do {
                              string "^"
                               ; m_space
                               ; p <- pretermparser
                               ; m_space
                               ; l <- getState
                               ; updateState (+1)
                               ; return (BlankConstruct (Existential $".b_" ++  show(l)) p s  (Existential $".b_" ++  show(l)) )
                              }
                        }

     
--do I need the try here?                        
annoBlank     =  do {
                           string "["
                           ; m_space
                           ; try (do{
                                     ; string "is"
                                     ; spaces
                                     ; p <- pretermparser
                                     ; spaces
                                     ; string "of"
                                     ; m_space 
                                     ; s <- termparser
                                     ; spaces
                                     ; string "]"
                                     ; m_space
                                     ; l <- getState
                                     ; updateState (+1)
                                     ; return (BlankConstruct s p (Existential $".b_" ++  show(l)) (Existential $".b_" ++  show(l)) )
                                     })
                          <|> do{
                                     t <- predicateparser
                                     ; o <- objectparser
                                     ; string "]"
                                     ; m_space 
                                     ; l <- getState
                                     ; updateState (+1)
                                     ; return (BlankConstruct  (Existential $".b_" ++  show(l)) t o (Existential $".b_" ++  show(l)))
                                     }
                            }

                         

predicateparser = try( do {
                     t <- termparser
                     ; return t
                     }) 
                <|> do {
                   m_lex $ (char 'a'>> space )
                   ; return (URI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                   } 
               <|>  try(do {
                   m_reserved "=>" 
                   ; return (URI "http://www.w3.org/2000/10/swap/log#implies")
                   } )  
               <|> do {
                   m_reserved "=" 
                   ; return (URI "http://www.w3.org/2002/07/owl#sameAs")
                   }                     

                 
termparser = 
      try (do {
                   t <- termparser2
                   ; return t
                   })  
        <|> try (do{
               a <- annoBlank
               ; return a
               })

termparser2 = do {
                 t <- termparser3
                 ; return t
                 }
              <|> fmap Exp ( exparser )
       
               
termparser3 = fmap Existential (string "_:" >> blank_node)
       <|> fmap URI ( iriref) 
       <|> try (do {
              pr <- pname_ns
              ; pos <- option [] pn_local
              ; m_space
              ; return (URI (pr++pos))
              } )           
          

      <|> fmap Universal (char '?' >> (m_lex ( varname ) ))
 
--literals  
      <|> try ( do {
             s <-  sign
             ;n <- (floating3 True)
             ; m_space
             ; return (Literal (show(s n)))
             })    
      <|> try (do {
             n <- int
             ; m_space
             ; return (Literal (show( n)))
             })
      <|> try (do { 
                l <- litcontent
               ; m_space
               ; o <- option [] (langtag <|>  ( try(string "^^")>> (iriref  <|> urip)))
               ; m_space
               ; return (Literal $l++"_"++o)
             })
             
--      <|> fmap Exp ( exparser )
             

      <|> do {string "("
                ; m_space
                ; p <- termlist
                ; m_reserved ")"
                ; return ( p )
                } 
     <|> try (
              do {
              m_reserved "["
              ; m_reserved "]"
              ; l <- getState
              ; updateState (+1)
              ;return $ Existential $".b_" ++  show(l)
             })  
    
           
          
             
      
termlist = try ( do {
               m_space
              ; t <-pretermparser
              ; m_space
              ; l <-termlist
              ; return ( List t l )
              })
           <|>  do {
               return (Literal "\"\'emptylist\'\"")
              }


urip =  do {             
              pr <- pname_ns
              ; pos <- option [] pn_local
              ; m_space
              ; return $ pr ++ pos
             }
                          


exparser :: Parsec [Char] Int Expression
exparser = (m_lex $ string "false" >> return (BE False))
          <|> try (m_lex $ string "@false" >> return (BE False))
          <|> try (m_lex $ string "@true"  >> return (BE True))
          <|> (m_lex $ string "true"  >> return (BE True))
          <|> do {string "{"
                  ; m_space
                  ; try ( do{
                            f <- formulaparser
                            ; optional (m_reserved ".")
                            ; m_reserved "}"
                            ; return (FE f)
                         })
                    <|> do {  
                            option [] ignore
                            ; m_reserved "}"  
                            ; return (BE True )
                           }
                 }




              
ignore = do {
             termparser
             ; try( do{
                      ; m_reserved "."
                      ; ignore
                      ; return [] 
                      }) 
            <|> do {
                      ; optional $ m_reserved "."
                      ; return []
                   }
             }



mainparser :: Parsec [Char] Int S
mainparser = mparser <* eof 
            where mparser :: Parsec [Char] Int S
                  mparser = try (do {
                               ;f <- formulaparser
                               ;m_reserved "."
                               ; spaces
                               ; skipMany begin
                              -- ; skipMany $ dotConstr
                               ; return (Mainformula f)                             
                               })
 
{-  
parseN3 :: String -> IO ()
parseN3 inp = case parse mainparser "" inp of
              { Left err -> print err
              ; Right ans -> print  ans 
              }
-}

begin = try (do space ; return [] )
        <|> do {commentline}
         <|> try (do prefix)
         <|> try (do base)
         <|> try (do keywords)
         <|> try (do explicitQuantification)
         <|> (do termparser2; char '.'; return [])

commentline = do{    char '#'
                     ; line
                     ; newline
                     ; return []
                 }


-- currently just used to accept prefixes, later I also want to deal with them
prefix = do { string "@prefix"
              ; spaces
              ; a <- pname_ns
              ; spaces
              ; b <- iriref
              ; char '.'
              ; spaces 
              ; return $ a++b
            }
      <|> do {
              caseInsensitiveString "prefix"
              ; spaces
              ; a <- pname_ns
              ; spaces
              ; b <- iriref
              ; return $ a++b
              }  

base = do {
           string "@base"
           ; spaces
           ; a <- iriref
           ; char '.'
           ; spaces
           ; return a
           }
       <|> do {
           caseInsensitiveString "base"
           ; spaces
           ; b <- iriref
           ; return b
           }

keywords = do {
           string "@keywords"
           ; spaces
           ; a <- manyTill anyChar (try (string "."))
           ; return a
           }

--explicit quantification is just ignored. Handling needs to be added
explicitQuantification = do {
           string "@for"
           ; c <- (string "All") <|> (string "Some")
           ; spaces
           ; a <- variables
           ; char '.'
           ; spaces
           ; return a
           }

variables = do {
                v <- variable
                ; spaces
                ; optional commentline
                ; spaces
                ; do{
                    char ','
                    ; spaces
                    ; optional commentline
                    ; spaces
                    ; v2 <- variables
                    ; return []
                    }
                <|> do { return v
                       }
                }
           

variable   =  do {
           pr <- pname_ns
           ; pos <- option [] pn_local
           ; spaces
           ; return pr
           }
           <|> do {
           a <- iriref
           ; spaces
           ; return a
           }

parseN3 :: String -> Either ParseError S
parseN3 s = runParser mainparser 0 "parameter" s




parseFF p fname
                = do{ input <- readFile fname
                      ; case runParser p 0 fname input of 
                      {
                      Left err -> return $ show err
                      ; Right ans -> return $ show ans
                      }                      
                     }





--terminals
line = many $ noneOf "\n" --for comment lines
--for literals in quotes
litcontent = try( do {
                string "\"\"\""
                ; a <- manyTill anyChar (try (string "\"\"\""))
                ; return a
                } )
            <|> try (do {
                string "\'\'\'"
                ; a <- manyTill anyChar (try (string "\'\'\'"))
                ; return  a
                })

            <|> do {
                char '\"'
                ; a <- many $ (convertToString (noneOf "\"\\\n\r" <|> uchar)) <|> echar
                ; char '\"'
                ; return $ concat a
                }
            <|> do {
                char '\''
                ;a <- many $ (convertToString (noneOf "\'\\\n\r" <|> uchar)) <|> echar
                ;char '\''
                ; return $ concat a
                }

               
                
                

convertToString :: ParsecT s u m Char -> ParsecT s u m String
convertToString a = do { res <- a
                         ; return [res]
                         }


iriref :: Parsec [Char] Int String 
iriref = try (do {
            char '<'
            ; uri <- many $ ( (noneOf  (['\x00'..'\x20'] ++ "=<>\"{}|^`\\"))<|>  uchar)
            ; char '>'
            ; m_space
            ;return uri
            })
            
pname_ns :: Parsec [Char] Int String 
pname_ns = do {
              a <- option [] pn_prefix
              ; char ':'
              ; return (a++":")
              }

{-
pname_ln :: Parsec [Char] Int String
pname_ln = do {
              a <- pname_ns
              ; b <- pn_local
              ; return (a++b)
              }
-}


--remark: slightly different then in the original grammar, we won't keep the "_:"
blank_node = do {
                      a <- alphaNum <|> char '_'    -- oneOf $ pn_chars_u++['0' .. '9']
                      ; b <- option [] p_end
                      ; m_space
                      ; return (a:b)
                     }

langtag :: Parsec [Char] Int String
langtag = do {
             char '@'
             ; w <- many1 (oneOf $ ['a' .. 'z']++['A' .. 'Z'])
             ; en <- many lang2
             ; return $ "@" ++  w ++ (concat en)
             }
lang2 = 
           do{
           a <- char '-'
           ; b <- many1 (oneOf $ ['a' .. 'z']++['A' .. 'Z']++['0'..'9'])
           ; return $ a:b
           }
           




uchar :: Parsec [Char] Int Char
uchar = try ( do { 
                 string "\\u"
                 ; u <-  count 4 hexDigit
                 ; return (  chr $ fst $ head $ readHex u )
                 }
         
            )
           
       <|> try ( do { 
                 string "\\U"
                 ; u <- count 8 hexDigit
                 ; return (chr $ fst $ head $ readHex u) 
                 }
         
            )     

--CHAR 	::= 	'\' [tbnrf"'\]
echar :: Parsec [Char] Int String
echar = do {
           char '\\'
           ; z <- oneOf "tbnrf\"\'\\"
           ; return ['\\', z]
           }           
pn_prefix :: Parsec [Char] Int String
pn_prefix = try (do {
                    first <- letter --oneOf pn_chars_base
                    ; second <-  (option [] p_end)
                    ; return (first:second)
                    } 
                ) 

p_end :: Parsec [Char] Int String
p_end = 
        try (do { 
                 a <-  (alphaNum <|>  oneOf ".-_") --oneOf (pn_chars++".")
                 ; b <- p_end
                 ; return ([a]++b)
                 })
        <|> try (do {            
            ;b <- (alphaNum <|>  oneOf "-_") -- (oneOf pn_chars)
            ; return [b] 
           })
varname :: Parsec [Char] Int String
varname = 
        try (do { 
                 a <-  (alphaNum <|>  char '_') --oneOf (pn_chars++".")
                 ; b <- p_end
                 ; return ([a]++b)
                 })
        <|> try (do {            
            ;b <- (alphaNum <|>  char '_') -- (oneOf pn_chars)
            ; return [b] 
           })

pn_local :: Parsec [Char] Int String
pn_local = try (do {
              a <-  ( (convertToString (alphaNum <|> oneOf "_:")) <|> plx )   --(oneOf (pn_chars_u++['1' .. '9']++":"))) <|> plx ) 
              ; b <- option [] pl_end
              ; m_space
              ; return $ a++b
              })

pl_end :: Parsec [Char] Int String
pl_end = 
        try (do { 
                 a <- (convertToString (alphaNum <|> oneOf "-_.:") <|> plx )
                 ; b <- pl_end

                 ; return (a++b)
                 })
        <|> try (do {            
            ; b <- (convertToString (alphaNum <|> oneOf "-_:") <|> plx )
            ; return b 
           })


plx :: Parsec [Char] Int String 
plx = try ( do { 
                char '%' 
                ; a <- hexDigit 
                ; b <- hexDigit 
                ; return ['%', a, b]
                })
      <|> try (do { char '\\' 
                    ; a <- oneOf "_~.-!$&'()*+,;=/?#@%"
                   ; return ['\\',a]
                  })




caseInsensitiveChar c = char (toLower c) <|> char (toUpper c) 
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""




--TODO: this works but has to be cleaned up (you can for sure do it shorter)---

triples :: Formula -> Formula
triples (Triple s p o) =  ( \a b c -> (
                                       conjunctions (
                                                     convertTriple ((Triple (fst a) (fst b) (fst c))),
                                                     ((snd a)++(snd b)++(snd c)) 
                                                    )
                                      ) 
                          ) (treatList s []) (treatList p []) (treatList o [])
triples   f = f

treatList :: Term -> [Formula] -> (Term, [Formula])
treatList (List (BlankConstruct s a b e) l) f = (\x -> ((List e (fst x)), snd x))(treatList l ( (Triple s a b):f))
treatList (List (List nest rest) l) f = (\x y -> ( (List (fst x) (fst y)), (snd x)++(snd y)          ))(treatList (List nest rest)  []) (treatList l f)
treatList (List e l) f = (\x -> ((List e (fst x)), snd x))(treatList l f)
--treatList (BlankConstruct e a b) f = ( e , ((Triple e a b):f))
treatList t l = (t, l)




convertTriple :: Formula -> Formula

convertTriple (Triple (BlankConstruct c a b e) p o) = Conjunction  (triples (Triple c a b)) (triples(Triple e p o))
convertTriple (Triple s (BlankConstruct c a b e) o) = Conjunction  (triples (Triple c a b)) (triples (Triple s e o))
convertTriple (Triple s p (BlankConstruct c a b e)) = Conjunction  (triples (Triple c a b)) (triples (Triple s p e))

convertTriple (Triple (Objectlist s list) p o) = Conjunction (triples (Triple s p o)) (triples (Triple list p o))
convertTriple (Triple (PredicateObjectList s p2 o2) p o)= Conjunction (triples (Triple s p o)) (triples (Triple o p2 o2))
convertTriple (Triple (InversePredicateObjectList s p2 o2) p o)= Conjunction (triples (Triple s p o)) (triples (Triple o2 p2 o))
convertTriple (Triple s p (Objectlist o list)) = Conjunction (triples (Triple s p o)) (triples (Triple s p list))
convertTriple (Triple s p (PredicateObjectList o p2 o2))= Conjunction (triples (Triple s p o)) (triples (Triple s p2 o2))
convertTriple (Triple s p (InversePredicateObjectList o p2 o2))= Conjunction (triples (Triple s p o)) (triples (Triple o2 p2 s))

convertTriple (Triple s p o) = (Triple s p o)





conjunctions :: (Formula, [Formula]) -> Formula
conjunctions (f, []) = f
conjunctions (f,  (f1:l)) = conjunctions ((Conjunction f (triples f1)), l)


fromBlank :: Term -> Formula
fromBlank (BlankConstruct c a b e) = (Triple c a b)

{-
--string sets as specified in the official grammar
-- This is too expensive to use, I use letter instead, characters from \x02e5 on can be a problem
pn_chars_base = ['a' .. 'z']++['A' .. 'Z']++['\x00C0' .. '\x00D6']++ ['\x00D8' .. '\x00F6'] ++ ['\x00F8' .. '\x02FF'] ++ ['\x0370' .. '\x037D'] 
                        ++ ['\x037F' ..'\x1FFF'] ++ ['\x200C' .. '\x200D' ] ++ ['\x2070' .. '\x218F'] ++ ['\x2C00' .. '\x2FEF'] ++ ['\x3001' .. '\xD7FF'] ++ ['\xF900' .. '\xFDCF'] 
                        ++ ['\xFDF0' .. '\xFFFD'] ++ ['\x10000' .. '\xEFFFF']

pn_chars_u = '_':pn_chars_base 

pn_chars = pn_chars_u ++ "-"++['0'.. '9'] ++ "\x00B7"++['\x0300' .. '\x036F'] ++ ['\x203F' .. '\x2040']

-}

