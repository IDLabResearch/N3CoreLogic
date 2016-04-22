module N3grammar
(
mainparser
, parseN3
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


data S = Mainformula Formula deriving Show
data Term =  URI String 
           | Universal String 
           | Existential String 
           | Literal String 
           | Exp Expression 
           | Objectlist Term Term 
           | PredicateObjectList Term Term Term 
           | BlankConstruct Term Term Term 
--TODO I did not find out how the attribute grammar can work on lists, if that is solved I would prefer to use haskell lists here
           | List Term Term
           | EmptyList
           deriving Show 
data Expression = FE Formula | BE Bool deriving Show
data Formula = Triple Term Term Term | Conjunction Formula Formula | Implication Expression Expression deriving Show 

--TODO ignore :a.


--TODO separate input grammer from pure grammar then translate to core
--TODO I don't like the empty non-triples. maybe I can put that out again?
--TODO path-notation ^ and !

def = emptyDef{ commentLine = "#"
              , opStart = oneOf "_:;,.=><"
              , opLetter = oneOf "; ,.=><"
              , reservedOpNames = [";", "<=", ".", "=>"]
              , reservedNames = ["\"", "<", ">", "(", ")", "[", "]", ",", ";", "{}", "{", "}"]
              }

TokenParser{ identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved 
           , whiteSpace = m_space
           , lexeme = m_lex
           } = makeTokenParser def


formulaparser :: Parsec [Char] Int Formula
formulaparser = try (do {
                     -- skipMany $ dotConstr
                      f1 <- simpleformula
                      ; skipMany $ m_reserved ";" 
                      ; m_reserved "."
                     -- ; skipMany $ try (dotConstr)
                      ; f2 <- formulaparser
                      ; return (Conjunction f1 f2)
                     })
                   <|>  do {
                        -- skipMany $ dotConstr
                         f <- simpleformula
                         ; skipMany $ m_reserved ";"
                      --   ; skipMany $ try (dotConstr)
                         ; return f}
                    
                

simpleformula =  try (do {
                      s <- termparser
                     ;p <- termparser
                     ;o <- objectparser
                     ; return (triples (Triple s p o))
                     })
                <|> try(do {
                     e1 <- exparser
                     ; m_reserved "=>"
                     ; e2 <- exparser
                     ; return (Implication e1 e2)})
                <|>  try (do {
                     e1 <- exparser
                     ; m_reserved "<="
                     ; e2 <- exparser
                     ; return (Implication e2 e1 )   
                     })
                --this case occurs but I am still not sure how to handle it
                   <|> try(do {
                     t1 <- termparser
                     ; m_reserved "=>"
                     ; t2 <- termparser
                     ; return (triples (Triple t1 (URI "http://www.w3.org/2000/10/swap/log#implies") t2))})
                
                
                
                     
dotConstr =   try (
                     do {
                     m_reserved "."
                     ; e <- termparser
                     ; m_space
                     ;return []
                     }
                     )
                     
                     



objectparser = try (do{ o <-termparser
                  ; m_reserved ","
                  ; o2 <- objectparser
                   ; skipMany $ m_reserved ";"
                  ; return (Objectlist o o2) } )
              <|> try (do{ o <- termparser
                  ; m_reserved ";"
                  ; p2 <- termparser
                  ; o2 <- objectparser
                  ; skipMany $ m_reserved ";"
                  ; return (PredicateObjectList o p2 o2) } )
              <|> try (do{ o <- termparser
                    ; skipMany $ m_reserved ";"
                     ; return o })
                    

                 
termparser = fmap Existential (string "_:" >> blank_node)
       <|> try (do {
                   m_lex $ (char 'a'>> space )
                   ; return (URI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                   })    
       <|> try (do {
                   m_reserved "=" 
                   ; return (URI "http://www.w3.org/2002/07/owl#sameAs")
                   })         


       <|> try (fmap URI ( iriref)) 
       <|> try (do {
              pr <- pname_ns
              ; pos <- option [] pn_local
              ; m_space
              ; return (URI (pr++pos))
              } )           
          

      <|> fmap Universal (char '?' >> (m_lex ( pn_prefix ) ))
 
--literals      
      <|> try (do {
             n <- int
             ; m_space
             ; return (Literal (show( n)))
             })
      <|> try ( do {
             s <-  sign
             ;n <- (floating3 True)
             ; m_space
             ; return (Literal (show(s n)))
             })
      <|> try (do { 
                l <- litcontent
               ; m_space
               ; o <- option [] (langtag <|>  ( string "^^">> iriref))
               ; m_space
               ; return (Literal $l++"_"++o)
             })

      


             
      <|> fmap Exp ( exparser )
      <|> try (
              do {
              m_reserved "["
              ; m_reserved "]"
              ; l <- getState
              ; updateState (+1)
              ;return $ Existential $".b_" ++  show(l)
             })
      <|> do { string "["
               ; m_space
               ; t <- termparser
               ; o <- objectparser
               ; string "]"
               ; m_space 
               ; l <- getState
               ; updateState (+1)
               ; return (BlankConstruct  (Existential $".b_" ++  show(l)) t o)
          }
      <|> do {string "("
               ; m_space
               ; p <- termlist
               ; m_reserved ")"
               ; return ( p )
             } 
      
             
             
             
      
termlist =  do {
               m_space
              ; t <-termparser
              ; m_space
              ; l <-termlist
              ; return ( List t l )
              }

          <|> do {
              return (EmptyList)
              }


           


exparser :: Parsec [Char] Int Expression
exparser = (m_lex $ string "false" >> return (BE False))
          <|> (m_lex $ string "true"  >> return (BE True))
          <|> try (m_reserved "{" >> m_reserved "}"  >> return (BE True ))
          <|> try (do {string "{"
                  ; m_space
                  ;f <- formulaparser
                  ; optional (m_reserved ".")
                  ; m_reserved "}"
                  ; return (FE f)
                  })
              


mainparser :: Parsec [Char] Int S
mainparser = mparser <* eof 
            where mparser :: Parsec [Char] Int S
                  mparser = try (do {
                               skipMany begin
                               ;f <- formulaparser
                               ;m_reserved "."
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
        <|> try ( do {
                     char '#'
                     ; line
                     ; newline
                     ; return []
                     })
        <|> try (do prefix)
         <|> try (do base)
         <|> try (do termparser; char '.'; return [])



-- currently just used to accept prefixes, later I also want to deal with them
prefix = try (do { string "@prefix"
              ; spaces
              ; a <- pname_ns
              ; spaces
              ; b <- iriref
              ; char '.'
              ; spaces 
              ; return $ a++b
            })
      <|> try(do {
              caseInsensitiveString "prefix"
              ; spaces
              ; a <- pname_ns
              ; spaces
              ; b <- iriref
              ; return $ a++b
              })   

base = try (do {
           string "@base"
           ; spaces
           ; a <- iriref
           ; char '.'
           ; spaces
           ; return a
           })
       <|> try (do {
           caseInsensitiveString "base"
           ; spaces
           ; b <- iriref
           ; return b
           })




parseN3 :: String -> Either ParseError S
parseN3 s = runParser mainparser 0 "parameter" s








parseFF p fname
                = do{ input <- readFile fname
                      ; return (runParser p 0 fname input)
                     }





--terminals
line = many $ noneOf "\n" --for comment lines
--for literals
litcontent = try( do {
                string "\"\"\""
                ;optional $ char '\"'
                ; optional $ char '\"'
                ;a <- many $ (convertToString (noneOf "\"\\" <|> uchar)) <|> echar
                ; string "\"\"\""
                ; return $ concat a
                } )
            <|> try (do {
                string "\'\'\'"
                ;optional $ char '\''
                ; optional $ char '\''
                ;a <- many $ (convertToString (noneOf "\'\\" <|> uchar)) <|> echar
                ; string "\'\'\'"
                ; return $ concat a
                })

            <|> try( do {
                char '\"'
                ; a <- many $ (convertToString (noneOf "\"\\\n\r" <|> uchar)) <|> echar
                ; char '\"'
                ; return $ concat a
                })
            <|> try( do {
                char '\''
                ;a <- many $ (convertToString (noneOf "\'\\\n\r" <|> uchar)) <|> echar
                ;char '\''
                ; return $ concat a
                })

               
                
                

convertToString :: ParsecT s u m Char -> ParsecT s u m String
convertToString a = do { res <- a
                         ; return [res]
                         }


iriref :: Parsec [Char] Int String 
iriref = try (do {
            char '<'
            ; uri <- many $ ( (noneOf  (['\x00'..'\x20'] ++ "<>\"{}|^`\\"))<|>  uchar)
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
lang2 = try(
           do{
           a <- char '-'
           ; b <- many1 (oneOf $ ['a' .. 'z']++['A' .. 'Z']++['0'..'9'])
           ; return $ a:b
           }
           )




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
treatList (List (BlankConstruct e a b) l) f = (\x -> ((List e (fst x)), snd x))(treatList l ((Triple e a b):f))
treatList (List e l) f = (\x -> ((List e (fst x)), snd x))(treatList l f)
treatList t l = (t, l)




convertTriple :: Formula -> Formula

convertTriple (Triple (BlankConstruct e a b) p o) = Conjunction  (convertTriple (Triple e a b)) (convertTriple(Triple e p o))
convertTriple (Triple s (BlankConstruct e a b) o) = Conjunction  (convertTriple (Triple e a b)) (convertTriple(Triple s e o))
convertTriple (Triple s p (BlankConstruct e a b)) = Conjunction  (convertTriple (Triple e a b)) (convertTriple(Triple s p e))

convertTriple (Triple s p (Objectlist o list)) = Conjunction (convertTriple (Triple s p o)) (convertTriple (Triple s p list))
convertTriple (Triple s p (PredicateObjectList o p2 o2))= Conjunction (convertTriple (Triple s p o)) (convertTriple (Triple s p2 o2))

convertTriple (Triple s p o) = (Triple s p o)





conjunctions :: (Formula, [Formula]) -> Formula
conjunctions (f, []) = f
conjunctions (f,  (f1:l)) = conjunctions ((Conjunction f (convertTriple f1)), l)




{-
--string sets as specified in the official grammar
-- This is too expensive to use, I use letter instead, characters from \x02e5 on can be a problem
pn_chars_base = ['a' .. 'z']++['A' .. 'Z']++['\x00C0' .. '\x00D6']++ ['\x00D8' .. '\x00F6'] ++ ['\x00F8' .. '\x02FF'] ++ ['\x0370' .. '\x037D'] 
                        ++ ['\x037F' ..'\x1FFF'] ++ ['\x200C' .. '\x200D' ] ++ ['\x2070' .. '\x218F'] ++ ['\x2C00' .. '\x2FEF'] ++ ['\x3001' .. '\xD7FF'] ++ ['\xF900' .. '\xFDCF'] 
                        ++ ['\xFDF0' .. '\xFFFD'] ++ ['\x10000' .. '\xEFFFF']

pn_chars_u = '_':pn_chars_base 

pn_chars = pn_chars_u ++ "-"++['0'.. '9'] ++ "\x00B7"++['\x0300' .. '\x036F'] ++ ['\x203F' .. '\x2040']

-}

