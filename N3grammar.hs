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

--TODO separate input grammer from pure grammar then translate to core

def = emptyDef{ commentLine = "#"
              , opStart = oneOf "; ,.=><"
              , opLetter = oneOf "; ,.=><"
              , reservedOpNames = [";", "<=", ".", "=>"]
              , reservedNames = ["\"", "<", ">", "(", ")", "[", "]", ",", ";", "{}", "{", "}", "false", "newBlank"]
              }

TokenParser{ identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved 
           } = makeTokenParser def


formulaparser :: Parsec [Char] Int Formula
formulaparser = try (do {
                      f1 <- simpleformula
                      ; skipMany $ m_reserved ";"
                      ; m_reserved "."
                      ;f2 <- formulaparser
                      ; return (Conjunction f1 f2)
                     })
                   <|>  do {f <- simpleformula
                         ; skipMany $ m_reserved ";"
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
                <|> try (do {
                     e1 <- exparser
                     ; m_reserved "<="
                     ; e2 <- exparser
                     ; return (Implication e2 e1 )   
                     })



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
                    

                 
termparser = fmap Existential (char '_' >> char ':' >> blank_node)
       <|> try (do {
                   char 'a'
                   ; space
                   ; return (URI "rdf_type")
                   })           
       <|>     try (do { 
                     pre <- m_identifier
                     ; char ':'
                     ; name <- uristring
                     ; spaces
                     ; return (URI (pre++"_"++name))
                     }
                 )

      <|> fmap Universal (char '?' >> m_identifier)
--URIs
       <|> try (fmap URI ( iriref)) 
       <|> try (do {
              pr <- pname_ns
              ; pos <- pn_local
              ; return (URI (pr++":"++pos))
              } )   
--literals      
      <|> try (do {
             n <- int
             ; spaces
             ; return (Literal (show( n)))
             })
      <|> try ( do {
             s <-  sign
             ;n <- (floating3 True)
             ; spaces
             ; return (Literal (show(s n)))
             })
      <|> try (do { 
                l <- litcontent
               ; o <- option [] (langtag <|>  (spaces >> string "^^">> iriref))
               ; spaces
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
      <|> do { m_reserved "["
               ; t <- termparser
               ; o <- objectparser
               ; m_reserved "]" 
               ; l <- getState
               ; updateState (+1)
               ; return (BlankConstruct  (Existential $".b_" ++  show(l)) t o)
          }
      <|> do {m_reserved "("
               ; p <- termlist
               ; m_reserved ")"
               ; return ( p )
             } 
      
             
             
             
      
termlist =  do {
               spaces
              ; t <-termparser
              ; spaces
              ; l <-termlist
              ; return ( List t l )
              }

          <|> do {
              return (EmptyList)
              }


           


exparser :: Parsec [Char] Int Expression
exparser = (m_reserved "false" >> return (BE False))
          <|> try (string "true" >> spaces >> return (BE True))
          <|> try(m_reserved "{" >> m_reserved "}"  >> return (BE True ))
          <|> try (do {m_reserved "{"
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
                               ; return (Mainformula f)                             
                               })
 
{-  
parseN3 :: String -> IO ()
parseN3 inp = case parse mainparser "" inp of
              { Left err -> print err
              ; Right ans -> print  ans 
              }
-}

begin = try (do space )
        <|> try ( do {
                     char '#'
                     ; line
                     ; newline
                     })
        <|> try (do prefix)
         <|> try (do base)


-- currently just used to accept prefixes, later I also want to deal with them
prefix = try (do { string "@prefix"
              ; spaces
              ; pname_ns
              ; iriref
              ; char '.'
              ; spaces 
              ; return 'l'
            })
      <|> try(do {
              caseInsensitiveString "prefix"
              ; spaces
              ; pname_ns
              ; iriref
              ; return 'l'
              })   

base = try (do {
           string "@base"
           ; iriref
           ; char '.'
           ; spaces
           ; return 'l'
           })
       <|> try (do {
           caseInsensitiveString "base"
           ; iriref
           ; return 'l'
           })



parseN3 :: String -> Either ParseError S
parseN3 s = runParser mainparser 0 "parameter" s




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





urisym = do ( alphaNum )
      -- <|> do ( plx) --escaped chars
        <|> do (oneOf "_:")
        
uriStringB = do (many $  (urisym  <|> char '.'))


uristring :: Parsec [Char] Int String           
uristring = try (do {
             
                     st <- char '\0041'   -- <- many (urisym  <|> char '.') 
                     ; char '*'
                     ; return  [st]
                     })
            <|> try (do {st <- many urisym
                    ; return st }) 
{-
[139s] 	PNAME_NS 	::= 	PN_PREFIX? ':'
[140s] 	PNAME_LN 	::= 	PNAME_NS PN_LOCAL
[141s] 	BLANK_NODE_LABEL 	::= 	'_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
[144s] 	LANGTAG 	::= 	'@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
[19] 	INTEGER 	::= 	[+-]? [0-9]+
[20] 	DECIMAL 	::= 	[+-]? [0-9]* '.' [0-9]+
[21] 	DOUBLE 	::= 	[+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
[154s] 	EXPONENT 	::= 	[eE] [+-]? [0-9]+
[22] 	STRING_LITERAL_QUOTE 	::= 	'"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"' /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
[23] 	STRING_LITERAL_SINGLE_QUOTE 	::= 	"'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'" /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
[24] 	STRING_LITERAL_LONG_SINGLE_QUOTE 	::= 	"'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
[25] 	STRING_LITERAL_LONG_QUOTE 	::= 	'"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
[26] 	UCHAR 	::= 	'\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
[159s] 	ECHAR 	::= 	'\' [tbnrf"'\]
[161s] 	WS 	::= 	#x20 | #x9 | #xD | #xA /* #x20=space #x9=character tabulation #xD=carriage return #xA=new line */
[162s] 	ANON 	::= 	'[' WS* ']'

[164s] 	PN_CHARS_U 	::= 	PN_CHARS_BASE | '_'
[166s] 	PN_CHARS 	::= 	PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
[167s] 	PN_PREFIX 	::= 	PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
[168s] 	PN_LOCAL 	::= 	(PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?




[163s] 	PN_CHARS_BASE 	::= 	[A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
-- I won't implement that, I just hope it is covered by alphanum bzw. letter
-}









iriref :: Parsec [Char] Int String 
iriref = try (do {
            spaces
            ;char '<'
            ; uri <- many $ ( (noneOf  (['\x00'..'\x20'] ++ "<>\"{}|^`\\"))<|>  uchar)
            ; char '>'
            ;spaces
            ;return uri
            })
            
pname_ns :: Parsec [Char] Int String 
pname_ns = try (do {
              a <- option [] pn_prefix
              ; char ':'
              ; return (a++":")
              })

pname_ln :: Parsec [Char] Int String
pname_ln = do {
              a <- pname_ns
              ; b <- pn_local
              ; return (a++b)
              }

--remark: slightly different then in the original grammar, we won't keep the "_:"
blank_node = do {
                      a <- oneOf $ pn_chars_u++['0' .. '9']
                      ; b <- option [] p_end
                      ; spaces
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
                    first <- oneOf pn_chars_base
                    ; second <- try (option [] p_end)
                    ; return (first:second)
                    } 
                ) 

p_end :: Parsec [Char] Int String
p_end = 
        try (do { 
                 a <- oneOf (pn_chars++".")
                 ; b <- p_end
                 ; return ([a]++b)
                 })
        <|> try (do {            
            ;b <- try (oneOf pn_chars)
            ; return [b] 
           })







pn_local :: Parsec [Char] Int String
pn_local = try (do {
              a <- letter -- oneOf (pn_chars_u++['1' .. '9']++":") 
              ; return [a]
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


--string sets as specified in the official grammar
pn_chars_base = ['a' .. 'z']++['A' .. 'Z']++['\x00C0' .. '\x00D6']++ ['\x00D8' .. '\x00F6'] ++ ['\x00F8' .. '\x02FF'] ++ ['\x0370' .. '\x037D'] 
                        ++ ['\x037F' ..'\x1FFF'] ++ ['\x200C' .. '\x200D' ] ++ ['\x2070' .. '\x218F'] ++ ['\x2C00' .. '\x2FEF'] ++ ['\x3001' .. '\xD7FF'] ++ ['\xF900' .. '\xFDCF'] 
                        ++ ['\xFDF0' .. '\xFFFD'] ++ ['\x10000' .. '\xEFFFF']

pn_chars_u = '_':pn_chars_base 

pn_chars = pn_chars_u ++ "-"++['0'.. '9'] ++ "\x00B7"++['\x0300' .. '\x036F'] ++ ['\x203F' .. '\x2040']

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

