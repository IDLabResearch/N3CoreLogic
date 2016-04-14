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
              , reservedNames = ["\"", "(", ")", "[", "]", ",", ";", "{}", "{", "}", "false", "newBlank"]
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
                    

                 
termparser = fmap Existential (char '_' >> char ':' >> m_identifier)
       <|> try (do {
                   char 'a'
                   ; space
                   ; return (URI "rdf_type")
                   })           
       <|>     try (do { 
                     pre <- m_identifier
                     ; char ':'
                     ; name <- m_identifier
                     ; return (URI (pre++"_"++name))
                     }
                 )
      <|> fmap URI (char ':' >> m_identifier)
      <|> fmap Universal (char '?' >> m_identifier)
      <|> do {
             s <-  sign
             ;n <- (floating3 True)
             ; spaces
             ; return (Literal (show(s n)))
             }
      <|> do { string "\"" 
               ; l <- litcontent 
               ;  string "\""
               ; spaces
               ; return (Literal l)
             }
      <|> fmap Exp ( exparser )
      <|> try (
              do {
              m_reserved "[]"
              ; l <- getState
              ; updateState (+1)
              ;return $ Existential $"newBlank_" ++  show(l)
             })
      <|> do { m_reserved "["
               ; t <- termparser
               ; o <- objectparser
               ; m_reserved "]" 
               ; l <- getState
               ; updateState (+1)
               ; return (BlankConstruct  (Existential $"newBlank_" ++  show(l)) t o)
          }
      <|> do {m_reserved "("
             ; p <- termlist
             ; m_reserved ")"
             ; return ( p )
             } 
      
             
             
             
      
termlist =  do {
              t <-termparser
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



prefix = do { string "@prefix"
              ; line
              ; newline 
            }

--simple constructs
line = many $ noneOf "\n"
litcontent = many $ noneOf "\""





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



















