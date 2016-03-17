module N3grammar
(
mainparser
, parseN3
)where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language



data S = Mainformula Formula deriving Show
data Term = URI String | Universal String | Existential String | Literal String | Exp Expression | Objectlist Term Term | PredicateObjectList Term Term Term | BlankConstruct Term Term |Blank String deriving Show 
data Expression = FE Formula | BE Bool deriving Show
data Formula = Triple Term Term Term | Conjunction Formula Formula | Implication Expression Expression  deriving Show 

def = emptyDef{ commentLine = "#"
              , opStart = oneOf "; ,.=><"
              , opLetter = oneOf "; ,.=><"
              , reservedOpNames = [";", "<=", ".", "=>"]
              , reservedNames = [ "[", "]", ",", ";", "{}", "{", "}", "false"]
              }

TokenParser{ identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved  
           } = makeTokenParser def


formulaparser :: Parser Formula
formulaparser = try (do {
                       f1 <- simpleformula
                      ;m_reserved "."
                      ;f2 <- formulaparser
                      ; return (Conjunction f1 f2)
                     })
                   <|> do {f <- simpleformula
                         ; return f}
                    
                

simpleformula =  try (do {
                      s <- termparser
                     ;p <- termparser
                     ;o <- objectparser
                     ; return (translate s p o 1)
                     })
                <|> try(do {
                     e1 <- exparser
                     ; m_reserved "=>"
                     ; e2 <- exparser
                     ; return (Implication e1 e2)})
                  <|> do {
                     e1 <- exparser
                     ; m_reserved "<="
                     ; e2 <- exparser
                     ; return (Implication e2 e1 )   
                     }



objectparser = try (do{ o <-termparser
                  ; m_reserved ","
                  ; o2 <- objectparser
                  ; return (Objectlist o o2) } )
              <|> try (do{ o <-termparser
                  ; m_reserved ";"
                  ; p2 <- termparser
                  ; o2 <- objectparser
                  ; return (PredicateObjectList o p2 o2) } )
              <|> do{ o <- termparser
                     ; return o }
                    

                 
termparser = fmap URI (char ':' >> m_identifier)
      <|> fmap Universal (char '?' >> m_identifier)
      <|> fmap Existential (char '_' >> char ':' >> m_identifier)
      <|> fmap Literal ( m_identifier)
      <|> fmap Exp ( exparser )
      <|> try (do {m_reserved "[]"
              ;return (Blank "blank")
             })
      <|> do { m_reserved "["
               ; t <- termparser
               ; o <- objectparser
               ; m_reserved "]" 
               ; return (BlankConstruct t o)
          }
      

exparser :: Parser Expression
exparser = (m_reserved "false" >> return (BE False))
          <|> (m_reserved "{}" >> return (BE True ))
          <|> do {m_reserved "{"
                  ;f <- formulaparser
                  ; optional (m_reserved ".")
                  ; m_reserved "}"
                  ; return (FE f)
                  }
              


mainparser :: Parser S
mainparser = mparser <* eof 
            where mparser :: Parser S
                  mparser = do {
                               f <- formulaparser
                               ;m_reserved "."
                               ; return (Mainformula f)
                               
                               }
   
parseN3 :: String -> IO ()
parseN3 inp = case parse mainparser "" inp of
              { Left err -> print err
              ; Right ans -> print  ans 
              }


--todo: this works but has to be cleaned up---

translate :: Term -> Term -> Term -> Int -> Formula
translate (Blank "blank")      p o          n  =             (translate  (Blank ("blank_"++show(n) )) p o   (n+1) )
translate s     (Blank "blank")  o          n  =             (translate  s (Blank ("blank_"++show(n) )) o   (n+1) )
translate s     p (Blank "blank")           n  =             (translate  s p (Blank ("blank_"++show(n) ))   (n+1) )
translate (BlankConstruct a b) p o          n  = Conjunction (translate  (Blank ("blank_"++show(n) )) p o   (n+1) )  
                                                             (translate  (Blank ("blank_"++show(n) )) a b   (n+1) ) 
translate s (BlankConstruct a b) o          n  = Conjunction (translate   s (Blank ("blank_"++show(n) )) o  (n+1) )  
                                                             (translate  (Blank ("blank_"++show(n) )) a b   (n+1) ) 
translate s p (BlankConstruct a b)          n  = Conjunction (translate   s p (Blank ("blank_"++show(n) ))  (n+1) )  
                                                             (translate  (Blank ("blank_"++show(n) )) a b   (n+1) )   
translate s p (Objectlist o list)           n  = Conjunction (translate s p o (n+1))  (translate s p  list   (n+1) )
translate s p (PredicateObjectList o p2 o2) n  = Conjunction (translate s p o (n+1))  (translate s p2 o2     (n+1) )
translate s p o                             n  = Triple s p o


newBlankName :: String -> String -> String
newBlankName a b = a ++ "_" ++ show(length(a)) ++"_" ++ b








