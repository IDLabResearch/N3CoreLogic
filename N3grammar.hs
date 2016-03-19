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
import Control.Monad.Supply


data S = Mainformula Formula deriving Show
data Term =  URI String 
           | Universal String 
           | Existential String 
           | Literal String 
           | Exp Expression 
--TODO Exp2 construction should be fixed
           | Exp2 Expression 
           | Objectlist Term Term 
           | PredicateObjectList Term Term Term 
           | BlankConstruct Term Term 
           | Blank String 
           deriving Show 
data Expression = FE Formula | BE Bool deriving Show
data Formula = Triple Term Term Term | Conjunction Formula Formula | Implication Expression Expression deriving Show 

--TODO separate input grammer from pure grammar then translate to core

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

--todo: need to make sure that the blanks from both sides are different
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
                     ; return (Triple s p o)
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
      <|> fmap Exp2 ( exparser )
      <|> try (
              do {
              m_reserved "[]"
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
                               ; return (Mainformula (cleanUp f "blank"))
                               
                               }
   
parseN3 :: String -> IO ()
parseN3 inp = case parse mainparser "" inp of
              { Left err -> print err
              ; Right ans -> print  ans 
              }


--todo: this works but has to be cleaned up (you can for sure do it shorter)---






--function to get rid of blank node constructions in expressions
cleanExp :: Expression -> String -> Expression
cleanExp (FE f)  st = (FE (cleanUp f st ))
cleanExp e       st = e


cleanUp :: Formula -> String -> Formula
cleanUp (Implication e1 e2) st = (Implication ( cleanExp e1 (st++show(1)))( cleanExp e2 (st++show(2))))
cleanUp (Conjunction f1 f2) st = (Conjunction ( cleanUp  f1 (st++show(1)))( cleanUp  f2 (st++show(2))))

cleanUp (Triple (Blank "blank") p o ) st = cleanUp (Triple ( Existential (st++show(1))) p o) (st++show(1))   
cleanUp (Triple s (Blank "blank") o ) st = cleanUp (Triple  s (Existential (st++show(2))) o) (st++show(2)) 
cleanUp (Triple s p (Blank "blank") ) st = cleanUp (Triple  s p (Existential (st++show(3)))) (st++show(3)) 

cleanUp (Triple (Exp2 e) p o )        st  = cleanUp (Triple ( Exp (cleanExp e (st++show(1)))) p o) (st++show(1)) 
cleanUp (Triple s (Exp2 e) o )        st  = cleanUp (Triple  s (Exp (cleanExp e (st++show(2)))) o) (st++show(2))
cleanUp (Triple s p (Exp2 e) )        st  = cleanUp (Triple  s p (Exp (cleanExp e (st++show(3))))) (st++show(3))

cleanUp (Triple (BlankConstruct a b) p o) st = (\x 
                                                -> 
                                               (cleanUp (Conjunction (Triple (Existential x) p o) (Triple (Existential x) a b)) x ))
                                               (st++show(1))
cleanUp (Triple s (BlankConstruct a b) o) st = (\x 
                                               -> 
                                               (cleanUp (Conjunction (Triple s (Existential x) o) (Triple (Existential x) a b)) x ))
                                               (st++show(2))
cleanUp (Triple s p (BlankConstruct a b)) st = (\x 
                                               ->
                                               (cleanUp (Conjunction (Triple s p (Existential x)) (Triple (Existential x) a b)) x ))
                                               (st++show(3))

cleanUp (Triple s p (Objectlist o list) ) st          = cleanUp (Conjunction (Triple s p o) (Triple s p list)) st
cleanUp (Triple s p (PredicateObjectList o p2 o2)) st = cleanUp (Conjunction (Triple s p o) (Triple s p2 o2))  st 

cleanUp f st = f







































