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
data Term = URI String | Universal String | Existential String | Literal String | Exp Expression  deriving Show 
data Expression = FE Formula | BE Bool deriving Show
data Formula = Triple Term Term Term | Conjunction Formula Formula | Implication Expression Expression  deriving Show 

def = emptyDef{ commentLine = "#"
              , opStart = oneOf ".=>"
              , opLetter = oneOf ".=>"
              , reservedOpNames = [  ".", "=>"]
              , reservedNames = ["{}", "{", "}", "false"]
              }

TokenParser{ identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved } = makeTokenParser def


formulaparser :: Parser Formula
formulaparser = try (do {
                       f1 <- simpleformula
                      ;m_reserved "."
                      ;f2 <- formulaparser
                      ; return (Conjunction f1 f2)
                     })
                 <|> do {f <-simpleformula
                         
                         ; return f}
                   <|> do {f <-simpleformula
                         ; return f}
                    
                

simpleformula = try (do {
                      s <- termparser
                     ;p <- termparser
                     ;o <- termparser
                     ; return (Triple s p o)
                     })
                <|> do {
                     e1 <- exparser
                     ; m_reserved "=>"
                     ; e2 <- exparser
                     ; return (Implication e1 e2)
                     
                     }


                 
termparser = fmap URI (char ':' >> m_identifier)
      <|> fmap Universal (char '?' >> m_identifier)
      <|> fmap Existential (char '_' >> char ':' >> m_identifier)
      <|> fmap Literal ( m_identifier)
      <|> fmap Exp ( exparser )

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
