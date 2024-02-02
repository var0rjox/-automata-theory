module ParserG where
import UU_Parsing
import Scanner
import Atrib

-- Definición de tipos de datos para representar la estructura del programa
data Prog = Prog Cuerpo
       deriving Show

pProg = Prog <$ pPalClave "include" <* pSimbolo "<" <* pPalClave "stdio" <* pSimbolo "." <* pPalClave "h" <* pSimbolo ">" <*> pMetodos <*> pMain

data Cuerpo = Body Declaraciones Asignacion Condicional String
       deriving Show

pCuerpo = (\a b c -> Body a b c ) <$ pPalClave "begin" <*> pDeclaraciones <*> pAsignacion <*> pCondicional <* pPalClave "return" <*> pIdent <* pSimbolo ";" <* pPalClave "end" <* pSimbolo "."

data Tipo = T1 
          | T2
      deriving Show

pTipo = T1 <$ pPalClave "int"
    <|> T2 <$ pPalClave "double"

data Declaraciones = Declars Tipo String Declaraciones
       | VacioS
       deriving Show

pDeclaraciones = (\a b c -> Declars a b c ) <$> pTipo <*> pIdent <* pSimbolo ";" <*> pDeclaraciones
  <|>  pSucceed VacioS

data Asignacion = Asignacions String Expresion Asignacion
        | VacioAsig
       deriving Show

pAsignacion = (\a b -> Asignacions a b ) <$> pIdent <* pSimbolo "=" <*> pExpresion <* pSimbolo ";" <*> pAsignacion
  <|>  pSucceed VacioAsig

data Condicional = Condi Expresion Asignacion
       deriving Show

pCondicional = (\a -> Condi a ) <$ pPalClave "if" <*> pExpresion <* pSimbolo "{" <*> pAsignacion <* pSimbolo "}"

--- cosas para reusar

data Expresion = Expresion Relacion A
        deriving Show

data A = A OpLog Expresion
       | AVacio
       deriving Show

data Relacion  = Relacion Terminos  B
       deriving Show

data B = B SimOps Relacion
       | BVacio
      deriving Show
                
data Terminos = Terminos Fr C
       deriving Show

data C = C SimOps Terminos 
       | CVacio
     deriving Show

data Fr  = Fr Expresion
         | Fr1 String
         | Fr2 Int 
       deriving Show

data OpLog = Opmenor
           | Opmayor
           | Opmayorigual
           | Opmenorigual
           | Opdistinto
           | Opigual
           | OAnd
           | OOr
       deriving Show

data SimOps = Smas
            | Smenos
            | Smul
            | Sdiv
      deriving Show

-- Definición de los parsers

pExpresion = (\a b -> Expresion a b ) <$>pRelacion <*> pA

pA = (\a b -> A a b ) <$> pOpLog <*> pExpresion
  <|>  pSucceed AVacio
 
pRelacion = (\a b -> Relacion a b ) <$> pTerminos <*> pB

pB = (\a b -> B a b ) <$> pSimOps <*> pRelacion 
  <|>  pSucceed BVacio

pTerminos = (\a b -> Terminos a b ) <$> pFr <*> pC

pC = (\a b -> C a b ) <$>pSimOps <*>pTerminos 
  <|>  pSucceed CVacio        

pFr = (\a -> Fr a ) <$ pSimbolo "(" <*> pExpresion <* pSimbolo ")"
     <|>(\a -> Fr1 a ) <$> pIdent  
     <|> (\a -> Fr2 a ) <$> pInt 

pOpLog = Opmenor <$ pSimbolo "<"
       <|> Opmenorigual <$ pOpClave "<="
       <|> Opmayor <$ pSimbolo ">"
       <|> Opmayorigual <$ pOpClave ">=" 
       <|> Opdistinto <$ pOpClave "!="
       <|> Opigual <$ pSimbolo "="
       <|> OAnd <$ pPalClave "Y"
       <|> OOr <$ pPalClave "O"

pSimOps = Smas <$ pSimbolo "+"
    <|> Smenos <$ pSimbolo "-"
    <|> Smul <$ pSimbolo "*"
    <|> Sdiv <$ pSimbolo "/"
