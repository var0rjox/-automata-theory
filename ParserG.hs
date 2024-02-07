module ParserG where
import UU_Parsing
import Scanner
import Atrib

-- -- Definición de tipos de datos para representar la estructura del programa
-- data Prog = Prog Metodos Main
--        deriving Show

-- pProg = Prog <$ pSimbolo "#" <* pPalClave "include" <* pSimbolo "<" <* pPalClave "stdio" <* pSimbolo "." <* pPalClave "h" <* pSimbolo ">" <*> pMetodos <*> pMain

-- -- Completo pProg y Prog
-- data Metodos = Metds Metodo Metodos
--        | VacioMet
--        deriving Show

-- pMetodos = (\a b -> Metds a b ) <$> pMetodo <*> pMetodos
--   <|>  pSucceed VacioMet

-- data Metodo = Metd Instancia Instancia Instancia CuerpoMet
--        deriving Show

-- pMetodo = (\a b c d -> Metd a b c d ) <$> pInstancia <* pSimbolo "(" <*> pInstancia <* pSimbolo "," <*> pInstancia <* pSimbolo ")"  <* pSimbolo "{" <*> pCuerpoMet <* pSimbolo "}"

-- data Instancia = Instancia Tipo String
--        deriving Show

-- pInstancia = (\a b -> Instancia a b ) <$> pTipo <*> pIdent

-- data Instancias = Instancias Instancia Instancias
--         | VacioInst
--        deriving Show

-- pInstancias = (\a b -> Instancias a b ) <$> pInstancia <* pSimbolo ";" <*> pInstancias
--   <|>  pSucceed VacioIns

-- data CuerpoMet = CuerpoM Expresion 
--         | CuerpoMCondicional Expresion Funciones Expresion -- if (Expresion) Funciones else return Expresion
--        deriving Show
       
-- pCuerpoMet = (\a -> CuerpoM a ) <$ pPalClave "return" <*> pExpresion <* pSimbolo ";"
--   <|> (\a b c -> CuerpoMCondicional a b c ) <$ pPalClave "if" <*> pExpresion <* pSimbolo "{" <*> pFunciones <* pSimbolo "}" <* pPalClave "else" <* pSimbolo "{" <* pPalClave "return" <*> pExpresion <* pSimbolo ";" <* pSimbolo "}"

-- data Funcion = Funcion1 String Mostrar --printf
--         | Funcion2 String -- scanf
--         | Funcion3 Instancia String String String -- llamada a funcion
--        deriving Show

-- pFuncion = (\a b -> Funcion1 a b ) <$ pPalClave "printf" <* pSimbolo "(" <*> pCadena <*> pMostrar <* pSimbolo ")"
--   <|> (\a -> Funcion2 a ) <$ pPalClave "scanf" <* pSimbolo "(" <*> pIdent <* pSimbolo ")"
--   <|> (\a b c d -> Funcion3 a b c d ) <$> pInstancia <* pSimbolo "=" <*> pIdent <* pSimbolo "(" <*> pIdent <* pSimbolo "," <*> pIdent <* pSimbolo ")"

-- data Mostrar = Mostrar String -- , pIdent
--         | MostrarVacio
--        deriving Show

-- pMostrar = (\a -> Mostrar a ) <$ pSimbolo "," <*> pIdent
--   <|>  pSucceed MostrarVacio

-- data Funciones = Funciones Funcion Funciones
--        | VacioFuncs
--        deriving Show

-- pFunciones = (\a b -> Funciones a b ) <$> pFuncion  <* pSimbolo ";" <*> pFunciones
--   <|>  pSucceed VacioFuncs

-- data Main = Main Instancias Funciones Int
--        deriving Show

-- pMain = Main <$ pPalClave "int" <* pPalClave "main" <* pOpClave "()" <* pSimbolo "{" <*> pInstancias <*> pFunciones <* pPalClave "return" <*> pInt <* pSimbolo ";" <* pSimbolo "}"
-- ---- Modificaciones de practicas anteriores :

-- data Tipo = T1
--           | T2
--       deriving Show

-- pTipo = T1 <$ pPalClave "int"
--     <|> T2 <$ pPalClave "float"


-- --- cosas para reusar

-- data Expresion = Expresion Relacion A
--         deriving Show

-- data A = A OpLog Expresion
--        | AVacio
--        deriving Show

-- data Relacion  = Relacion Terminos  B
--        deriving Show

-- data B = B SimOps Relacion
--        | BVacio
--       deriving Show
                
-- data Terminos = Terminos Fr C
--        deriving Show

-- data C = C SimOps Terminos 
--        | CVacio
--      deriving Show

-- data Fr  = Fr Expresion
--          | Fr1 String
--          | Fr2 Int 
--        deriving Show

-- data OpLog = Opmenor
--            | Opmayor
--            | Opmayorigual
--            | Opmenorigual
--            | Opdistinto
--        deriving Show

-- data SimOps = Smas
--             | Smenos
--             | Smul
--             | Sdiv
--       deriving Show

-- -- Definición de los parsers

-- pExpresion = (\a b -> Expresion a b ) <$>pRelacion <*> pA

-- pA = (\a b -> A a b ) <$> pOpLog <*> pExpresion
--   <|>  pSucceed AVacio
 
-- pRelacion = (\a b -> Relacion a b ) <$> pTerminos <*> pB

-- pB = (\a b -> B a b ) <$> pSimOps <*> pRelacion 
--   <|>  pSucceed BVacio

-- pTerminos = (\a b -> Terminos a b ) <$> pFr <*> pC

-- pC = (\a b -> C a b ) <$>pSimOps <*>pTerminos 
--   <|>  pSucceed CVacio        

-- pFr = (\a -> Fr a ) <$ pSimbolo "(" <*> pExpresion <* pSimbolo ")"
--      <|>(\a -> Fr1 a ) <$> pIdent
--      <|> (\a -> Fr2 a ) <$> pInt 

-- pOpLog = Opmenor <$ pSimbolo "<"
--        <|> Opmenorigual <$ pOpClave "<="
--        <|> Opmayor <$ pSimbolo ">"
--        <|> Opmayorigual <$ pOpClave ">=" 
--        <|> Opdistinto <$ pOpClave "!="


-- pSimOps = Smas <$ pSimbolo "+"
--     <|> Smenos <$ pSimbolo "-"
--     <|> Smul <$ pSimbolo "*"
--     <|> Sdiv <$ pSimbolo "/"

-- semantica nueva

pProg = sem_Prog_Prog <$ pSimbolo "#" <* pPalClave "include" <* pSimbolo "<" <* pPalClave "stdio" <* pSimbolo "." <* pPalClave "h" <* pSimbolo ">" <*> pMetodos <*> pMain

pMetodos = sem_Metodos_Metds <$> pMetodo <*> pMetodos
  <|>  pSucceed sem_Metodos_VacioMet


pMetodo = sem_Metodo_Metd <$> pInstancia <* pSimbolo "(" <*> pInstancia <* pSimbolo "," <*> pInstancia <* pSimbolo ")"  <* pSimbolo "{" <*> pCuerpoMet <* pSimbolo "}"


pInstancia = sem_Instancia_Instancia <$> pTipo <*> pIdent


pInstancias = sem_Instancias_Instancias <$> pInstancia <* pSimbolo ";" <*> pInstancias
  <|>  pSucceed sem_Instancias_VacioIns

       
pCuerpoMet = sem_CuerpoMet_CuerpoM <$ pPalClave "return" <*> pExpresion <* pSimbolo ";"
  <|> sem_CuerpoMet_CuerpoMCondicional <$ pPalClave "if" <*> pExpresion <* pSimbolo "{" <*> pFunciones <* pSimbolo "}" <* pPalClave "else" <* pSimbolo "{" <* pPalClave "return" <*> pExpresion <* pSimbolo ";" <* pSimbolo "}"



pFuncion = sem_Funcion_Funcion1 a b ) <$ pPalClave "printf" <* pSimbolo "(" <*> pCadena <*> pMostrar <* pSimbolo ")"
  <|> sem_Funcion_Funcion2 <$ pPalClave "scanf" <* pSimbolo "(" <*> pIdent <* pSimbolo ")"
  <|> sem_Funcion_Funcion3 <$> pInstancia <* pSimbolo "=" <*> pIdent <* pSimbolo "(" <*> pIdent <* pSimbolo "," <*> pIdent <* pSimbolo ")"



pMostrar =sem_Mostrar_Mostrar <$ pSimbolo "," <*> pIdent
  <|>  pSucceed sem_Mostrar_MostrarVacio



pFunciones = sem_Funciones_Funciones <$> pFuncion  <* pSimbolo ";" <*> pFunciones
  <|>  pSucceed sem_Funciones_VacioFuncs


pMain = sem_Main_Main <$ pPalClave "int" <* pPalClave "main" <* pOpClave "()" <* pSimbolo "{" <*> pInstancias <*> pFunciones <* pPalClave "return" <*> pInt <* pSimbolo ";" <* pSimbolo "}"


pTipo = sem_Tipo_T1 <$ pPalClave "int"
    <|> sem_Tipo_T2 <$ pPalClave "float"

pExpresion = sem_Expresion_Expresion <$>pRelacion <*> pA

pA = sem_A_A <$> pOpLog <*> pExpresion
  <|>  pSucceed sem_A_AVacio
 
pRelacion = sem_Relacion_Relacion <$> pTerminos <*> pB

pB = sem_B_B <$> pSimOps <*> pRelacion 
  <|>  pSucceed sem_B_BVacio

pTerminos = sem_Terminos_Terminos <$> pFr <*> pC

pC = sem_C_C <$>pSimOps <*>pTerminos 
  <|>  pSucceed sem_C_CVacio        

pFr = sem_Fr_Fr <$ pSimbolo "(" <*> pExpresion <* pSimbolo ")"
     <|>sem_Fr_Fr1 <$> pIdent
     <|>sem_Fr_Fr2 <$> pInt 

pOpLog = sem_OpLog_Opmenor <$ pSimbolo "<"
       <|> sem_OpLog_Opmenorigual <$ pOpClave "<="
       <|> sem_OpLog_Opmayor <$ pSimbolo ">"
       <|> sem_OpLog_Opmayorigual <$ pOpClave ">=" 
       <|> sem_OpLog_Opdistinto <$ pOpClave "!="


pSimOps = sem_SimOps_Smas <$ pSimbolo "+"
    <|> sem_SimOps_Smenos <$ pSimbolo "-"
    <|> sem_SimOps_Smul <$ pSimbolo "*"
    <|> sem_SimOps_Sdiv <$ pSimbolo "/"
