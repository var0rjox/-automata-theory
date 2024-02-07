module ParserG where
import UU_Parsing
import Scanner
import Atrib


pProg = sem_Prog_Prog <$ pSimbolo "#" <* pPalClave "include" <* pSimbolo "<" <* pPalClave "stdio" <* pSimbolo "." <* pPalClave "h" <* pSimbolo ">" <*> pMetodos <*> pMain

pMetodos = sem_Metodos_Metds <$> pMetodo <*> pMetodos
  <|>  pSucceed sem_Metodos_VacioMet


pMetodo = sem_Metodo_Metd <$> pInstancia <* pSimbolo "(" <*> pInstancia1 <* pSimbolo "," <*> pInstancia2 <* pSimbolo ")"  <* pSimbolo "{" <*> pCuerpoMet <* pSimbolo "}"


pInstancia = sem_Instancia_Instancia <$> pTipo <*> pIdent

pInstancia1 = sem_Instancia1_Instancia1 <$> pTipo <*> pIdent

pInstancia2 = sem_Instancia2_Instancia2 <$> pTipo <*> pIdent

pInstancias = sem_Instancias_Instancias <$> pInstancia <* pSimbolo ";" <*> pInstancias
  <|>  pSucceed sem_Instancias_VacioInst

       
pCuerpoMet = sem_CuerpoMet_CuerpoM <$ pPalClave "return" <*> pExpresion <* pSimbolo ";"
  <|> sem_CuerpoMet_CuerpoMCondicional <$ pPalClave "if" <*> pExpresion <* pSimbolo "{" <*> pFunciones <* pSimbolo "}" <* pPalClave "else" <* pSimbolo "{" <* pPalClave "return" <*> pExpresion1 <* pSimbolo ";" <* pSimbolo "}"



pFuncion = sem_Funcion_Funcion1 <$ pPalClave "printf" <* pSimbolo "(" <*> pCadena <*> pMostrar <* pSimbolo ")"
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

pExpresion1 = sem_Expresion1_Expresion1 <$>pRelacion <*> pA

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


pSimOps = sem_SimpOps_Smas <$ pSimbolo "+"
    <|> sem_SimpOps_Smenos <$ pSimbolo "-"
    <|> sem_SimpOps_Smul <$ pSimbolo "*"
    <|> sem_SimpOps_Sdiv <$ pSimbolo "/"
