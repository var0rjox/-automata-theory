module ParserG where
import UU_Parsing
import Scanner
import Atrib

pProg = sem_Prog_P1 <$ pPalClave "Programa" <*> pIdent <*> pDecls <*> pIntrs <* pPalClave "finPrograma"


pDecls = sem_Decls_De <$> pDecl <* pSimbolo ";" <*> pDecls
      <|>  pSucceed sem_Decls_Vacio


pDecl = sem_Decl_Dec <$ pPalClave "var" <*> pIdent <* pSimbolo ":" <*> pTipo


pIntrs = sem_Intrs_Is1 <$> pInst <* pSimbolo ";" <*> pIntrs 
    <|> pSucceed sem_Intrs_Is2 

pInst = sem_Inst_I1 <$ pPalClave "leer" <*> pIdent 
    <|> sem_Inst_I2 <$ pPalClave "escribir" <*> pIdent


pTipo = sem_Tipo_T1 <$ pPalClave "entero"
      <|> sem_Tipo_T2 <$ pPalClave "real"


