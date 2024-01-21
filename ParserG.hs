module ParserG where
import UU_Parsing
import Scanner

data Seudo = Se String Cuerpo
     deriving Show


pSeudo = (\a b -> Se a b ) <$ pPalClave "Programa" <*> pIdent <* pOpClave "()" <*> pCuerpo 


data Cuerpo = Cu Decls Sentencias
     deriving Show

pCuerpo = (\a b -> Cu a b ) <$ pPalClave "inicio" <*> pDecls <*> pSentencias <* pPalClave "fin" <* pSimbolo "."

data Decls = De Decl Decls
           | Vacio 
      deriving Show

pDecls = (\a b -> De a b ) <$> pDecl <* pSimbolo "," <*> pDecls
      <|>  pSucceed Vacio

data Decl = Dec String Tipo
      deriving Show

pDecl = (\a b -> Dec a b) <$ pPalClave "var" <*> pIdent <* pSimbolo ":" <*> pTipo

data Tipo = T1
          | T2
      deriving Show

pTipo = T1 <$ pPalClave "entero"
      <|> T2 <$ pPalClave "real"

data Sentencias = Sent Sentencia Sentencias
                | VacioS
          deriving Show

pSentencias = (\a b -> Sent a b ) <$> pSentencia <* pSimbolo ";" <*> pSentencias
           <|> pSucceed VacioS

data Sentencia = Sente String
               | Sente1 String
               | Sente2 String
          deriving Show

pSentencia = (\a -> Sente a ) <$ pPalClave "leer" <*> pIdent
      <|>  (\a -> Sente1 a ) <$ pPalClave "escribir" <*> pIdent
      <|>  (\a -> Sente2 a ) <$ pPalClave "mostrar" <* pSimbolo "(" <*> pCadena <* pSimbolo ")"

