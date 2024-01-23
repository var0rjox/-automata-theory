module ParserG where

import UU_Parsing
import Scanner

data OpEtiquetas = Opmenor
           | Opmenorcierre
           | Opmayor
           | Opmayorcierre
           | Opmenordistinto
       deriving Show

data Seudo = Seu HTML
            deriving Show
data HTML = Estructura Cabeza Body
            deriving Show
data Cabeza = Cab EtiquetasCabeza
            deriving Show
data EtiquetasCabeza =  EtiquetaCabeza1 String
          | EtiquetaCabeza2 String
        deriving Show
data Body = Bod EtiquetasBody
            deriving Show
data EtiquetasBody = EtiquetaBody1 String 
        deriving Show

pSeudo = (\a -> Seu a) <$ pOpEtiquetas <* pPalClave "doctype html" <*> pOpEtiquetas <*> pHtml


pHtml = (\a b -> Estructura a b) <$ pOpEtiquetas <* pPalClave "html" <*> pOpEtiquetas <*> pCabeza <*> pBody <*> pOpEtiquetas <* pPalClave "html" <*> pOpEtiquetas


pCabeza = (\a -> Cab a) <$ pOpEtiquetas <* pPalClave "head" <*> pOpEtiquetas <*> pEtiquetasCabeza <*> pOpEtiquetas <* pPalClave "head" <*> pOpEtiquetas


pBody = (\a -> Bod a) <$ pOpEtiquetas <* pPalClave "body" <*> pOpEtiquetas <*> pEtiquetasBody <*> pOpEtiquetas <* pPalClave "body" <*> pOpEtiquetas


pEtiquetasCabeza = (\a -> EtiquetaCabeza1 a) <$ pOpEtiquetas <* pPalClave "meta" <* pPalClave "charset" <* pSimbolo "=" <*> pCadena <*> pOpEtiquetas
     <|> (\a -> EtiquetaCabeza2 a) <$ pOpEtiquetas <* pPalClave "title" <*> pOpEtiquetas <*> pIdent <*> pOpEtiquetas <* pPalClave "title" <*> pOpEtiquetas

pEtiquetasBody = (\a -> EtiquetaBody1 a) <$ pOpEtiquetas <* pPalClave "p" <*> pOpEtiquetas <*> pCadena <*> pOpEtiquetas <* pPalClave "p" <*> pOpEtiquetas

pOpEtiquetas = Opmenor <$ pSimbolo "<"
       <|> Opmenorcierre <$ pOpClave "</"
       <|> Opmayor <$ pSimbolo ">"
       <|> Opmayorcierre <$ pOpClave "/>" 
       <|> Opmenordistinto <$ pOpClave "<!"