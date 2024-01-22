module ParserG where

import UU_Parsing
import Scanner

data Etiquetas = Et Etiquetas
                | Et1 Etiquetas Etiquetas
                | Et2 Etiquetas
                | Et3 Etiquetas
                | Et4 String
                | Et5 String
                | Et6 String
                | VacioS
              deriving Show
pDoctype = Et <$ pOpClave "<!" <* pPalClave "doctype html" <* pSimbolo ">" <*> pHtml

pHtml = (\a b -> Et1 a b) <$ pSimbolo "<" <* pPalClave "html" <* pSimbolo ">" <*> pHead <*> pBody <* pOpClave "</" <* pPalClave "html" <* pSimbolo ">"

pHead = (\a -> Et2 a) <$ pSimbolo "<" <* pPalClave "head" <* pSimbolo ">" <*> (pMeta <|> pTitle) <* pOpClave "</" <* pPalClave "head" <* pSimbolo ">"

pBody = (\a -> Et3 a) <$ pSimbolo "<" <* pPalClave "body" <* pSimbolo ">" <*> pParrafo <* pOpClave "</" <* pPalClave "body" <* pSimbolo ">"

pMeta = (\a -> Et4 a) <$ pSimbolo "<" <* pPalClave "meta" <* pPalClave "charset" <* pSimbolo "=" <*> pCadena <* pOpClave "</" <* pPalClave "head" <* pSimbolo ">"

pTitle = (\a -> Et5 a) <$ pSimbolo "<" <* pPalClave "title" <* pSimbolo ">" <*> pIdent <* pOpClave "</" <* pPalClave "title" <* pSimbolo ">"

pParrafo = (\a -> Et6 a) <$ pSimbolo "<" <* pPalClave "p" <* pSimbolo ">" <*> pCadena <* pOpClave "</" <* pPalClave "p" <* pSimbolo ">"

pEtiquetas = pDoctype
          <|> pHtml
          <|> pHead
          <|> pBody
          <|> pMeta
          <|> pTitle
          <|> pParrafo
          <|> pSucceed VacioS