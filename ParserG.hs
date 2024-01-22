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

pEtiquetas = Et <$ pOpClave "<!" <* pPalClave"doctype html" <* pSimbolo ">" <*> pEtiquetas
      <|>  (\a b -> Et1 a b) <$ pSimbolo "<" <* pPalClave "html" <* pSimbolo ">" <*> pEtiquetas <*> pEtiquetas <* pOpClave "</" <* pPalClave "html" <* pSimbolo ">"
      <|>  (\a -> Et2 a) <$ pSimbolo "<" <* pPalClave "head" <* pSimbolo ">" <*> pEtiquetas <* pOpClave "</" <* pPalClave "head" <* pSimbolo ">"
      <|>  (\a -> Et3 a) <$ pSimbolo "<" <* pPalClave "body" <* pSimbolo ">" <*> pEtiquetas <* pOpClave "</" <* pPalClave "body" <* pSimbolo ">" -- checar la parte recursiva de llamada a estos elementos
      <|>  (\a -> Et4 a) <$ pSimbolo "<" <* pPalClave "meta" <* pPalClave "charset" <* pSimbolo "=" <*> pCadena <* pOpClave "</" <* pPalClave "head" <* pSimbolo ">"
      <|>  (\a -> Et5 a) <$ pSimbolo "<" <* pPalClave "title" <* pSimbolo ">" <*> pIdent <* pOpClave "</" <* pPalClave "title" <* pSimbolo ">"
      <|> (\a -> Et6 a) <$ pSimbolo "<" <* pPalClave "p" <* pSimbolo ">" <*> pCadena <* pOpClave "</" <* pPalClave "p" <* pSimbolo ">"
      <|> pSucceed VacioS