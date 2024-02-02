module Scanner where

import UU_Parsing

type Tokens = [Token]

data Token  = Token Tok String NumLin
type NumLin = Int

instance Show Token where
  show (Token t str nl) = show t ++ " " ++ show str ++ " en la linea " ++ show nl ++ "\n"

data Tok  = TokIdent
          | TokPalClave
          | TokOpClave
          | TokSimbolo
          | TokEntero
          | TokCadena
          | TokError
          deriving (Eq, Ord)

instance Show Tok where
  show TokPalClave = " Palabra clave : "
  show TokOpClave  = " Operador clave: "
  show TokIdent    = " Identificador : "
  show TokSimbolo  = " Simbolo      : "
  show TokEntero   = " Numero entero : "
  show TokCadena   = " Cadena : "
  show TokError    = " Error         : "

scanner :: PalsClave -> OpsClave -> OpSimbs -> OpBasic -> FileName -> IO Tokens
scanner psc osc oss bas fn = do tokens <- tokenize psc osc oss bas fn
                                return tokens

type FileName  = String
type PalsClave = [String]
type OpsClave  = [String]
type OpSimbs   = String
type OpBasic   = String

tokenize psc osc oss bas fn = do input <- readFile fn
                                 let tokens = scan psc osc oss bas input 1
                                 return tokens

scan psc osc oss bas xs n = scan' xs n
  where scan' []         _ = []
        scan' xxs@(x:xs) n
          = if isSpaceComent x xs then scan' nbs nn else Token tok str n : scan' rs n
          where (tok,str,rs) = token x xs
                (nn,nbs)     = saltarBlancos xxs n

        isOperator x       = x `elem` bas
        isSimbolo x        = x `elem` oss
        isSpaceComent x xs = isSpace x || isComentario x xs

        palClave str
          | str `elem` psc = TokPalClave
          | otherwise      = TokIdent

        opClave str
          | str `elem` osc = TokOpClave
          | otherwise      = TokSimbolo

        token x xs
          | isDigit x    = let (str,xss) = span isDigit    xs
                           in  (TokEntero   , x:str, xss)
          | isAlpha x    = let (str,xss) = span isAlphaNum xs
                               nxs       = x:str
                           in  (palClave nxs, nxs  , xss)
          | isComillaD x = let(str,xss)  = span noComillaD xs
                              c          = sacarElem xss
                              nxs        = c:str++c:[]
                           in (TokCadena,nxs,(evalCaden c xss))
          | isOperator x = let (str,xss) = span isOperator xs
                               nxs       = x:str
                           in  (opClave nxs , nxs  , xss)
         
          | isSimbolo x = (TokSimbolo , x:[]  , xs) 
          | otherwise    = (TokError, "simbolo desconocido \"" ++ x:[] ++ "\"", xs)

saltarBlancos []     n = (n,[])
saltarBlancos xxs@(x:xs) n
  | isSpace x = saltarBlancos xs (n + if x == '\n' then 1 else 0)
  | (x == '/' && (head xs) == '/') = (n, (dropWhile (/='\n') xs)) 
  | otherwise = (n,xxs)

isComentario  x xs =  (x == '/' && (head xs) == '/')
		      
isComillaD x  
	| x == '"'=True
	| x == '\''=True
	| x == '`'=True
	|otherwise  = False	

noComillaD x
	|x== '"' =   False
        | x == '\''= False
	| x == '`'=  False
	|(x== '\n')  = False
    |otherwise  = True

evalCaden c xss
	|c == '\n' =xss
	|otherwise = (tail xss)


sacarElem str@(x:xs) = x


-- Integracion con el Parser

instance Eq Tok => Eq Token where
  (Token TokIdent   _  _) == (Token TokIdent   _  _) = True
  (Token TokEntero  _  _) == (Token TokEntero  _  _) = True
  (Token TokCadena  _  _) == (Token TokCadena  _  _) = True
  (Token t1         s1 _) == (Token t2         s2 _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x==y      = EQ
              | x<=y      = LT
              | otherwise = GT
  Token tok1 str1 _ <= Token tok2 str2 _
      = tok1 < tok2 || (tok1 == tok2 && str1 <= str2)

instance Symbol Token

obtenerVal (Token _ v _) = v

tSym :: Tok -> String -> Parser Token String
tSym tk str = obtenerVal <$> pSym (Token tk str 0)

pInt         = (\x -> (read x) :: Int) <$> tSym TokEntero   ""
pIdent       =                             tSym TokIdent    ""
pCadena      =                             tSym TokCadena   ""
pPalClave pc =                             tSym TokPalClave pc
pOpClave  oc =                             tSym TokOpClave  oc
pSimbolo  si =                             tSym TokSimbolo  si 
