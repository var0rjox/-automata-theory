module Auxiliar where
import Scanner



data Error = ErrorSemantico String
           deriving Show

  
validacion w = null w  

mensajeError a = putStr ("***Error semantico:*** \n" ++ " Variables no declaradas:  " ++ show (a))
