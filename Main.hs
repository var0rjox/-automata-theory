module Main where
import Scanner
import UU_Parsing
import Atrib
import ParserG


palabrasClave = ["if","else","float","include","stdio","h","int","main","printf","scanf","return"]

operadores = ["<=",">=","!=","()"]
opeBasicos = "+-*/=()<>!#"
simbolos = "{};,.\""

generar archivo codigo = do
                             writeFile ((faux1 archivo)++".pas") codigo
                             putStr ( "Archivo: " ++(faux1 archivo)++".pas " ++"generado")

faux1 ('.':xs)=[]
faux1 []      =[]
faux1 (x:xs)  =x : faux1 xs

main = do 
       putStr "Nombre del Archivo: "
       nomF <- getLine
       tokens <-  scanner palabrasClave operadores simbolos opeBasicos nomF
       out <- parseIO pProg tokens  -- une el scanner a parser
       let (cod,con,d,r,u) = out
       generar nomF cod
       putStr (show "Esta es la lista de Declarados:" ++ show d ++"\n")
       putStr (show "Esta es la lista de Usados:" ++ show u ++"\n")
       putStr (show "Resultado de Variables no declaradas:" ++ show r ++"\n")
       putStr (show "Resultado de Contador de declaradas:" ++ show con ++"\n")