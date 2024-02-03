module Main where
import Scanner
import UU_Parsing
-- import Atrib
import ParserG


palabrasClave = ["if","else","float","include","studio","h","int","main","printf","scanf","return"]

operadores = ["<=",">=","!=","()"]
opeBasicos = "+-*/=()<>!#"
simbolos = "{};,.\""

main = do 
       putStr "Nombre del Archivo: "
       nomF <- getLine
       tokens <-  scanner palabrasClave operadores simbolos opeBasicos nomF
       out <- parseIO pProg tokens
       putStr (show tokens)
       putStr (show  out)


