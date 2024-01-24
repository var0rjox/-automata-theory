module Main where
import UU_Parsing
import Scanner
import ParserG


palabrasClave = ["Codigo","Foltran","begin","int","double","if","return","end","mod","Y","O"]

operadores = ["<=",">=","!="]
opeBasicos = "+-*/=()<>!"
simbolos = "{};:,.\""

main = do 
       putStr "Nombre del Archivo: "
       nomF <- getLine
       tokens <-  scanner palabrasClave operadores simbolos opeBasicos nomF
       out <- parseIO pProg tokens
       putStr (show tokens)
       putStr (show  out)


