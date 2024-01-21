module Main where
import UU_Parsing
import Scanner
import ParserG



palabrasClave = [ "Programa","inicio","fin","mostrar","leer","escribir","var","entero","real"]
operadores = ["()"]
opeBasicos = "()"
simbolos = ";.:,"

main = do 
       putStr "Nombre del Archivo: "
       nomF <- getLine
       tokens <-  scanner palabrasClave operadores simbolos opeBasicos nomF
       out <- parseIO pSeudo tokens
       putStr (show tokens)
       putStr (show  out)
