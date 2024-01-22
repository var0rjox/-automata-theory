module Main where
import UU_Parsing
import Scanner
import ParserG



palabrasClave = [ "doctype html","html","head","meta","charset","title","body","p"]
operadores = ["<!","/>","</"]
opeBasicos = "="
simbolos = "</>!"

main = do 
       putStr "Nombre del Archivo: "
       nomF <- getLine
       tokens <-  scanner palabrasClave operadores simbolos opeBasicos nomF
       out <- parseIO pEtiquetas tokens
       putStr (show tokens)
       putStr (show  out)
