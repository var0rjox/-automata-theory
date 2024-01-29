module Main where
import Scanner
import UU_Parsing
import Atrib
import ParserG

palabrasClave = ["Programa","leer","escribir","finPrograma","var","entero"]

operadores = []

simbolos = ";:"

main = do 
       putStr "Ingrese el Nombre del Archivo: "
       nomF <- getLine
       tokens <- scanner palabrasClave operadores simbolos nomF
       putStr (show tokens)
       out <- parseIO pProg tokens  -- une el scanner a parser
       let (con,d,r,u) = out
       putStr (show "Esta es la lista de Declarados:" ++ show d ++"\n")
       putStr (show "Esta es la lista de Usados:" ++ show u ++"\n")
       putStr (show "Resultado de Varibles no declaradas:" ++ show r ++"\n")
       putStr (show "Resultado de Contador de declaradas:" ++ show con ++"\n")