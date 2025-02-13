-- Función que determina si un número es primo
esPrimo :: Int -> Bool
esPrimo n
    | n < 2     = False
    | otherwise = not (any divisible [2..(n-1)])
    where divisible x = n `mod` x == 0

-- Función que convierte un número en su forma escrita en español
numeroEnEspañol :: Int -> String
numeroEnEspañol n
    | n == 0    = "Cero"
    | n == 1    = "Uno"
    | n == 2    = "Dos"
    | n == 3    = "Tres"
    | n == 4    = "Cuatro"
    | n == 5    = "Cinco"
    | n == 6    = "Seis"
    | n == 7    = "Siete"
    | n == 8    = "Ocho"
    | n == 9    = "Nueve"
    | n == 10   = "Diez"
    | n == 11   = "Once"
    | n == 12   = "Doce"
    | n == 13   = "Trece"
    | n == 14   = "Catorce"
    | n == 15   = "Quince"
    | n == 16   = "Dieciséis"
    | n == 17   = "Diecisiete"
    | n == 18   = "Dieciocho"
    | n == 19   = "Diecinueve"
    | n == 20   = "Veinte"
    | n == 30   = "Treinta"
    | n == 40   = "Cuarenta"
    | n == 50   = "Cincuenta"
    | n == 60   = "Sesenta"
    | n == 70   = "Setenta"
    | n == 80   = "Ochenta"
    | n == 90   = "Noventa"
    | n == 100  = "Cien"
    | n < 100   = let (decenas, unidades) = (n `div` 10, n `mod` 10)
                      decenasStr = numeroEnEspañol (decenas * 10)
                      unidadesStr = numeroEnEspañol unidades
                  in if unidades == 0
                     then decenasStr
                     else decenasStr ++ " y " ++ unidadesStr
    | n < 1000  = let (centenas, resto) = (n `div` 100, n `mod` 100)
                      centenasStr = numeroEnEspañol (centenas) ++ "cientos"
                  in if resto == 0
                     then centenasStr
                     else centenasStr ++ " " ++ numeroEnEspañol resto
    | n < 1000000 = let (miles, resto) = (n `div` 1000, n `mod` 1000)
                        milesStr = numeroEnEspañol miles ++ " mil"
                    in if resto == 0
                       then milesStr
                       else milesStr ++ " " ++ numeroEnEspañol resto
    | n == 1000000 = "Un millón"
    | otherwise    = "Número demasiado grande"

-- Función principal que combina todo
fizzBuzz :: Int -> String
fizzBuzz n
    | esPrimo n = "FizzBuzz!"
    | otherwise = numeroEnEspañol n


main :: IO ()
main = do
    putStrLn "Introduce un numero entre 0 y 1000000: " -- Muestra un mensaje al usuario
    input <- getLine  
    let numero = read input :: Int  -- Convierte la cadena en un número entero
    if esPrimo numero
        then putStrLn "FizzBuzz!"  -- Si es primo, muestra "FizzBuzz!"
        else putStrLn (numeroEnEspañol numero)  -- Si no es primo, muestra el número en español
