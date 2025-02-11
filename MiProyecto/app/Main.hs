
-- Función principal que implementa FizzBuzz

fizzBuzz :: Int -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz!" -- Si el número es múltiplo de 3 y 5, devuelve "FizzBuzz!"
    | n `mod` 5 == 0  = "Fizz!"     -- Si el número es múltiplo de 5, devuelve "Fizz!"
    | n `mod` 3 == 0  = "Buzz!"     -- Si el número es múltiplo de 3, devuelve "Buzz!"
    | otherwise       = numeroAPalabra n -- Si no es múltiplo de ninguno, devuelve el número en palabras.

-- Lista con los nombres de los dígitos del 0 al 9

digitos :: [String]
digitos = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

-- Lista con los nombres de los números del 10 al 19

masDiez :: [String]
masDiez = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
          "sixteen", "seventeen", "eighteen", "nineteen"]

-- Lista con los nombres de las decenas (20, 30, ..., 90)

decenas :: [String]
decenas = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- Función que convierte un número en su representación en palabras

numeroAPalabra :: Int -> String
numeroAPalabra n
    | n < 10    = digitos !! n  -- Si el número es menor a 10, usa la lista de dígitos

    | n < 20    = masDiez !! (n - 10) -- Si está entre 10 y 19, usa la lista de números especiales
    
    | n < 100   = let (t, u) = n `divMod` 10  -- Divide el número en decena y unidad
                  in decenas !! (t - 2) ++ (if u /= 0 then " " ++ digitos !! u else "")
                  -- Si hay unidades (u ≠ 0), las concatena con la decena
    | otherwise = "Numero fuera de rango" -- Maneja el caso de números mayores a 99

-- Función principal que ejecuta el programa

main :: IO ()
main = do
    putStrLn "Introduce un numero entre 0 y 100: " -- Muestra un mensaje al usuario
    input <- getLine  
    let numero = read input :: Int  -- Convierte la cadena en un número entero
    putStrLn (fizzBuzz numero)  -- Llama a fizzBuzz y muestra el resultado
