-- Ejercicios de Conjuntos Potencia
-- Equipo: Lupita Alvarado y Grecia Cortés
import Data.List

-- Cálculo de Conjuntos Potencia
cpotencia :: [a] -> [[a]]
cpotencia [] = [[]]
cpotencia (x:xs) = let cpotencia_xs = cpotencia xs
                   in cpotencia_xs++[(x:z) | z <- cpotencia_xs]

main = do
    let conjunto = [1..5]
    let lConjunto = length conjunto
    let conPotencia = cpotencia conjunto
    let lConPotencia = length conPotencia
    print ("Combinaciones esperadas: ", 2^lConjunto)
    print ("Combinaciones: ", conPotencia)
    print ("Total de combinaciones: ", lConPotencia)
