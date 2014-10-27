-- Ejercicios de Conjuntos Potencia
-- Equipo: Lupita Alvarado y Grecia Cortés

subsets [] = [[]]
 
subsets (x:xs) = let subsets_xs = subsets xs
              in subsets_xs++[(x:z) | z <- subsets_xs]

main= print (subsets [1..3])