-- Clase 7.
-- Definición de lenguajes

-- Definición de números Naturales
data Natural = Z | S Natural
    deriving (Eq, Show)

-- Suma de números Naturales
m `plus` Z = m
m `plus` (S n) = S (m `plus` n)

-- Multiplicación de números Naturales
m `mult` Z = Z
m `mult` (S n) = (m `mult` n) `plus` m

-- Exponenciación de números Naturales
expn m Z = (S Z)
expn m (S n) = (expn m n) `mult` m


main = do
    print ("Suma de 2 y 3: ", (S (S Z)) `plus` (S (S (S Z))) )
    print ("Multiplicacion de 2 y 3: ", (S (S Z)) `mult` (S (S (S Z))) )
    print ("Exponente de 2 a la 3: ", expn (S (S Z)) (S (S (S Z))) )
    print ('\n')
    print ("Suma de 2 y 5: ", (S (S Z)) `plus` (S (S (S (S (S Z))))) )
    print ("Multiplicacion de 2 y 5: ", (S (S Z)) `mult` (S (S (S (S (S Z))))) )
    print ("Exponente de 2 a la 5: ", expn (S (S Z)) (S (S (S (S (S Z))))) )
