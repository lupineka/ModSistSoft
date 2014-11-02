-- Clase 8.
-- Árboles binarios
-- Tipo de dato nodo para la estructura árbol binario
data BinTree = L | N BinTree BinTree deriving Show

-- Creación de un árbol binario
makeBinTree :: Integer -> BinTree
makeBinTree 0 = L
makeBinTree n = N (makeBinTree (n - 1)) (makeBinTree (n - 1))

-- Conteo de nodos en un árbol binario
count :: BinTree -> Integer
count L = 1
count (N t1 t2) = 1 + count t1 + count t2

-- Profundidad de un árbol binario
depth :: BinTree -> Integer
depth L = 0
depth (N t1 t2) = (max (depth t1) (depth t2)) + 1

-- Prueba que un árbol binario está balanceado
balanced :: BinTree -> Bool
balanced L = True
balanced (N t1 t2) = (balanced t1)
                    && (balanced t2)
                    && depth t1 == depth t2


main = do
    let nivel = 6
    let arbolBi = makeBinTree nivel
    print ("Arbol binario de 6 niveles: ", arbolBi)
    print ("Numero de nodos en un arbol de 6 niveles: ", count (arbolBi))
    print ("Prueba de numero de nodos esperados (2^7 - 1 = ", (2^(nivel + 1) - 1), "): ", count (arbolBi) == (2^(nivel + 1) - 1))
    print ("El arbol esta balanceado: ", balanced (arbolBi))
