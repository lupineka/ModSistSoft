--Ejercicios del 15 al 26 de la pag. 24 del libro de Rosen_c1
--Equipo Lupita Alvarado y Grecia CortesS

equiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
equiv3 func1 func2 = and [ (func1 a b c) == (func2 a b c) | a <- [True, False], b <- [True, False], c <- [True, False]]

equiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
equiv2 func1 func2 = and [ (func1 a b ) == (func2 a b ) | a <- [True, False], b <- [True, False]]

-- Home-made version of max
-- max' :: Int -> Int -> Int
-- max' x y | x >= y    = x
--          | otherwise = y
max' :: Int -> Int -> Bool
max' x y = x >= y

-- equiv2Int :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Bool
equiv2Int :: (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> Bool
equiv2Int func1 func2 = and [ (func1 a b) == (func2 a b) | a <- [2, 1, 0], b <- [2, 1, 0]]

-- equiv3Int :: (Int -> Int -> Int -> Int) -> (Int -> Int -> Int -> Int) -> Bool
equiv3Int :: (Int -> Int -> Int -> Bool) -> (Int -> Int -> Int -> Bool) -> Bool
equiv3Int func1 func2 = and [ (func1 a b c) == (func2 a b c) | a <- [2, 1, 0], b <- [2, 1, 0], c <- [2, 1, 0]]


main = do
    --Ejercicio 15: (p -> q) ->  r y p -> (q -> r) --- False
    print (equiv3 (\ p q r -> ((p <= q) <= r))(\ a b c -> (a <= (b <=c))))
    --Ejercicio 16: p -> q y ¬q -> ¬p --- True
    print (equiv2 (\ p q  -> (p <= q ))(\ p q -> (not q <= not p)))
    --Ejercicio 17: ¬p <-> q y p <-> ¬q --- True
    print (equiv2 (\ p q  -> (not p == q ))(\ p q -> p == not q))
    --Ejercicio 18: ¬(p ⊕ q) y p <-> ¬q --- True
    print (equiv2 (\ p q  -> not( p /= q ))(\ p q -> p ==  q))
    --Ejercicio 19: ¬(p <-> q) y p <-> q --- True
    print (equiv2 (\ p q  -> not(p) == not(q))(\ p q -> p ==  q))
    --Ejercicio 20: (p -> q) ∧ (p -> r) y p -> (q ∧ r) --- True
    print (equiv3 (\ p q r -> (p <= q) && (p <= r))(\ p q r -> p <= (q && r)))
    --Ejercicio 21: (p -> r) ∧ (q -> r) y (p ∨ q) -> r --- True
    print (equiv3 (\ p q r -> (p <= r) && (q <= r))(\ p q r -> (p || q) <= r))
    --Ejercicio 22: (p -> q) ∨ (p -> r) y p -> (q ∨ r) --- True
    print (equiv3 (\ p q r -> (p <= q) || (p <= r))(\ p q r -> p <= (q || r)))
    --Ejercicio 23: (p -> r) ∨ (q -> r) y (p ∧ q) -> r --- True
    print (equiv3 (\ p q r -> (p <= r) || (q <= r))(\ p q r -> (p && q) <= r))
    --Ejercicio 24: ¬p -> (q->r) y q -> (p ∨ q) --- True
    print (equiv3 (\ p q r -> not(p) <= (q <= r))(\ p q r -> q <= (p || r)))
    --Ejercicio 25: p <-> p y (p -> q) ∧ (q -> p) --- True
    print (equiv2 (\ p q -> p == q)(\ p q -> (p <= q) && (q <= p)))
    --Ejercicio 26: p <-> p y ¬p <-> ¬q --- True
    print (equiv2 (\ p q -> p == q)(\ p q -> not(p) == not(q)))
    --Ejercicio de Demostracion por Casos: max(max(a, b), c) y max(a, max(b, c))
    -- print (equiv2Int (\ a b -> max(a, b))(\ a c -> max(a, c)))
    -- print (equiv3Int (\ a b c -> max(max(a, b), c))(\ a b c -> max(a, max(b, c))))
