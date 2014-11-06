-- Máquinas de Estados Finitos
-- Maquina de estado que imprime comentarios

-- Estado 1
s1 :: [Char] -> Int -> [Char] -> [Char]
s1 cinpunt cindex cbuffer | length(cinpunt)-1 <= 0 = ""
                          | cindex > length(cinpunt)-1 = ""
                          | [cinpunt !! cindex] == "/" = (s2 cinpunt (cindex + 1) "")
                          | otherwise = (s1 cinpunt (cindex + 1) "")
-- Estado 2
s2 :: [Char] -> Int -> [Char] -> [Char]
s2 cinpunt cindex cbuffer | cindex > length(cinpunt)-1 = ""
                          | [cinpunt !! cindex] == "*" = (s3 cinpunt (cindex + 1) "")
                          | otherwise = (s1 cinpunt (cindex + 1) "")
-- Estado 3
s3 :: [Char] -> Int -> [Char] -> [Char]
s3 cinpunt cindex cbuffer | cindex > length(cinpunt)-1 = ""
                          | [cinpunt !! cindex] == "*" = (s4 cinpunt (cindex + 1) (cbuffer  ++ [cinpunt !! cindex]) )
                          | otherwise = (s3 cinpunt (cindex + 1) (cbuffer ++ [cinpunt !! cindex]) )
-- Estado 4
s4 :: [Char] -> Int -> [Char] -> [Char]
s4 cinpunt cindex cbuffer | cindex > length(cinpunt)-1 = ""
                          | [cinpunt !! cindex] == "*" = (s4 cinpunt (cindex + 1) (cbuffer ++ [cinpunt !! cindex]) )
                          | [cinpunt !! cindex] == "/" = ((init cbuffer) ++ (s1 cinpunt (cindex + 1) ""))
                          | otherwise = (s3 cinpunt (cindex + 1) (cbuffer ++ [cinpunt !! cindex]) )

main = do
    print ("Prueba  1 []: ", (s1 "" 0 "" ))
    let comment = "/"
    print ("Prueba  2 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/*"
    print ("Prueba  3 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/**"
    print ("Prueba  4 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/**/"
    print ("Prueba  5 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "sin comentarios"
    print ("Prueba  6 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/*con comentarios*/"
    print ("Prueba  7 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/solo texto/"
    print ("Prueba  8 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/*solo texto"
    print ("Prueba  9 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/*solo texto**"
    print ("Prueba 10 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/*un comentario /* mas comentarios*/"
    print ("Prueba 11 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/*un comentario*/ solo texto /*otro comentario*/"
    print ("Prueba 12 [", comment, "]: ", (s1 comment 0 ""))
    let comment = "/***un comentario***/ solo texto /***otro comentario***/"
    print ("Prueba 13 [", comment, "]: ", (s1 comment 0 ""))
