
cadenaValida :: String -> Bool
cadenaValida (x:xs) = estados xs "q0" x 

estados :: String -> String -> Char -> Bool
estados (x:xs) q caracter
	|	q == "q0" && caracter == '0' && xs /= "" = estados xs "q2" x
	|	q == "q0" && (caracter >= '1' && caracter <= '9') && xs /= "" = estados xs "q1" x
	|	q == "q1" && (caracter >= '1' && caracter <= '9') && xs /= "" = estados xs "q1" x
	|	q == "q1" && caracter == '.' && xs /= ""  = estados xs "q3" x 
	|	q == "q2" && caracter == '.' && xs /= ""  = estados xs "q3" x
	|	q == "q2" && (caracter >= '0' && caracter <= '9') = False
	|	q == "q3" && (caracter >= '0' && caracter <= '9') && xs /= "" = estados xs "q4" x
	|	q == "q3" && caracter == '.' = False
	|	q == "q4" && (caracter >= '0' && caracter <= '9') && xs /= ""  = estados xs "q4" x
	|	otherwise = True
