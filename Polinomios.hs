--Marcos Coszion 332945, Francisco Lino 347691 
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module Polinomios where

type Monomio = (Int, Int)
type Polinomio = [Monomio]

-- ======================
-- POLINOMIOS
-- ======================

--1
agregarMon :: Monomio -> Polinomio -> Polinomio
agregarMon =   \m p -> case m of
    (0,e) -> p
    (c,e) -> case p of 
        [] -> [(c,e)]
        (c1,e1) : ps -> case (e == e1) of
            True -> case (c + c1 == 0) of
                True -> ps
                False -> (c + c1, e) : ps
            
            False -> case (e > e1) of
                True -> (c,e) : (c1,e1) : ps
                False -> (c1,e1) : agregarMon (c,e) ps
            
        
    


--2
redPol :: Polinomio -> Polinomio
redPol = \p -> case p of
    [] -> []
    m:ps -> agregarMon m (redPol ps)


--3
sumPol :: Polinomio -> Polinomio -> Polinomio
sumPol = \p1 p2 -> case p1 of
    [] -> p2
    (c1,e1) : ps1 -> case p2 of
        [] -> p1
        (c2,e2) : ps2 -> case (e1 == e2) of
            True -> case (c1 + c2 == 0) of
                True -> sumPol ps1 ps2
                False -> (c1 + c2, e1) : sumPol ps1 ps2
            
            False -> case (e1 > e2) of
                True -> (c1,e1) : sumPol ps1 p2
                False -> (c2,e2) : sumPol p1 ps2
            
        
    


--4
mulMon :: Monomio -> Monomio -> Monomio
mulMon = \m1 m2 -> case m1 of
    (c1,e1) -> case m2 of
        (c2,e2) -> (c1 * c2, e1 + e2)
    


mulMonPol :: Monomio -> Polinomio -> Polinomio
mulMonPol = \m p -> case p of
    [] -> []
    m1 : ps -> agregarMon (mulMon m m1) (mulMonPol m ps)


mulPol :: Polinomio -> Polinomio -> Polinomio
mulPol = \p1 p2 -> case p1 of
    [] -> []
    m1 : ps1 -> sumPol (mulMonPol m1 p2) (mulPol ps1 p2)


-- 5
derPol :: Polinomio -> Polinomio
derPol = \p -> case p of 

    [] -> []

    (c,e) : ps ->
        case (e == 0) of 

            True -> derPol ps   

            False ->
                case (c * e == 0) of  

                    True -> derPol ps

                    False -> (c * e, e - 1) : derPol ps
                
        




--6
evalPol :: Polinomio -> Int -> Int
evalPol = \p i -> case p of
    [] -> 0
    (c,e) : ps -> c * (i ^ e) + evalPol ps i


--7
gradoPol :: Polinomio -> Int
gradoPol = \p -> case p of 

    [] -> 0

    (c,e) : ps ->
        case (c == 0) of 

            True -> gradoPol ps  
            False -> e
        


																	
																	
-- ======================
-- SHOW
-- ======================

--8
showMon :: Monomio -> String
showMon = \m -> case m of
    (c,e) -> case(c == 0) of
        True -> ""
        False -> case (e == 0) of
            True -> show c
            False -> case (e == 1) of
                True -> case(c == 1) of
                    True -> "x"
                    False -> case (c == -1) of
                        True -> "-x"
                        False -> show c ++ "x"
                    
                
                False -> case (c == 1) of
                    True -> "x^" ++ show e
                    False -> case (c == -1) of
                        True -> "-x^" ++ show e
                        False -> show c ++ "x^" ++ show e
                    
                
            
    
    


--9
showPol :: Polinomio -> String
showPol = \p -> case p of 

    [] -> ""

    (c,e):ps ->
        case (c == 0) of 

            True -> showPol ps

            False -> showMon (c,e) ++ showResto ps
        


showResto :: Polinomio -> String
showResto = \p -> case p of 

    [] -> ""

    (c,e):ps ->
        case (c == 0) of 

            True -> showResto ps

            False ->
                case (c > 0) of 
                    True  -> "+" ++ showMon (c,e) ++ showResto ps
                    False -> showMon (c,e) ++ showResto ps
                
        



showRestoNeg :: Polinomio -> String
showRestoNeg = \p -> case p of
    [] -> ""

    (c,e) : ps -> case (c==0) of

        True -> showRestoNeg ps
        
        False -> case (c > 0) of

            True -> "+" ++ showMon (c,e) ++ showResto ps

            False -> showMon (c,e) ++ showRestoNeg ps
        
    




