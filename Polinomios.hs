--Marcos Coszion 332945, Francisco Lino 347691 
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module Polinomios where

type Monomio = (Int, Int)
type Polinomio = [Monomio]

-- ======================
-- POLINOMIOS
-- ======================

--1)
agregarMon :: Monomio -> Polinomio -> Polinomio
agregarMon =   /m p -> case m of{
    (0,e) -> p;
    (c,e) -> case p of {
        [] -> [(c,e)];
        (c1,e1) : ps -> case (e == e1) of{
            True -> case (c + c1 == 0) of{
                True -> ps;
                False -> (c + c1, e) : ps
            };
            False -> case (e > e1) of{
                True -> (c,e) : (c1,e1) : ps;
                False -> (c1,e1) : agregarMon (c,e) ps
            }
        }
    }
}

--2)
redPol :: Polinomio -> Polinomio
redPol = \p -> case p of{
    [] -> [];
    m:ps -> agregarMon m (redPol ps)
}

--3)
sumPol :: Polinomio -> Polinomio -> Polinomio
sumPol = \p1 p2 -> case p1 of{
    [] -> p2;
    (c1,e1) : ps1 -> case p2 of{
        [] -> p1;
        (c2,e2) : ps2 -> case (e1 == e2) of{
            True -> case (c1 + c2 == 0) of{
                True -> sumaPol ps1 ps2;
                False -> (c1 + c2, e1) : sumaPol ps1 ps2
            };
            False -> case (e1 > e2) of{
                True -> (c1,e1) : sumaPol ps1 p2;
                False -> (c2,e2) : sumaPol p1 ps2
            }
        }
    }
}

--4)
mulMon :: Monomio -> Monomio -> Monomio
mulMon =

mulMonPol :: Monomio -> Polinomio -> Polinomio
mulMonPol = \

mulPol :: Polinomio -> Polinomio -> Polinomio
mulPol = 

--5)
derPol :: Polinomio -> Polinomio
derPol = undefined

--6)
evalPol :: Polinomio -> Int -> Int
evalPol = undefined

--7)
gradoPol::Polinomio -> Int
gradoPol = undefined
																	
																	
-- ======================
-- SHOW
-- ======================

--8)
showMon :: Monomio -> String
showMon = undefined

--9)
showPol :: Polinomio -> String
showPol = undefined  
