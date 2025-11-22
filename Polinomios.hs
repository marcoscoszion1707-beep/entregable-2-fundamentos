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
        
    };
}

--2)
redPol :: Polinomio -> Polinomio
redPol = undefined	

--3)
sumPol :: Polinomio -> Polinomio -> Polinomio
sumPol = undefined

--4)
mulPol :: Polinomio -> Polinomio -> Polinomio
mulPol = undefined 

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
