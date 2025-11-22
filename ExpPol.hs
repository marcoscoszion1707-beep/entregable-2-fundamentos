--Marcos Coszion 332945, Francisco Lino 347691 
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module ExpPol where

import Polinomios

-- ========================
-- EXPRESIONES POLINOMICAS
-- ========================

data ExPol where 
		Pol  :: Polinomio -> ExPol 
		Der  :: ExPol -> ExPol 
		Eval :: ExPol -> Int -> ExPol 
		Sum  :: [ExPol] -> ExPol 
		Prod :: [ExPol] -> ExPol 
				deriving Show

																	
--10) 
cantPol :: ExPol -> Int
cantPol = \exp -> case exp of {
	Pol p -> 1;

	Der e -> cantPol e;

	 Eval e n -> cantPol e;

	 sum lista -> case lista of{
		[] -> 0;
		e:es -> cantPol e + cantPol (Sum es)
	 }

	 Prod lista -> case lista of{
		[] -> 0;
		e:es -> cantPol e + cantPol (Prod es)
	 }

--11)
cantx :: ExPol -> Int
cantx  =  \exp -> case exp of {

	Pol p -> cantEnPol p;

    Der e -> cantx e;

    Eval e n -> cantx e;

	sum lista -> case lista of{
		
		[] -> 0;
		e:es -> cantx e + cantx (Sum es)
	};

	prod lista -> case lista of{

		[] -> 0;
		e:es -> cantx e + cantx (Prod es)
	}
}

cantEnPol :: Polinomio -> Int
cantEnPol = \p -> case p of{

	[] -> 0;

	(c,e):ps -> case (e == 0) of{
		True -> cantEnPol ps;
		False -> 1 + cantEnPol ps
	}
}

--12)
maxProd :: ExPol -> Int
maxProd  = \exp -> case exp of {

	Pol p -> 0;

	Der e -> maxProd e;

	Eval e n -> maxProd e;

	sum lista -> case lista of{

		[] -> 0;

		e:es -> case (maxProd e > maxProd (Sum es)) of{

			True -> maxProd e;

			False -> maxProd (Sum es)
		} 
	};

	prod lista -> case lista of{

		[] -> 0;

		e:es -> case (length (e:es) > mayorSubProd e es) of{

			True -> length (e:es);

			False -> mayorSubProd e es 
		}
	  }
	}

	mayorSubProd :: ExPol -> [ExPol] -> Int
	mayorSubProd = \e es -> case es of {

		[] -> maxProd e;

		x:xs -> case (maxProd e > mayorSubProd x xs) of{

			True -> maxProd e;

			False -> mayorSubProd x xs
		}
	}


--13)
gradoPol :: Polinomio -> Int
gradoPol = \p -> case p of {

	[] -> 0;

	(c,e):ps -> case (e > gradoPol ps) of{

		True -> e;

		False -> gradoPol ps
	}
}
	gradoEP :: ExPol -> Int
	gradoEP = \exp -> case exp of {

	Pol p -> gradoPol p;

	Der e -> gradoEP e;

	Eval e n -> gradoEP e;

	sum lista -> case lista of{

		[] -> 0;

		e:es -> case (gradoEP e > gradoEP (Sum es)) of{

			True -> gradoEP e;

			False -> gradoEP (Sum es)
		}
	};

	Prod lista -> case lista of{

		[] -> 0;

		e:es -> case(gradoEP e > gradoEP (Prod es)) of{

			True -> gradoEP e;

			False -> gradoEP (Prod es)
		 }
	   }
	}
	


--14)	
calcEP :: ExPol -> Polinomio
calcEP =  \exp -> case exp of {
		Pol p -> redPol p;

		Der e -> derPol (calcEP e);

		case evalPol (calcEP e) n of{

			0 -> [];

			valor -> [(valor,0)]
		};

		sum lista -> case lista of{

			[] -> [];

			e:es ->
		}
} --falta terminar





--15)
resultado :: ExPol -> String
resultado = undefined

