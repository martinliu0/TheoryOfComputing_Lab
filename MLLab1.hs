{-(1)-}
minList [a] = a
minList (start:rest) = if (start > minList(rest))
						then minList rest
						else start
						
listSort [a] = [a]
listSort (start: rest) = if (start > minList(rest))
						  then listSort (rest ++ (start:[]))
						  else (start: listSort (rest))						

						 
inS a [] = False
inS a (start:rest) = if (a == start)
					  then True
					  else inS a rest

norep [] = []
norep (start:rest) = if (inS start rest)
					  then norep rest
					  else start: norep rest
					  
fixS (start:rest) = listSort (norep (start:rest))


addtoSet a (start:rest) = fixS ((start:rest) ++ (a:[]))
							
unionS list1 list2 = fixS(list1 ++ list2)

interS list [] = []
interS [] list = []
interS list1 (start:rest) = if (inS start list1)
							  then (start: (interS list1 rest))
							  else interS list1 rest
		
setEq [] list = False
setEq list [] = False
setEq (start0:rest0) (start1:rest1) = if (start0 == start1)
										then setEq rest0 rest1
										else False


{-2-}
data Nat = Zero | S Nat deriving (Read, Show, Eq)

nattoInt Zero = 0
nattoInt (S(n)) = 1 + nattoInt n 

buildNat 0 = Zero
buildNat i = S(buildNat (i-1))

add Zero (S(z)) = S(z)
add (S(n)) Zero = S(n)
add (S(n)) (S(z)) = add n (S(S(z)))

minus Zero (S(z)) = S(z)
minus (S(n)) Zero = S(n)
minus (S(n)) (S(z)) = minus n z

mult Zero z = Zero
mult (S(n)) z = add z (mult n z)



lt Zero (S(z)) = True
lt (S(n)) Zero = False
lt Zero Zero = False
lt (S(n)) (S(z)) = lt n z

lte Zero (S(z)) = True
lte (S(n)) Zero = False
lte Zero Zero = True
lte (S(n)) (S(z)) = lte n z


{-4-}
factNat (S(Zero)) = (S(Zero))
factNat (S(n)) = mult (S(n)) (factNat n)

{-

Results for factNat(S(S(S(Zero)))):
S (S (S (S (S (S Zero)))))

Results for nattoInt(factNat(buildNat 5)):
120

-}
