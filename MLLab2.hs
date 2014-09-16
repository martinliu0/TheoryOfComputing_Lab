{-1-}
addA [] = []
addA (head:rest) =  ["a"++head] ++ (addA rest) 

astar = "" : (addA astar)

{-2-}
addAB [] = []
addAB (head:rest) =  [head++"a"]++ [head++"b"]++(addAB rest)

abstar = "": (addAB abstar)

{-3-}
comp f g  = g.f  

{-4-}
fsmspec1 'a' 0 = 1
fsmspec1 'b' 0 = 0
fsmspec1 'a' 1 = 1
fsmspec1 'b' 1 = 2
fsmspec1 'a' 2 = 0
fsmspec1 'b' 2 = 1 

{-5-}
fsm1 "a"= fsmspec1 'a'
fsm1 "b"= fsmspec1 'b'
fsm1 (head:rest)  = comp (fsmspec1 head) (fsm1 rest)

{-6-}
mm1 string = fsm1 string 0

{-7-}
fsmspec2 'a' 0 = 1
fsmspec2 'b' 0 = 0
fsmspec2 'a' 1 = 2
fsmspec2 'b' 1 = 1
fsmspec2 'a' 2 = 3
fsmspec2 'b' 2 = 2
fsmspec2 'a' 3 = 0
fsmspec2 'b' 3 = 3

fsm2 "a" = fsmspec2 'a'
fsm2 "b" = fsmspec2 'b'
fsm2 (head:rest) = comp (fsmspec2 head) (fsm2 rest)
mm2 (head:rest) = fsm2 (head:rest) 0
