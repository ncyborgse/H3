{- Test for Program -}

import Program

main :: IO ()
main = do

    --putStr (toString p)

    print (Program.exec p [3,16])

    print (Program.exec p1 [1024, 2])

    --putStr (toString p2)
    print( Program.exec p2 [5])

    --print rp
    --print rp1


p, p1, p2 :: Program.T
p = fromString  ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

p1 = fromString  ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")

p2 = fromString  ("\
\read n;\
\s := 0;\
\repeat\
\  begin\
\    s := s + n;\
\    n := n - 1;\
\  end\ 
\until (0-n)+1;\
\write s;")


