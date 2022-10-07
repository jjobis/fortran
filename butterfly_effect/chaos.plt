n=2000

do for [i=1:n]{
       splot 'chaos1.dat' every ::1::i with l lt 2 lw 1 lc 7 ,\
       'chaos2.dat' every ::1::i with l lt 1 lw 2 lc 3 
       }