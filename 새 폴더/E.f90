!       
       integer,dimension(365)::D
       real,dimension(365)::E,T
       real,parameter::pi=4. * atan(1.)
       open(unit=1,file='Er.dat')
       open(unit=2,file='Tr.dat')

       do i = 1, 365, 1
              D(i) = i
              E(i) = 1. + 0.033*cos((2.*pi*D(i))/365.)
              T(i) = 1.000110 + 0.034221 * cos(2.*pi*(D(i)-1)/365.) + 0.001280*sin(2.*pi*(D(i)-1)/365.) + &
              0.000719*cos(2.*(2.*pi*(D(i)-1)/365.)) + 0.000077*sin(2.*(2.*pi*(D(i)-1)/365.))
              write(1,10)D(i),E(i)
              write(2,10)D(i),T(i)
10            format(i3,3X,f15.10)
       end do

       end
