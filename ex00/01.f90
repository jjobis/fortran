!ex01
       integer::x
       real::y
       do i=1,50,1
              x=i
              y=sqrt(3.0*x**3-6.0*x**2+x+2.0)
              write(*,10)y
       end do
10     format('y=',f10.4)
       end