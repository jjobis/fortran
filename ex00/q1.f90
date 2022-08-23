!
       real::x,y
       read(*,*)x
       if(x>0)then
       y=sqrt(x)
       else if(x<0)then
       y=x**2
       else
       go to 7
!      write(*,*)y
       end if
       write(*,*)y
7      continue
       end