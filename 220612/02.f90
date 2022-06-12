!문제 2
       integer::x1,y1
       read(*,10)x1
       read(*,10)y1
10     format(i3)
       call mul_xy(x1,y1)
       end

       subroutine mul_xy(x1,y1)
       integer::x1,y1
       write(*,*)x1*y1
       end
       