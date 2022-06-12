!subroutine 1
       integer::a,b,c
       read(*,*)a,b
       call sum_routine(a,b,c)
       write(*,*)c
       
       end 
       
       subroutine sum_routine(a,b,c)
       integer a,b,c
       c=a+b
       end