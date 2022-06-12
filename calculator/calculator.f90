!calculator.f90 v0.01
!v0.01 +,-,*,/

       integer::math_code
       real::four_cal
       real::four_ans
       
       write(*,*)'add number'
       read(*,*)four_cal
              write(*,*)'enter 1 if you want to + it'
              write(*,*)'enter 2 if you want to - it'
              write(*,*)'enter 3 if you want to x it'
              write(*,*)'enter 4 if you want to / it'
              read(*,*)math_code
              if(math_code==1)then
                     call plus(four_cal,four_ans)
              else if(math_code==2)then
                     call minus(four_cal,four_ans)
              else if(math_code==3)then
                     call multiply(four_cal,four_ans)
              else if(math_code==4)then
                     call division(four_cal,four_ans)
              end if
       write(*,10)four_ans
10     format(f10.4)
       end 

       subroutine plus(a,c)
              real::a,b,c
              write(*,*)'add number'
              read(*,*)b
              c=a+b
       end subroutine plus

       subroutine minus(a,c)
              real::a,b,c
              write(*,*)'add number'
              read(*,*)b
              c=a-b
       end subroutine minus

       subroutine multiply(a,c)
              real::a,b,c
              write(*,*)'add number'
              read(*,*)b
              c=a*b
       end subroutine multiply

       subroutine division(a,c)
              real::a,b,c
              write(*,*)'add number'
              read(*,*)b
              c=a/b
       end subroutine division

       