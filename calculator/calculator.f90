!calculator.f90 v0.01
!v0.01 +,-,*,/
!v0.02 *,/ first
!v0.03 menu
!v0.04 linear equation, quadratic equation
!v0.05 menu +

       integer::a
       write(*,*)'four fundamental rules of arithmetics -> 1'
       write(*,*)'linear equation -> 2'
       write(*,*)'quadratic equation ->3'
       write(*,*)'select mod'
       if(a==1) call four_fundamental_rules_of_arithmetics
       if(a==2) call linear_equation
       if(a==3) call quadratic_equation
       end
       
       subroutine four_fundamental_rules_of_arithmetics
       integer::num_count
       write(*,*)'---------------four_fundamental_rules_of_arithmetics---------------'
       write(*,*)'write the number of numbers in the equation'
       read(*,*)num_count
       call four_math_cal_read(num_count)
       end

       subroutine four_math_cal_read(a)
              integer::a
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              write(*,*)'add number'
              read(*,*)four_cal(1)
              do i=2,a
                     write(*,*)'enter 1 if you want to + it'
                     write(*,*)'enter 2 if you want to - it'
                     write(*,*)'enter 3 if you want to x it'
                     write(*,*)'enter 4 if you want to / it'
              read(*,*)math_code(i-1)
              write(*,*)'add number'
              read(*,*)four_cal(i)
              end do
              write(*,*)(four_cal(i),i=1,a),(math_code(j),j=1,a-1)
              call four_math_cal(a,math_code,four_cal,four_ans)
       end

       subroutine four_math_cal(a,math_code,four_cal,four_ans)
              integer::a,b
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
7             do i=1,a-1
                     b=i
                     if(math_code(i)==3)then
                     call multiply(a,math_code,four_cal,four_ans,b)
                     else if(math_code(i)==4)then
                     call division(a,math_code,four_cal,four_ans,b)
                     else if(math_code(i)==1)then
                     call plus(a,math_code,four_cal,four_ans,b)
                     else
                     call minus(a,math_code,four_cal,four_ans,b)
                     end if
                     a=a-1
                     write(*,*)four_ans
                     go to 7
              end do
              write(*,*)'ans=',four_cal(1)
       end

       subroutine multiply(a,math_code,four_cal,four_ans,b)
              integer::a,b,c
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              four_ans=four_cal(b)*four_cal(b+1)
              four_cal(b)=four_ans
              do i=b,a-1
              if(b/=a-1)then   
                     math_code(i)=math_code(i+1)
              end if
              end do
              do i=b+1,a
              if(b/=a-1)then   
                     four_cal(i)=four_cal(i+1)
              end if
              end do
       end

       subroutine division(a,math_code,four_cal,four_ans,b)
              integer::a,b
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              four_ans=four_cal(b)/four_cal(b+1)
              four_cal(b)=four_ans
              do i=b,a-1
              if(b/=a-1)then   
                     math_code(i)=math_code(i+1)
              end if
              end do
              do i=b+1,a
              if(b/=a-1)then   
                     four_cal(i)=four_cal(i+1)
              end if
              end do
       end

       subroutine plus(a,math_code,four_cal,four_ans,b)
              integer::a,b
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              four_ans=four_cal(b)+four_cal(b+1)
              four_cal(b)=four_ans
              do i=b,a-1
              if(b/=a-1)then   
                     math_code(i)=math_code(i+1)
              end if
              end do
              do i=b+1,a
              if(b/=a-1)then   
                     four_cal(i)=four_cal(i+1)
              end if
              end do
       end

       subroutine minus(a,math_code,four_cal,four_ans,b)
              integer::a,b
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              four_ans=four_cal(b)-four_cal(b+1)
              four_cal(b)=four_ans
              do i=b,a-1
              if(b/=a-1)then   
                     math_code(i)=math_code(i+1)
              end if
              end do
              do i=b+1,a
              if(b/=a-1)then   
                     four_cal(i)=four_cal(i+1)
              end if
              end do
       end

       subroutine linear_equation
              write(*,*)'---------------linear_equation---------------'
       end

       subroutine quadratic_equation
              write(*,*)'---------------quadratic_equation---------------'
       end