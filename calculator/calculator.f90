!calculator.f90 v0.01
!v0.01 +,-,*,/
!v0.02 *,/ first
!v0.03 menu
!v0.03.2 error with four fundamental rules of arithmetics : error code : Program received signal SIGSEGV: Segmentation fault - invalid memory reference.
!v0.04 linear equation, quadratic equation
!v0.05 menu + 

       integer::mod_num
       integer::con_cal
11     write(*,*)'four fundamental rules of arithmetics -> 1'
       write(*,*)'                      linear equation -> 2'
       write(*,*)'                   quadratic equation -> 3'
       write(*,*)'select mod'
       read(*,*)mod_num
       if(mod_num==1) call four_fundamental_rules_of_arithmetics
       if(mod_num==2) call linear_equation
       if(mod_num==3) call quadratic_equation
       write(*,*)'continue calculating? Y->1 N->2'
       read(*,*)con_cal
       if (con_cal==1) goto 11
       end
       
       subroutine four_fundamental_rules_of_arithmetics
       integer::num_count
       write(*,*)'---------------four_fundamental_rules_of_arithmetics---------------'
       write(*,*)'write the number of numbers in the equation'
       read(*,*)num_count
       call four_math_cal_read(num_count)
       end

       subroutine check_code_dimensions(a,math_code,four_cal)
              integer::a,b
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              write(*,*)'dimension math code'
              do i=1,a-1
              write(*,*)math_code(i)
              end do
              write(*,*)'dimension cal code'
              do i=1,a
              write(*,*)four_cal(i)
              end do
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
              call four_math_cal(a,math_code,four_cal,four_ans)
       end

       subroutine if_mul_div(a,math_code,four_cal,four_ans)
              integer::a,b
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              if(a/=1)goto 7
              if(a==1)goto 8
7             continue
              do i=1,a-1
                     b=i
                     if(math_code(i)==3)then
                     call multiply(a,math_code,four_cal,four_ans,b)
                     else if(math_code(i)==4)then
                     call division(a,math_code,four_cal,four_ans,b)
                     end if
              end do
8             continue
              if(math_code(1)==3)then
                     call multiply(a,math_code,four_cal,four_ans,b)
              else if(math_code(1)==4)then
                     call division(a,math_code,four_cal,four_ans,b)
              end if
       end

       subroutine if_plu_min(a,math_code,four_cal,four_ans)
              integer::a,b
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              if(a/=1)goto 7
              if(a==1)goto 8
7             continue
              do i=1,a-1
                     b=i
                     if(math_code(i)==1)then
                     call plus(a,math_code,four_cal,four_ans,b)
                     else if(math_code(i)==2)then
                     call minus(a,math_code,four_cal,four_ans,b)
                     end if
              end do
8             continue
              b=1
              if(math_code(1)==1)then
                     call plus(a,math_code,four_cal,four_ans,b)
              else if(math_code(1)==2)then
                     call minus(a,math_code,four_cal,four_ans,b)
              end if
       end

       subroutine four_math_cal(a,math_code,four_cal,four_ans)
              integer::a,b
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              call if_mul_div(a,math_code,four_cal,four_ans)
              call if_plu_min(a,math_code,four_cal,four_ans)
              write(*,*)'ans=',four_ans
       end

       subroutine multiply(a,math_code,four_cal,four_ans,b)
              integer::a,b,c
              integer,dimension(a-1)::math_code
              real,dimension(a)::four_cal
              real::four_ans
              four_ans=four_cal(b)*four_cal(b+1)
              four_cal(b)=four_ans
              if(b/=a)goto 7
              if(b==a)goto 8
7             do i=b,a-1
              if(b/=a)then   
                     math_code(i)=math_code(i+1)
              end if
              end do
              goto 9
8             math_code(1)=math_code(2)
9             do i=b+1,a
              if(b/=a-1)then   
                     four_cal(i)=four_cal(i+1)
              end if
              end do
              do i=1,a-1
                     if(math_code(i)==3  .or. math_code(i)==4) then
                     a=a-1
                     call if_mul_div(a,math_code,four_cal,four_ans)
                     end if
              end do
              do i=1,a-1
                     if(math_code(i)==1 .or. math_code(i)==2) then
                     a=a-1
                     call if_plu_min(a,math_code,four_cal,four_ans)
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
              if(b/=a)goto 7
              if(b==a)goto 8
7             do i=b,a-1
              if(b/=a)then   
                     math_code(i)=math_code(i+1)
              end if
              end do
              goto 9
8             math_code(1)=math_code(2)
9             do i=b+1,a
              if(b/=a)then   
                     four_cal(i)=four_cal(i+1)
              end if
              end do
              do i=1,a-1
                     if(math_code(i)==3  .or. math_code(i)==4) then
                     a=a-1
                     call if_mul_div(a,math_code,four_cal,four_ans)
                     end if
              end do
              do i=1,a-1
                     if(math_code(i)==1 .or. math_code(i)==2) then
                     a=a-1
                     call if_plu_min(a,math_code,four_cal,four_ans)
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
              if(b/=a)goto 7
              if(b==a)goto 8
7             do i=b,a-1
              if(b/=a)then   
                     math_code(i)=math_code(i+1)
              end if
              end do
              goto 9
8             math_code(1)=math_code(2)
9             do i=b+1,a
              if(b/=a)then   
                     four_cal(i)=four_cal(i+1)
              end if
              end do
              do i=1,a-1
                     if(math_code(i)==1 .or. math_code(i)==2) then
                     a=a-1
                     call if_plu_min(a,math_code,four_cal,four_ans)
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
              if(b/=a)goto 7
              if(b==a)goto 8
7             do i=b,a-1
              if(b/=a)then   
                     math_code(i)=math_code(i+1)
              end if
              end do
              goto 9
8             math_code(1)=math_code(2)
9             do i=b+1,a
              if(b/=a-1)then   
                     four_cal(i)=four_cal(i+1)
              end if
              end do
              do i=1,a-1
                     if(math_code(i)==1 .or. math_code(i)==2) then
                     a=a-1
                     call if_plu_min(a,math_code,four_cal,four_ans)
                     end if
              end do
       end

       subroutine linear_equation
              integer::a,b
              real::c
              write(*,*)'---------------linear_equation---------------'
              write(*,*)'  ax+b=0   write a,b'
              read(*,*)a,b
              c=-b/(a*1.0)
              write(*,*)'ans=',c            
       end

       subroutine quadratic_equation
              integer::a,b,c
              real,dimension(2)::d
              write(*,*)'---------------quadratic_equation---------------'
              write(*,*)' ax^2+bx+c  write a,b,c'
              read(*,*)a,b,c
              d(1)=-b+sqrt(b**2.0-4.0*a*c)/2.0*a
              d(2)=-b-sqrt(b**2.0-4.0*a*c)/2.0*a
              write(*,*)d(1),'and',d(2)
       end