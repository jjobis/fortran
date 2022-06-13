!2의 배수 3의 배수 6의 배수 분류 코드
       integer::r
       open(1,file='10000.dat',status='old')
       open(11,file='6baesu.dat')
       open(12,file='2baesu.dat')
       open(13,file='3baesu.dat')
       do i=1,10000,1
              read(1,10)r
              if(mod(r,6)==0)then
                     write(11,10)r
              else if(mod(r,2)==0)then
                     write(12,10)r
              else if(mod(r,3)==0)then
                     write(13,10)r
              end if
       end do
10     format(i5)
       end

                     
       