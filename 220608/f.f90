! 10000.f90 , 02,f90 , 03.f90 코드 합침 close 활용법

       integer::r
       integer,dimension(3334)::a
       integer,dimension(1667)::b
       integer,dimension(1666)::c
       open(1,file='10000.dat')
       do i=1,10000
              write(1,11)i
11            format(i5)
       end do
       close(1)
       open(1,file='10000.dat',status='old')
       open(11,file='6baesu.dat')
       open(12,file='2baesu.dat')
       open(13,file='3baesu.dat')
       open(14,file='ff_baenu.dat')
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
       close(11)
       close(12)
       close(13)
       open(11,file='2baesu.dat',status='old')
       open(12,file='3baesu.dat',status='old')
       open(13,file='6baesu.dat',status='old')
       do i=1,3334,1
              read(11,20)a(i)
       end do
       do i=1,1667,1
              read(12,20)b(i)
       end do
       do i=1,1666,1
              read(13,20)c(i)
       end do
20     format(i8)
       write(14,30)
30     format(6x,'2bae',8x,'3bae',4x,'6bae'/50('-')/)
       do i=1,1666
              write(14,40)a(2*i-1),a(2*i),b(i),c(i)
40            format(4i8)
       end do
       write(14,50)a(3333),a(3334),b(1667)
50     format(3i8)
       end