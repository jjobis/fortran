!2,4,6배수 출력 코드
       integer,dimension(3334)::a
       integer,dimension(1667)::b
       integer,dimension(1666)::c
       open(1,file='2baesu.dat',status='old')
       open(2,file='3baesu.dat',status='old')
       open(3,file='6baesu.dat',status='old')
       open(11,file='f_baenu.dat')
       do i=1,3334,1
              read(1,10)a(i)
       end do
       do i=1,1667,1
              read(2,10)b(i)
       end do
       do i=1,1666,1
              read(3,10)c(i)
       end do
10     format(i8)
       write(11,20)
20     format(6x,'2bae',8x,'3bae',4x,'6bae'/50('-')/)
       do i=1,1666
              write(11,30)a(2*i-1),a(2*i),b(i),c(i)
30            format(4i8)
       end do
       write(11,40)a(3333),a(3334),b(1667)
40     format(3i8)
       end