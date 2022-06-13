!구구단 출력
       integer,dimension(50)::a,b
       integer::m
       open(1,file='gugudan.dat')
       do i=1,50,1
              a(i)=i
              b(i)=i
       end do
       do i=1,50,1
              write(1,20)
20            format(50('-'))
              do j=1,50,1
              m=a(i)*b(j)
              write(1,10)a(i),b(j),m
10            format(i2,1x,'X',1x,i2,1x,'=',1x,i4)
              end do
       end do
       end