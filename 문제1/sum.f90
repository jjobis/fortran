! 원하는 숫자를 read한 후 read.dat파일에 저장, 그 후 read.dat파일을 read후 문제 1 구하기 [close 활용]
       integer::r
       integer,dimension(6,4)::a
       integer,dimension(6)::L
       integer,dimension(4)::M
       open(1,file='read.dat')
       write(*,*)'enter a number from 1 to 1000, 24 times'
       do i=1,24,1
              read(*,10)r
              write(1,10)r
       end do
10     format(i3)
       close(1)
       open(1,file='read.dat',status='old')
       open(2,file='ans.dat')
       do i=1,4,1
              do j=1,6,1
                     read(1,15)a(j,i)
              end do
       end do
15     format(i3)
       do i=1,6,1
              L(i)=0
              do j=1,4,1
                     L(i)=L(i)+a(i,j)
              end do
       end do
       do i=1,4,1
              M(i)=0 
              do j=1,6,1
                     M(i)=M(i)+a(j,i)
              end do
       end do
       do i=1,5,1
       write(2,20)(a(i,j),j=1,4,1),L(i)
20     format(3(i3,1x,'+',1x),i3,1x,'=',1x,i5/4(1x,'+',4x))
       end do
       write(2,25)(a(6,j),j=1,4,1),L(6)
25     format(3(i3,1x,'+',1x),i3,1x,'=',1x,i5/4(1x,'=',4x))
       write(2,30)(M(i),i=1,4,1)
30     format(4(i4,2x))
       end