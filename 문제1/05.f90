!문제 1) num.dat 파일에 24개의 3자리 숫자를 (6,4)배열의 가로 방향으로 읽은 후 가로로 더한 값을 L(), 세로로 더한값을 M()으로 두고 ans.dat에 주어진 형식처럼 출력하라.
       integer,dimension(6,4)::a
       integer,dimension(6)::L
       integer,dimension(4)::M
       open(1,file='num.dat',status='old')
       open(2,file='ans.dat')
       do i=1,4,1
              do j=1,6,1
                     read(1,10)a(j,i)
              end do
       end do
10     format(i3)
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