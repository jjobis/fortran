!read test dimension

       integer::g,h
       integer,dimension(g,h)::a
       integer,dimension(g*h)::b

       write(*,*)'enter dimension 2 array g and h (g,h)'
       read(*,*)g,h

       do i=1,g
              do j=1,h
              a(i,j)=i*j
              end do
       end do
       
       do i=1,g
       write(*,10)(a(g,i),i=1,h)
10     format(hi5)
       end do
       end