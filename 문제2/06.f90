!
       integer,dimension(13)::a
       integer::b

       do i=1,13,1
              a(i)=i
       end do
       b=1
       write(*,10)b
       do i=1,13,1
             b=a(i)*b
             write(*,10)b
       end do
10     format(i20)
       end