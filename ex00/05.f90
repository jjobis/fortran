!
       integer::a
       open(1,file='05.dat')
       do a=1,500,1
              if(a>100 .and. mod(a,2)==0)write(1,10)a
       end do
10     format(i5)
       end