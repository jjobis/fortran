!               
        integer::a,b,c,d
        open(1,file='ex.dat',status='old')
        read(1,10)a
10      format(i5)
        write(*,*)('enter a number from 1 to 100')
        do i=1,10,1
        read(*,10)b
        if(b>a)then
            write(*,*)('down')
        else if(b<a)then
            write(*,*)('up')
        else
            write(*,*)('correct!')
            go to 7
        end if
        end do
7       continue
        write(*,*)('gameover')
        end