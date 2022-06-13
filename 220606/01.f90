!read 03.dat
        open(1,file='03.dat')
        do i=1,100,1
        write(1,10)i
10      format(i3)
        end do
        end