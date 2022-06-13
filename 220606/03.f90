!iajdiwawd
        integer,dimension(100)::a
        open(1,file='03.dat',status='old')
        open(11,file='04.dat')
        open(12,file='05.dat')
        do i=1,100,1
        read(1,10)a(i)
        end do
        do i=1,100,1
            if(mod(a(i),2)==0)then
                write(11,10)a(i)
            else
                write(12,10)a(i)
            end if
        end do
10      format(i3)
        end