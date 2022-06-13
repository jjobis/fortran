!awihjhjaw
        integer::a,b
        real::sumA,sumB
        open(1,file='04.dat',status='old')
        open(2,file='05.dat',status='old')
        open(11,file='06.dat')
        sumA=0
        sumB=0
        write(3,40)
40      format(8x,'04.dat',1x,'sumA',3x,'05.dat',1x,'sumB')
        do i=1,50,1
            read(1,10)a
            read(2,10)b
10      format(i3)
            sumA=sumA+a
            sumB=sumB+b
            write(11,15)i,a,sumA,b,sumB
15      format(i2')',3x,2(i5,2x,f7.2))
        end do
        write(*,20)sumA/50.,sumB/50.
        write(3,20)sumA/50.,sumB/50.
20      format(2f16.3)
        end
            
