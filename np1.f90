        REAL :: w1,w2,w3,a,b,c,ws1,ws2,ws3
        REAL, DIMENSION(365) :: d
        REAL, parameter :: pi = 4. * atan(1.)
        
        open(1, file = 'sd1.csv')
        open(2, file = 'Np4.csv')

        do i = 1, 365

        read(1, 10) d(i)
10      format(8x, f6.2)

        w1 = 30 * (pi/180.)
        w2 = 85 * (pi/180.)
        w3 = -85 * (pi/180.)

        ds = d(i) * (pi/180.)
        
         a = -tan(w1) * tan(ds)
         b = -tan(w2) * tan(ds)
         c = -tan(w3) * tan(ds)
        !write(2,*)b-floor(b)
       ! write(2,*)a,b,c
9       CONTINUE
        if(b>1) then
        b=1-(b-floor(b))
        go to 9
        else if(c>1)then
        c=1-(c-floor(c))
        go to 9
        else if(b<-1)then
        b=b-floor(b)
        go to 9
        else if(c<-1)then
        c=c-floor(c)
        go to 9
        end if
      !  write(2,*)a,b,c
        ws1 = acos(-tan(w1) * tan(ds))
        !ws2 = acos(-tan(w2) * tan(ds))
        !ws3 = acos(-tan(w3) * tan(ds))
        ws2 = acos(b)
        ws3 = acos(c)

   !     write(2,*)w1,w2,w3,ds
    !    write(2,*)a,b,c

        !ws1 = acos(a)
        !ws2 = acos(b)
        !ws3 = acos(c)

        write(2, 20) i,',',ws1,',',ws2,',',ws3,','

20      format(i3, a, f6.2, a, f6.2, a, f6.2, a)
        end do

        CALL SYSTEM('gnuplot -p Np2.plt')

        end
