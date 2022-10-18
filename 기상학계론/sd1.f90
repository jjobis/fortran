        REAL :: r, d
        REAL, parameter :: pi = 4. * atan(1.)

        open(1, file = 'sd1.csv')

        do i = 1, 365

        r = (2 * pi * (i - 1))/365

        d1 = 0.006918 - 0.399912*cos(r) + 0.070257*sin(r) - 0.006758*cos(2*r)
        d2 = 0.000907*sin(2*r) - 0.002697*cos(3*r) + 0.00148*sin(3*r)

        d = (d1 + d2) * (180/pi)


        write(1, 10) i,',',d
10      format(i3, a, f10.2, a)
        end do
        end
        
