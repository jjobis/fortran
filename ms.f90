       integer,dimension(24) :: wd, ws
       real,dimension(24) :: u, v, cpwd
       real :: ap1,ap2,ap3,rp1,rp2
       real, parameter :: pi = 4.*atan(1.)

       open(1,file='time.dat')
       open(2,file='time2.dat')
       do i=1,24
       read(1,10)ws(i),wd(i)
10     format(7x, i2, 4x, i3)
       u(i) = (ws(i)*0.1) * cos((270d0-(wd(i)*1.0)) * (pi/180d0))
       v(i) = (ws(i)*0.1) * sin((270d0-(wd(i)*1.0)) * (pi/180d0))
       cpwd(i) = atan2(v(i),u(i))
       ap1=ap1+u(i)
       ap2=ap2+v(i)
       ap3=ap3+cpwd(i)
       write(2,20)u(i),v(i),cpwd(i),ap1,ap2,ap3
20     format(6f10.4)
       end do
       rp1 = ap1/24.
       rp2 = ap2/24.
       write(*,30)rp1,rp2, sqrt(rp1**2+rp2**2)
30     format(3(f10.4,'(m/s)'))

       end