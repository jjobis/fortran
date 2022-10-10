       integer,dimension(24) :: wd, ws, time
       real,dimension(24) :: u, v, cpwd
       real ::ap1,ap2,rp1,rp2,cw
       real, parameter :: pi = 4.*atan(1.)
       character(2), parameter :: utf_degree = char(int(Z'C2'))//char(int(Z'B0'))

       open(2,file='time2.dat')
       call readTime(time)
       call calUV(ws,wd,u,v,cpwd,ap1,ap2)
       call writeTimeCal(time,cpwd,u,v)
       call meanUV(rp1,rp2,ap1,ap2)
       call writeMeanUV(rp1,rp2,cw)
       
       end

!mean of resultant wind component -> 합성 바람 성분
!mean of resultant wind velocity -> 합성 풍속
!mean of resultant wind direction -> 합성 풍향

       subroutine readTime(time)
              integer,dimension(24) :: time
              open(1,file='time.dat')
              do i =1,24
              read(1,15)time(i)
       15     format(i2)
              enddo
              close(1)
       end subroutine

       subroutine calUV(ws,wd,u,v,cpwd,ap1,ap2)
              integer,dimension(24) :: wd, ws
              real,dimension(24) :: u, v, cpwd
              real :: ap1,ap2,ap3
              real, parameter :: pi = 4.*atan(1.)
              open(11,file='time.dat')
              do i=1,24
              read(11,10)ws(i),wd(i)
       10     format(7x, i2, 4x, i3)
              u(i) = (ws(i)*0.1) * cos((270d0-(wd(i)*1.0)) * (pi/180d0))
              v(i) = (ws(i)*0.1) * sin((270d0-(wd(i)*1.0)) * (pi/180d0))
              cpwd(i) = atan2(v(i),u(i))
              ap1=ap1+u(i)
              ap2=ap2+v(i)
              ap3=ap3+cpwd(i)
              end do

              close(11)
       end subroutine

       subroutine writeTimeCal(time,cpwd,u,v)
              integer,dimension(24) :: time
              real,dimension(24) :: u, v, cpwd
              do i = 1, 24
              write(2,10)time(i),':00'
       10     format(i2,a)
              write(2,20)u(i)
       20     format('resultant wind component u = ',f10.4)
              write(2,30)v(i)
       30     format('resultant wind component v = ',f10.4)
              write(2,40)cpwd(i)
       40     format('resultant wind velocity    = ',f10.4)
              write(2,*)'---------------------------------------------'
              end do

       end subroutine

       subroutine meanUV(rp1,rp2,ap1,ap2)
              real :: rp1,rp2,ap1,ap2
              rp1 = ap1/24. !u 평균
              rp2 = ap2/24. !v 평균

       end subroutine

       subroutine writeMeanUV(rp1,rp2,cw)
!             real, parameter :: pi = 4.*atan(1.)
              character(2), parameter :: utf_degree = char(int(Z'C2'))//char(int(Z'B0'))
              real :: rp1,rp2,cw
              write(2,10)rp1
       10     format('mean of resultant wind component u = ',f10.4,'(m/s)')
              write(2,20)rp2
       20     format('mean of resultant wind component v = ',f10.4,'(m/s)')
              write(2,30)sqrt(rp1**2+rp2**2)
       30     format('mean of resultant wind velocity    = ',f10.4,'(m/s)')
              cw = atan(rp1/rp2)*180./pi
              write(2,40)cw,utf_degree
       40     format('mean of resultant wind direction   = ',f11.4,a2)

       end subroutine
