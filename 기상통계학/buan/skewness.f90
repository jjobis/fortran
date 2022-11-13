       real,dimension(49,12)::arD,anoD
       real,dimension(12)::meanD,devD,skewD,f3D,f4D,sD,kurD,exkurD
       real,parameter::n = 49.0

       !open(1,file='anomaly.dat')
       open(2,file='arrange.dat')
       open(3,file='skew.csv')
       open(4,file='kur.csv')
       open(5,file='exkur.csv')
       !open(3,file='mean.dat')

       do i = 1,49
       read(2,*)(arD(i,j),j=1,12)
      ! read(1,*)(anoD(i,j),j=1,12)
       end do

       do i = 1,12
       meanD(i) = 0
       do j = 1,49
       meanD(i) = arD(j,i) + meanD(i)
       end do
       meanD(i) = meand(i)/n
       end do

       do i = 1,12
       do j = 1,49
       anoD(j,i) = 0
       anoD(j,i) = arD(j,i) - meanD(i)
       end do
       end do
       !do i = 1,12
      ! read(3,*)meanD(i)
      ! end do

       do i = 1,12
       sD(i) = 0
       do j = 1,49
       sD(i) = (anoD(j,i))**2 + sD(i)
       end do
       end do

       do i = 1, 12
       sD(i) = sqrt(sD(i)/(n-1))
       write(*,*)sD(i)
       end do

       do i = 1,12 
       f3D(i) = 0 ; f4D(i) = 0
       do j = 1,49

       f3D(i) = (anoD(j,i)/sD(i))**3 + f3D(i)
       f4D(i) = (anoD(j,i)/sD(i))**4 + f4D(i)

       end do
       end do

       do i = 1, 12
       skewD(i) = 0
       skewD(i) = (n/((n-1)*(n-2)))*f3D(i)
       write(*,'(f15.5)')skewD(i)
       end do

       do i = 1, 12
       kurD(i) = 0 ; exkurD(i) = 0
       kurD(i) = ((n*(n+1))/((n-1)*(n-2)*(n-3)))*f4D(i)
       exkurD(i) = ((n*(n+1))/((n-1)*(n-2)*(n-3)))*f4D(i)-(3*(n-1)**2/((n-2)*(n-3)))
       write(*,'(2(f15.5))')kurD(i),exkurD(i)
       end do

       do i =1,12
       write(3,'(f12.5)')skewD(i)
       write(4,'(f12.5)')kurD(i)
       write(5,'(f12.5)')exkurD(i)
       end do

       call arrange_mon(arD)

       end


       subroutine arrange_mon(arD)
              real,dimension(49,12)::arD
              open(10,file='1.dat')
              open(11,file='2.dat')
              open(12,file='3.dat')
              open(13,file='4.dat')
              open(14,file='5.dat')
              open(15,file='6.dat')
              open(16,file='7.dat')
              open(17,file='8.dat')
              open(18,file='9.dat')
              open(19,file='10.dat')
              open(20,file='11.dat')
              open(21,file='12.dat')

              do i = 1,12
              write(i+9,50)(arD(j,i),j=1,49)
       50     format(49(f7.3,','))
              end do
              end