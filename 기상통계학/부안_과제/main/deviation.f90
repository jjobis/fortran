       real,dimension(12)::sumD,meanD
       real,dimension(49,12)::a,aD
       write(*,*)'s'
       call arrange_dat(a)
       
       do i = 1,12
         sumD(i) = 0
       do j = 1,49
         sumD(i) = sumD(i) + a(j,i)
       end do
         meanD(i)=sumD(i)/49
       end do

       do i = 1,12
       do j = 1,49
         aD(j,i) = a(j,i) - meanD(i)
       end do
       end do
       
      write(*,*)(meanD(i),i=1,12)

!      do i = 1, 49
!      write(*,'(12(3x,f7.2))')(aD(i,j),j=1,12)
!      end do

       call datcsv_dat(a,aD,meanD)
       end

       subroutine arrange_dat(a)
      
       real,dimension(49,12) :: a
       open(1,file='buan.dat')
       do i = 1,49
       do j = 1,12
       read(1,*)a(i,j)
       end do
       end do
       close(1)
!      do i = 1, 49
!      write(*,'(12(3x,f7.2))')(a(i,j),j=1,12)
!      end do
       end subroutine
       
       subroutine datcsv_dat(a,aD,meanD)
       real,dimension(49,12)::a,aD
       real,dimension(12)::meanD,punD
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
       open(30,file='anomaly.csv')
       open(40,file='mean.csv')
       open(50,file='pun.csv')
       open(60,file='jan.csv')
       open(61,file='jul.csv')
       open(65,file='arrange.csv')
       do i = 1,49
       write(60,'(f10.3)')a(i,1)
       write(61,'(f10.3)')a(i,7)
       end do






      do i = 1,12
      punD(i)=0
      do j = 1,49
      punD(i) = punD(i) + (aD(j,i)**2)
      end do
      write(50,'(f10.3)')sqrt(punD(i)/49.)
      end do




       do i = 10,21
       do j = 1,49
       write(i,'(3(f7.2,a))')a(j,i-9),',',aD(j,i-9),',', meanD(i-9),','
       end do
       end do
       do j = 1,49
       write(65,10)(a(j,i),i=1,12)
10     format(12(f7.2,','))
      end do
      do i = 1, 12
      write(40,'(f10.3)')MeanD(i)
      end do
       end
