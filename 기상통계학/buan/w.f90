       real,dimension(12)::sumD,meanD,perD,skewD,skewA,kurD,kurA
       real,dimension(49,12)::aD
       real,parameter::n=49.0d0
       open(1,file='')
       open(11,file='')
       open(20,file='')

       do i = 1,49
       read(*,*)(aD(i,j),j=1,12)
       end do

       do i = 1,12
       read(1,*)sumD(i),meanD(i),perD(i)
       end do
       
       do i =1,12
       do j =1,49
       skewA(i) = (aD(j,i)-meanD(i))**3
       kurA(i) = (aD(j,i)-meanD(i))**4
       end do
       end do

       do i = 1,12
       skewD(i) = (n/(n-1.0d0)*(n-2.0d0))*(meanD(i)/(perD(i))**3)
       end do

       
       do i = 1,12
       kurD(i) = ((n*(n+1.0d0)/(n-1.0d0)*(n-2.0d0)*(n-3.0d0))* &
                 kurA(i)/(perD(i))**4) - &
                 3*(n-1.0d0)**2/(n-2.0d0)*(n-3.0d0)
       end do

       do = 1,12
       write(20,'(2f10.2)')skewD(i),kurD(i)
       end do
       
       end 