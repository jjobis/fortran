       real :: r
       integer :: a,b
       character :: c
       write(*,*)'count of x,y'
       read(*,*)a
       write(*,*)'file -> 1        write -> 2'
       read(*,*)b

       if (b==1) then
       write(*,*)'file name'
       read(*,*)c
       else if (b==2)
       call cal_covr(a,r)
       end if
       write(*,*)r

       end

       subroutine cal_covr(a,r)

       real :: r
       integer :: a
       real,dimension(a) :: x,y
       