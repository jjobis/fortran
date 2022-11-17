real,dimension(49,12)::b,e
real,dimension(588) :: reb,ree,rankb,ranke
real :: meanb,meane,sb,se,cov,T,R,Mean_cal,COD
integer :: co

!namelist /Pearson/ meanb,meane,sb,se,cov,T,R,COD

open(1,file='barrange.dat')
open(2,file='e.csv')
open(3,file='1.txt')
open(4,file='2.txt')
open(5,file='3.txt')
open(6,file='4.txt')
open(7,file='5.txt')
open(8,file='6.txt')
co = 0

do i = 1,49
read(1,*)(b(i,j),j=1,12)
read(2,*)(e(i,j),j=1,12)
co = co + 12
end do

call uplevel(b,reb)
call uplevel(e,ree)

meanb = Mean_cal(co,reb)
meane = Mean_cal(co,ree)
cov= covxy(reb,ree,meanb,meane,co)
sb = scal(reb,meanb,co)
se = scal(ree,meane,co)
R = Rcal(cov,sb,se)
COD = R**2
T = Tcal(R,co)

!write(7,Pearson)

!do i = 1, 588
!write(3,'(f7.1)')reb(i)
!write(4,'(f7.1)')ree(i)
!end do

call rank(reb, rankb)
call rank(ree, ranke)

!do i = 1, 588
!write(5,'(f7.2)')rankb(i)
!write(6,'(f7.2)')ranke(i)
!end do

end

function Mean_cal(n,x) result(xm)
       integer::n
       real,dimension(n)::x
       real :: xm
       xm = 0
       do i = 1,n
       xm = x(i) + xm
       end do
       xm = xm/(n*1.0)
end

function covxy(x,y,xm,ym,n) result(f)
       real::xm,ym
       integer :: n
       real,dimension(n)::x,y
       f = 0
       do i = 1,n
       f = (x(i) - xm)*(y(i)-ym) + f
       end do

       f = f/((n-1)*1.0)
end

function scal(x,xm,n) result(f)
       real::xm,f
       integer :: n
       real,dimension(n)::x
       f = 0
       do i = 1,n
       f = (x(i) - xm)**2 + f
       end do

       f = sqrt(f/((n-1)*1.0))
end

function Tcal(r,n) result(f)
       real :: f,r
       integer :: n
       f = r*sqrt(((n-2)*1.0))/sqrt((1-r**2))
end

function Rcal(c,sx,sy) result(f)
       real :: c,sx,sy,f
       f = c/(sx*sy)
end

subroutine uplevel(x,f)
       real,dimension(49,12)::x
       real,dimension(588)::f
       real :: n
       f = reshape(x,(/588/))
       do i = 1,588
       do j = 1,587
       if(f(j)>f(j+1))then
       n = f(j)
       f(j) = f(j+1)
       f(j+1) = n
       end if
       end do
       end do
end

subroutine rank(list,ranklist)
       real,dimension(588) :: list
       real,dimension(588) :: ranklist
       integer :: co , n , w, y
       co = 0 ; n = 0 ; w = 0 ; y = 0
       do i = 1,587
       co = co + 1
       y = co
       !write(8,*)co
       ranklist(i) = 0
         if ( i == co ) then
          if (list(i) == list(i+1)) then
          co = co + 1
          n = n + 1
          w = c
           write(8,*)co
           do j = co , 587
           if (list(j) == list(j+1))then
           co = co + 1
           n = n + 1
           y = co + y
           end if
           do k = w,co
           ranklist(k) = y / (n * 1.0)
           end do
           end do
         do j = i,co
         ranklist(j) = ((co-1)+co)/2.
         end do
         end if
       else if (list(i) /= list(i+1))then
       ranklist(i) = co
      ! write(8,*)ranklist(i)
       end if
       end do
end subroutine rank

