real,dimension(49,12)::b,e
real,dimension(588) :: reb,ree
real :: meanb,meane,sb,se,cov,T,R,Mean_cal,COD
integer :: co

namelist /Pearson/ meanb,meane,sb,se,cov,T,R,COD

open(1,file='barrange.dat')
open(2,file='e.csv')
open(3,file='1.txt')
open(4,file='2.txt')

co = 0

do i = 1,49
read(1,*)(b(i,j),j=1,12)
read(2,*)(e(i,j),j=1,12)
co = co + 12
end do

call rank_cal(b,reb)
call rank_cal(e,ree)

meanb = Mean_cal(co,reb)
meane = Mean_cal(co,ree)

cov= covxy(reb,ree,meanb,meane,co)
sb = scal(reb,meanb,co)
se = scal(ree,meane,co)

R = Rcal(cov,sb,se)
COD = R**2
T = Tcal(R,co)

write(*,Pearson)

do i = 1, 588
write(3,'(f7.1)')reb(i)
write(4,'(f7.1)')ree(i)
end do


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

subroutine rank_cal(x,a)
       real,dimension(49,12)::x
       real,dimension(588) :: a
       real,dimension(588) :: ar
       real :: R
       integer :: n,m,co,v
       m = 588
       a = reshape(x,(/588/))
       do j = 1,587
       do i = 1,587
       if (a(i)>a(i+1))then
       R = a(i)
       a(i) = a(i+1)
       a(i+1) = R
       end if
       end do
       end do

       co = 0
       n = 0

       do i = 1,587
       if (i > n) then
              if (a(i) == a(i+1)) then
              n = i + 1 + n
              m = m - 1
              co = co + 1
                     do j = 2,m
                     if (a(i) == a(i+j))then
                     n = n + 1
                     m = m - 1
                     end if
                     if (a(i) /= a(i+j))then
                     do v = i,n
                     ar(v) = n/(co*1.0)
                     go to 7
                     end do
                     end if
                     end do
7             continue
              co = 0
              else
              ar(i)=i
              end if
       end if
       end do

       write(*,*)ar
end
