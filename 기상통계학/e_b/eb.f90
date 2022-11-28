real,dimension(49,12)::b,e
real,dimension(588) :: reb,ree,rankb,ranke
real(kind=8) :: meanb,meane,sb,se,cov,T,R,Mean_cal,COD
real(kind=8) :: meanbr,meaner,covr,Tr,Rr,CODR,sbr,ser
real(kind=8):: scal,Rcal,Tcal,covxy
integer :: co

open(1,file='barrange.dat')
open(2,file='e.csv')
open(3,file='1.dat')
open(4,file='2.dat')
open(5,file='3.dat')
open(6,file='4.dat')
open(7,file='5.dat')
open(8,file='6.dat')
open(9,file='log.dat')
open(10,file='rlog.dat')
open(11,file='ylog.dat')
open(12,file='seqlog.dat')
co = 0

do i = 1,49
read(1,*)(b(i,j),j=1,12)
read(2,*)(e(i,j),j=1,12)
co = co + 12
end do

call uplevel(b,reb)
call uplevel(e,ree)
call rank(reb, rankb)
call rank(ree, ranke)

meanb = Mean_cal(co,reb)
meane = Mean_cal(co,ree)
cov= covxy(reb,ree,meanb,meane,co)
sb = scal(reb,meanb,co)
se = scal(ree,meane,co)
R = Rcal(cov,sb,se)
COD = R**2
T = Tcal(R,co)
write(7,*)"Pearson correlation"
write(7,'(a,f15.4)')'Mean_b= ',meanb
write(7,'(a,f15.4)')'Mean_e= ',meane
write(7,'(a,f15.4)')' covxy= ',cov
write(7,'(a,f15.4)')'  Sr_b= ',sb
write(7,'(a,f15.4)')'  Se_e= ',se
write(7,'(a,f15.4)')'     R= ',R
write(7,'(a,f15.4)')'   COD= ',COD
write(7,'(a,f15.4)')'     T= ',T
write(7,*)


meanbr = Mean_cal(co,rankb)
meaner = Mean_cal(co,ranke)
covr = covxy(rankb,ranke,meanbr,meaner,co)
sbr = scal(rankb,meanbr,co)
ser = scal(ranke,meaner,co)
Rr = Rcal(covr,sbr,ser)
CODR = Rr**2
Tr = Tcal(Rr,co)
write(7,*)"'Spearman's rank correlation"
write(7,'(a,f15.4)')'Mean_b= ',meanbr
write(7,'(a,f15.4)')'Mean_e= ',meaner
write(7,'(a,f15.4)')' covxy= ',covr
write(7,'(a,f15.4)')'  Sr_b= ',sbr
write(7,'(a,f15.4)')'  Se_e= ',ser
write(7,'(a,f15.4)')'     R= ',Rr
write(7,'(a,f15.4)')'   COD= ',CODR
write(7,'(a,f15.4)')'     T= ',Tr

do i = 1, 588
write(3,'(f7.1)')reb(i)
write(4,'(f7.1)')ree(i)
end do

do i = 1, 588
write(5,'(f7.2)')rankb(i)
write(6,'(f7.2)')ranke(i)
end do

end


function Mean_cal(n,x) result(xm)
       integer::n
       real,dimension(n)::x
       real(kind=8) :: xm
       xm = 0
       do i = 1,n
       xm = x(i) + xm
       end do
       xm = xm/(n*1.0)
end

function covxy(x,y,xm,ym,n) result(f)
       real(kind=8)::xm,ym,f
       integer :: n
       real,dimension(n)::x,y
       f = 0
       do i = 1,n
       f = (x(i) - xm)*(y(i)-ym) + f
       end do

       f = f/((n-1)*1.0)
       !write(9,*)f  ->> error log finding code
end

function scal(x,xm,n) result(f)
       real(kind=8)::xm,f
       integer :: n
       real,dimension(n)::x
       f = 0
       do i = 1,n
       f = (x(i) - xm)**2 + f
       end do

       f = sqrt(f/((n-1)*1.0))
end

function Tcal(r,n) result(f)
       real(kind=8) :: f,r
       integer :: n
       f = r*sqrt(((n-2)*1.0))/sqrt((1-r**2))
end

function Rcal(c,sx,sy) result(f)
       real(kind=8) :: c,sx,sy,f
       f = c/(sx*sy)
end

subroutine uplevel(x,f)
       real,dimension(49,12)::x
       real,dimension(588)::f
       real(kind=8):: n
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
       real(kind=8) :: y
       integer :: co , n , w
       co = 0 ; n = 1 ; w = 0 ; y = 0
       do i = 1, 588
       n = 1 ; y = 0;
       co = co + 1
       if(i>w)then
       if(list(i) /= list(i+1))then
       ranklist(i) = co
       else if(list(i) == list(i+1))then
       if(list(i+1) /= list(i+2))then
       n = n + 1
       y = (co + co + 1) / (n*1.0)
       w = co + n - 1
       do j = 1,n
       ranklist(j+co-1) = y
       end do
       else if(list(i+1) == list(i+2))then
       do j = 1,587-co
       if(list(co) == list(co+j))then
       n = n + 1
       y = co + (n - 2) + y
       else if(list(co) /= list(co+j))then
       y = y + co + j - 1
       y = y/(n*1.0)
       w = co + n - 1
       do k = 1,n
       ranklist(k+co-1) = y
       end do
       go to 7
       end if
       end do
       end if       
       end if
7      continue
       end if     
       end do
end subroutine rank

