! linear regression analysis (Global Land and Ocean Temperature Anomalies)

integer :: n
real,allocatable,dimension(:)::x,y,yex
real :: xmean,ymean,cov,sx,sy,r,T,B,al,Mean_cal
real :: SSR,SSE,SST,MSR,MSE,MST,Fsta,Ta,Tb,se,sa,sb
real(kind=selected_real_kind(18,4931)) :: P,pcal
integer :: EDF,RDF,TDF,k,e

write(*,*)'[1901] -> 1 | [1930] -> 2 | [1950] -> 3'
write(*,*)'[1970] -> 4 | [1980] -> 5 | [1990] -> 6'
read(*,*)n

open(1,file='1901-2000.csv')
open(2,file='1930-2000.csv')
open(3,file='1950-2000.csv')
open(4,file='1970-2000.csv')
open(5,file='1980-2000.csv')
open(6,file='1990-2000.csv')
open(44,file='result.dat')
open(999,file='logfind.dat')
open(9999,file='ANOVA.dat')

call ne(n,e)

allocate(x(n),y(n),yex(n))

do i = 1,n
       read(e,*)x(i),y(i)
       x(i) = i
end do

xmean = Mean_cal(n,x) ; ymean = Mean_cal(n,y)
cov = covxy(x,y,xmean,ymean,n)
sx = scal(x,xmean,n)
sy = scal(y,ymean,n)
r = Rcal(cov,sx,sy)
T = Tcal(r,n)
P = Pcal(n,T)
B = beta(x,y,xmean,ymean,n)
al = acal(xmean,ymean,B)

do i = 1,n
yex(i) = tyex(al,B,x(i))
end do

se = Secal(y,yex,n)
sa = Sacal(se,x,xmean,n)
sb = Sbcal(se,x,xmean,n)
Ta = tabcal(al,sa)
Tb = tabcal(B,sb)

call cssr(SSR,y,yex,n)
call csse(SSE,yex,ymean,n)
call csst(SST,SSR,SSE)


k = 1 !단순회귀 -> 독립변수(k) = 1
EDF = k
RDF = n-k-1
TDF = n-1
MSE = SSE/(EDF*1.0)
MSR = SSR/(RDF*1.0)
MST = SST/(TDF*1.0)
Fsta = MSE/MSR
!write

write(44,10)'xmean =',xmean
write(44,10)'ymean =',ymean
write(44,10)'covxy =',cov
write(44,10)'sx =',sx
write(44,10)'sy =',sy
write(44,10)'r =',r
write(44,10)'R =',r**2
write(44,10)'T =',T
write(44,10)'P-val =',P*2
write(44,10)'LRA a =',al
write(44,122)'LRA b =',B
write(44,10)'SSR =',SSR
write(44,10)'SSE =',SSE
write(44,10)'SST =',SST
write(44,10)'MSR =',MSR
write(44,10)'MSE =',MSE
write(44,10)'MST =',MST
write(44,11)'EDF =',EDF
write(44,11)'RDF =',RDF
write(44,11)'TDF =',TDF
write(44,10)'Fsta =',Fsta
write(44,10)'se =',se
write(44,10)'sa =',sa
write(44,10)'sb =',sb
write(44,10)'ta =',ta
write(44,10)'tb =',tb

write(44,*)
write(44,*)'yex arrange'
do i = 1, n
write(44,20)yex(i)
end do

call write_anova(EDF,RDF,TDF,SSE,SSR,SST,MSE,MSR,MST,Fsta)

!format

10     format(a15,f15.4)
11     format(a15,i10)
20     format(f15.4)
122    format(a15,f15.8)
end

! function & subroutine

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

function Secal(y,yex,n) result(f)
       real :: f,t
       integer :: n
       real,dimension(n)::y,yex

       t = 0
       do i = 1,n
       t = (y(i)-yex(i))**2 + t
       end do

       f = (1/((n-2)*1.0)) * t
       f = sqrt(f)
end

function Sacal(se,x,xm,n) result(f)
       real :: se,xm,f,T,H
       integer :: n
       real,dimension(n) :: x

       T = 0; H = 0;
       do i = 1,n      
       T = x(i)**2 + T
       H = (x(i)-xm)**2 + H
       end do

       f = se*(T/((n*1.0)*H))

end

function Sbcal(se,x,xm,n) result(f)
       real :: se,xm,f,T
       integer :: n
       real,dimension(n) :: x

       T = 0
       do i = 1,n
       T = (x(i)-xm)**2 + T
       end do

       f = se / sqrt(T)
end

function Tcal(r,n) result(f)
       real :: f,r
       integer :: n
       f = r*sqrt(((n-2)*1.0))/sqrt((1-r**2))
end

function Tabcal(a,s) result(f)
       real :: f,a,s
       f = a/s
end

function Rcal(c,x,y) result(f)
       real :: c,x,y,f
       f = c/(x*y)
end

function Pcal(n,T) result(f)
       integer :: n,v
       real,parameter :: pi = 4. * atan(1.)
       real :: T
       real(kind=selected_real_kind(18,4931)) :: f
       v = n-2
       f = gamma((v + 1.0d0)/2.0d0)/gamma(v/2.0d0)/sqrt(v * pi)/sqrt((1.0d0 + (T*T/v))**(v+1.0d0))
end

function beta(x,y,xm,ym,n) result(f)
       real,dimension(n)::x,y
       integer :: n
       real :: xm,ym,re1,re2
       re1 = 0; re2 = 0; f =0
       do i = 1,n
       re1 = ((x(i) - xm)*(y(i) - ym)) + re1
       re2 = (x(i) - xm)**2 + re2
       end do
       write(*,*)re1,re2
       f = re1/re2
end

function acal(xm,ym,b) result(f)
       real :: b,xm,ym
       f = ym - (b*xm)
end
       
function tyex(a,b,x) result(f)
       real::a,b,x
       f = a + (b*x)
end

subroutine cssr(x,y,ye,n)
       integer :: n
       real,dimension(n) :: y,ye
       real :: x
       x = 0;
       do i = 1, n
       x = (y(i) - ye(i))**2 + x
       end do
end

subroutine csse(x,ye,ym,n)
       integer :: n
       real,dimension(n) :: ye
       real :: ym,x

       x = 0;
       do i = 1, n
       x = (ye(i)-ym)**2 + x
       end do
end

subroutine csst(f,x,y)
       real :: f,x,y
       f = x + y
end

subroutine write_anova(EDF,RDF,TDF,SSE,SSR,SST,MSE,MSR,MST,Fsta)
       real :: SSE,SSR,SST,MSE,MSR,MST,Fsta
       integer :: EDF,RDF,TDF

       write(9999,100)'',' | ','Degrees of freedom',' | ','Sum of Square',' | ','Mean Square',' | ','F Statistic'
       write(9999,103)'-----------------------------------------------------------------------------------------------------------'
       write(9999,101)'Regression',' | ',EDF,' | ',SSE,' | ',MSE,' | ',Fsta
       write(9999,103)'-----------------------------------------------------------------------------------------------------------'
       write(9999,102)'Residual',' | ',RDF,' | ',SSR,' | ',MSR,' | '
       write(9999,103)'-----------------------------------------------------------------------------------------------------------'
       write(9999,102)'Total',' | ',TDF,' | ',SST,' | ',MST,' | '
       write(9999,103)'-----------------------------------------------------------------------------------------------------------'
100    format(a15,4(a3,a20))
101    format(a15,a3,i20,3(a3,f20.4))
102    format(a15,a3,i20,2(a3,f20.4),a3)
103    format(a107)

end

subroutine ne(n,e)

integer :: n,e

if(n==1)then
n = 1200
e = 1
else if(n==2)then
n = 852
e = 2
else if(n==3)then
n = 612
e = 3
else if(n==4)then
n = 372
e = 4
else if(n==5)then
n = 252
e = 5
else if(n==6)then
n = 132
e = 6
end if
end