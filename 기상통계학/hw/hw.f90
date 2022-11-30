! linear regression analysis (Global Land and Ocean Temperature Anomalies)

integer :: n
real,allocatable,dimension(:)::x,y,yex
real :: xmean,ymean,cov,sx,sy,r,T,B,al,Mean_cal
real :: SSR,SSE,SST,MSR,MSE,MST,Fsta,Ta,Tb,se,sa,sb
real(kind=selected_real_kind(18,4931)) :: P,pcal
integer :: EDF,RDF,TDF,k,e
integer,dimension(6) :: co

open(1,file='1901-2000.csv')
open(2,file='1930-2000.csv')
open(3,file='1950-2000.csv')
open(4,file='1970-2000.csv')
open(5,file='1980-2000.csv')
open(6,file='1990-2000.csv')
open(44,file='result.dat')
open(45,file='result30.dat')
open(46,file='result50.dat')
open(47,file='result70.dat')
open(48,file='result80.dat')
open(49,file='result90.dat')
open(999,file='logfind.dat')
open(9999,file='ANOVA.dat')
open(22,file='n.dat')

do n = 1,6

read(22,*)co(n)

allocate(x(co(n)),y(co(n)),yex(co(n)))

do i = 1,co(n)
       read(n,*)x(n),y(n)
       x(n) = i
end do

xmean = Mean_cal(co(n),x) ; ymean = Mean_cal(co(n),y)
cov = covxy(x,y,xmean,ymean,co(n))
sx = scal(x,xmean,co(n))
sy = scal(y,ymean,co(n))
r = Rcal(cov,sx,sy)
T = Tcal(r,co(n))
P = Pcal(co(n),T)
B = beta(x,y,xmean,ymean,co(n))
al = acal(xmean,ymean,B)

do i = 1,co(n)
yex(i) = tyex(al,B,x(i))
end do

se = Secal(y,yex,co(n))
sa = Sacal(se,x,xmean,co(n))
sb = Sbcal(se,x,xmean,co(n))
Ta = tabcal(al,sa)
Tb = tabcal(B,sb)

call cssr(SSR,y,yex,co(n))
call csse(SSE,yex,ymean,co(n))
call csst(SST,SSR,SSE)


k = 1 !단순회귀 -> 독립변수(k) = 1
EDF = k
RDF = co(n)-k-1
TDF = co(n)-1
MSE = SSE/(EDF*1.0)
MSR = SSR/(RDF*1.0)
MST = SST/(TDF*1.0)
Fsta = MSE/MSR
!write

call gplot(y,al,B,co(n))
CALL SYSTEM('gnuplot -p plot.plt')

write(n+43,10)'xmean =',xmean
write(n+43,10)'ymean =',ymean
write(n+43,10)'covxy =',cov
write(n+43,10)'sx =',sx
write(n+43,10)'sy =',sy
write(n+43,10)'r =',r
write(n+43,10)'R =',r**2
write(n+43,10)'T =',T
write(n+43,10)'P-val =',P
write(n+43,10)'LRA a =',al
write(n+43,122)'LRA b =',B
write(n+43,10)'SSR =',SSR
write(n+43,10)'SSE =',SSE
write(n+43,10)'SST =',SST
write(n+43,10)'MSR =',MSR
write(n+43,10)'MSE =',MSE
write(n+43,10)'MST =',MST
write(n+43,11)'EDF =',EDF
write(n+43,11)'RDF =',RDF
write(n+43,11)'TDF =',TDF
write(n+43,10)'Fsta =',Fsta
write(n+43,10)'se =',se
write(n+43,10)'sa =',sa
write(n+43,10)'sb =',sb
write(n+43,10)'ta =',ta
write(n+43,10)'tb =',tb

write(n+43,*)
write(n+43,*)'yex arrange'
do i = 1, co(i)
write(n+43,20)yex(i)
end do

call write_anova(EDF,RDF,TDF,SSE,SSR,SST,MSE,MSR,MST,Fsta)

!format

10     format(a15,f15.4)
11     format(a15,i10)
20     format(f15.4)
122    format(a15,f15.8)

deallocate(x,y,yex)

end do

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
       f = gamma((v + 1.0d0)/2.0d0)/(gamma(v/2.0d0)*sqrt(v * pi))/sqrt((1.0d0 + (T*T/v))**(v+1.0d0))
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

subroutine gplot(x,a,b,n)
       real,dimension(n)::x
       integer :: n
       real :: a,b

       open(777,file='data.txt')
       open(888,file='plot.plt')

       do i = 1,n
       write(777,*)i,x(i)
       end do

       


       write(888,'(a)')'set title "Fortran Example'
       write(888,'(a)')'set nokey'
       write(888,'(a)')'set grid'
       write(888,'(a)')'set xlabel "x"'
       write(888,'(a)')'set ylabel "y"'
       write(888,'(a)')'m="data.txt"'
       write(888,200)'plot m using 1:2,',a,'+',b,'*x'

!with linespoints

200    format(a57,f7.4,a1,f15.8,a2)

end