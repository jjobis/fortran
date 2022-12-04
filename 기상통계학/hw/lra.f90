! linear regression analysis (Global Land and Ocean Temperature Anomalies)

integer :: n
real,allocatable,dimension(:)::x,y,yex
real :: xmean,ymean,cov,sx,sy,r,T,B,al,Mean_cal
real :: SSR,SSE,SST,MSR,MSE,MST,Fsta,Ta,Tb,se,sa,sb
real(kind=selected_real_kind(18,4931)) :: P,pcal
integer :: EDF,RDF,TDF,k,e,bo,lao

write(*,*)'[Land] -> 1 | [Ocean] -> 2 '
read(*,*)lao

write(*,*)'[1901] -> 1 | [1930] -> 2 | [1950] -> 3'
write(*,*)'[1970] -> 4 | [1980] -> 5 | [1990] -> 6'
read(*,*)n

open(1,file='land/la1901-2000.csv')
open(2,file='land/la1930-2000.csv')
open(3,file='land/la1950-2000.csv')
open(4,file='land/la1970-2000.csv')
open(5,file='land/la1980-2000.csv')
open(6,file='land/la1990-2000.csv')
open(7,file='ocean/oc1901-2000.csv')
open(8,file='ocean/oc1930-2000.csv')
open(9,file='ocean/oc1950-2000.csv')
open(10,file='ocean/oc1970-2000.csv')
open(11,file='ocean/oc1980-2000.csv')
open(12,file='ocean/oc1990-2000.csv')
open(44,file='land/data/resultla.dat')
open(45,file='land/data/result30la.dat')
open(46,file='land/data/result50la.dat')
open(47,file='land/data/result70la.dat')
open(48,file='land/data/result80la.dat')
open(49,file='land/data/result90la.dat')
open(50,file='ocean/data/resultOC.dat')
open(51,file='ocean/data/result30OC.dat')
open(52,file='ocean/data/result50OC.dat')
open(53,file='ocean/data/result70OC.dat')
open(54,file='ocean/data/result80OC.dat')
open(55,file='ocean/data/result90OC.dat')
open(999,file='logfind.dat')

call ne(n,e,bo,lao)

allocate(x(n),y(n),yex(n))

do i = 1,n
       read(e,*)x(i),y(i)
       x(i) = i + bo
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

call gplot(y,al,B,n,e,bo,lao)
CALL SYSTEM('gnuplot -p plot.plt')
CALL SYSTEM('gnuplot -p plotall.plt')

write(e+43,10)'xmean =',xmean
write(e+43,10)'ymean =',ymean
write(e+43,10)'covxy =',cov
write(e+43,10)'sx =',sx
write(e+43,10)'sy =',sy
write(e+43,10)'r =',r
write(e+43,10)'R =',r**2
write(e+43,10)'T =',T
write(e+43,10)'P-val =',P
write(e+43,10)'LRA a =',al
write(e+43,122)'LRA b =',B
write(e+43,10)'SSR =',SSR
write(e+43,10)'SSE =',SSE
write(e+43,10)'SST =',SST
write(e+43,10)'MSR =',MSR
write(e+43,10)'MSE =',MSE
write(e+43,10)'MST =',MST
write(e+43,11)'EDF =',EDF
write(e+43,11)'RDF =',RDF
write(e+43,11)'TDF =',TDF
write(e+43,10)'Fsta =',Fsta
write(e+43,10)'se =',se
write(e+43,10)'sa =',sa
write(e+43,10)'sb =',sb
write(e+43,10)'ta =',ta
write(e+43,10)'tb =',tb

call write_anova(EDF,RDF,TDF,SSE,SSR,SST,MSE,MSR,MST,Fsta,e)

!format

10     format(a15,f15.4)
11     format(a15,i10)
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

subroutine write_anova(EDF,RDF,TDF,SSE,SSR,SST,MSE,MSR,MST,Fsta,e)
       real :: SSE,SSR,SST,MSE,MSR,MST,Fsta
       integer :: EDF,RDF,TDF,e

       character (len = 20):: link

              if (e == 1) link = 'land/data/ANOVA1'
              if (e == 2) link = 'land/data/ANOVA2'
              if (e == 3) link = 'land/data/ANOVA3'
              if (e == 4) link = 'land/data/ANOVA4'
              if (e == 5) link = 'land/data/ANOVA5'
              if (e == 6) link = 'land/data/ANOVA6'
              if (e == 7) link = 'ocean/data/ANOVA1'
              if (e == 8) link = 'ocean/data/ANOVA2'
              if (e == 9) link = 'ocean/data/ANOVA3'
              if (e == 10) link = 'ocean/data/ANOVA4'
              if (e == 11) link = 'ocean/data/ANOVA5'
              if (e == 12) link = 'ocean/data/ANOVA6'

       open(9999,file=link)


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

subroutine ne(n,e,bo,lao)

integer :: n,e,bo,lao

if ( lao == 1) then
       if(n==1)then
       n = 1200
       e = 1
       bo = 0
       else if(n==2)then
       n = 852
       e = 2
       bo = 1200 - 852
       else if(n==3)then
       n = 612
       e = 3
       bo = 1200 - 612
       else if(n==4)then
       n = 372
       e = 4
       bo = 1200 - 372
       else if(n==5)then
       n = 252
       e = 5
       bo = 1200 - 252
       else if(n==6)then
       n = 132
       e = 6
       bo = 1200 - 132
       end if
else if ( lao == 2) then
       if(n==1)then
       n = 1200
       e = 7
       bo = 0
       else if(n==2)then
       n = 852
       e = 8
       bo = 1200 - 852
       else if(n==3)then
       n = 612
       e = 9
       bo = 1200 - 612
       else if(n==4)then
       n = 372
       e = 10
       bo = 1200 - 372
       else if(n==5)then
       n = 252
       e = 11
       bo = 1200 - 252
       else if(n==6)then
       n = 132
       e = 12
       bo = 1200 - 132
       end if
end if
end

subroutine gplot(x,a,b,n,e,bo,lao)
       real,dimension(n)::x
       integer :: n,e,bo
       real :: a,b
       character (len = 12):: link
       if (lao == 1) then
       link = 'land/data/'
       else if (lao == 2) then
       link = 'ocean/data/'
       end if
       
       open(777,file='data.txt')
       open(888,file='plot.plt')

       do i = 1,n
       write(777,*)i+bo,x(i)
       end do

       write(888,'(a)')'set title "linear regression analysis (Global Temperature Anomalies)"'
       write(888,'(a)')'set nokey'
       write(888,'(a)')'set grid'
       write(888,'(a)')'set ylabel "anomaly"'
       write(888,'(a)')'set xlabel "year/month count"'
       write(888,'(a)')'m="data.txt"'
       write(888,'(a)')'set style line 11 lc rgb "#808080" lt 1'
       write(888,'(a)')'set border 3 back ls 11'
       write(888,'(a)')'set tics nomirror'
       write(888,'(a)')'set style line 12 lc rgb "#808080" lt 0 lw 1'
       write(888,'(a)')'set grid back ls 12'
       write(888,'(a)')'set term png size 1000,600'
       write(888,'(a,i4,a,i4,a)')'set xrange [',bo,':',1200,']'
       write(888,'(a,a,i2,a)')'set output "',link,e,'.png"'
       write(888,200)'plot m using 1:2 pt 1 ps 1 lt 1 lw 1,',a,'+',b,'*x'
!with linespoints

200    format(a57,f15.4,a1,f15.8,a2)

end