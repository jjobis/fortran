real,allocatable,dimension(:)::x,y
real :: xmean,ymean,covxy,sx,sy,r,T,al,pval,DELTA,IDF,Mean_cal,cov,var,B
integer::n
real(kind=selected_real_kind(18,4931)) :: P,pcal
open(1,file='a.dat')
write(*,*) 'write N'
read(*,*)n
allocate(x(n),y(n))
read(1,*)(x(i),i=1,n)
read(1,*)(y(i),i=1,n)

xmean = Mean_cal(n,x) ; ymean = Mean_cal(n,y)
write(*,'(2(a,f7.2))')'xmean= ',xmean,' ymean= ',ymean
var = var_cal(x,n,xmean)
!write(*,*)'var= ',var

!write(*,*)rf/(n-1.),sqrt(rs/(n-1.)),sqrt(rt/(n-1.))
cov = covxy(x,y,xmean,ymean,n)
sx = scal(x,xmean,n)
sy = scal(y,ymean,n)
r = Rcal(cov,sx,sy)
T = Tcal(r,n)
write(*,'(a,f15.4)')'COV [X,Y] = ' ,cov  ! fortran gsl
write(*,'(2(a,f15.4))')'Sx= ',sx,'  Sy= ',sy
write(*,'(3(a,f15.4))')'r= ',r,'  R = ',r**2,'  T= ',T
P = Pcal(n,T)
write(*,'(2(a,f15.6))') 'P= ',P,'  p-value= ',P*2
!B = beta(r,sy,sx)
B = beta(x,y,xmean,ymean,n)
write(*,'(a,f15.4)')'B= ' ,B
al = acal(xmean,ymean,B)
write(*,'(a,f15.4)')'al = ',al


end

function var_cal(x,n,mx) result(f)
       integer :: n
       real :: mx
       real,dimension(n)::x
       f = 0
       do i = 1, n
        f = (x(i) -  mx) + f
        !write(*,*) f
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
       
