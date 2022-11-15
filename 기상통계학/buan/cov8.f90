real,allocatable,dimension(:)::x,y
real :: xmean,ymean,covxy,sx,sy,r,T,pval,DELTA,IDF,Mean_cal,cov
integer::n
real(kind=selected_real_kind(18,4931)) :: P,pcal
open(1,file='a.dat')
write(*,*) 'write N'
read(*,*)n
allocate(x(n),y(n))
read(1,*)(x(i),i=1,n)
read(1,*)(y(i),i=1,n)

xmean = Mean_cal(n,x) ; ymean = Mean_cal(n,y)
write(*,*)xmean,ymean

!write(*,*)rf/(n-1.),sqrt(rs/(n-1.)),sqrt(rt/(n-1.))
cov = covxy(x,y,xmean,ymean,n)
sx = scal(x,xmean,n)
sy = scal(y,ymean,n)
r = Rcal(cov,sx,sy)
T = Tcal(r,n)
write(*,'(f15.4)')cov  ! fortran gsl
write(*,'(2f15.4)')sx,sy
write(*,'(2f15.4)')r,T
P = Pcal(n,T)
write(*,'(f15.6)') P


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
       v = n-1
       f = gamma((v+1)/2.)/(sqrt(v*pi)*gamma(v/2.))*sqrt(1/((1 + T**2/(v*1.0))**(v+1))*1.0)
end