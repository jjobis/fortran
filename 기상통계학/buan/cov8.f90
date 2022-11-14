real,dimension(15)::x,y
real :: xmean,ymean,rf,rs,rt,r,T,pval,P,DELTA,IDF
integer::n
open(1,file='a.dat')
write(*,*) 'write N'
read(*,*)n
read(1,*)(x(i),i=1,n)
read(1,*)(y(i),i=1,n)

xmean = 0 ; ymean = 0

do i = 1,n
xmean = x(i) + xmean
ymean = y(i) + ymean
end do

xmean = xmean/n
ymean = ymean/n

write(*,*)xmean,ymean

rf=0;rs=0;rt=0;r=0

do i = 1,n
rf = (x(i) - xmean)*(y(i)-ymean)+rf
rs = (x(i) - xmean)**2+rs
rt = (y(i) - ymean)**2+rt
end do

!write(*,*)rf/(n-1.),sqrt(rs/(n-1.)),sqrt(rt/(n-1.))

rf = rf/((n-1)*1.0)
rs = sqrt(rs/((n-1)*1.0))
rt = sqrt(rt/((n-1)*1.0))

r = rf/(rs*rt)

T = r*sqrt(((n-2)*1.0))/sqrt((1-r**2))

!pval = 2*min()

!write(*,*)tndf(T,13,n)    IMSL F90 Library 134679Cjh@ 김승황의 이것저것 연구실

write(*,'(2f15.4)')r,T  ! fortran gsl
end