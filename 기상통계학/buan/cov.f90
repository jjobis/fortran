real,dimension(15)::x,y
real :: xmean,ymean,rf,rs,rt,r,T,pval
open(1,file='a.dat')
read(1,*)(x(i),i=1,15)
read(1,*)(y(i),i=1,15)

xmean = 0 ; ymean = 0

do i = 1,15
xmean = x(i) + xmean
ymean = y(i) + ymean
end do

xmean = xmean/15.
ymean = ymean/15.

write(*,*)xmean,ymean

rf=0;rs=0;rt=0;r=0

do i = 1,15
rf = (x(i) - xmean)*(y(i)-ymean)+rf
rs = (x(i) - xmean)**2+rs
rt = (y(i) - ymean)**2+rt
end do

write(*,*)rf/14.,sqrt(rs/14.),sqrt(rt/14.)

rf = rf/14.
rs = sqrt(rs/14.)
rt = sqrt(rt/14.)

r = rf/(rs*rt)

T = r*sqrt((13.))/sqrt((1-r**2))

pval = 2*min()

write(*,'(2f15.8)')r,T
end