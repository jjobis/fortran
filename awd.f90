real,dimension(2000)::a,b
open(1,file='f.dat')
do i = 0,2000
       a(i)=1-(i*sin((i-1000)*0.1))
end do
do i = 0,2000
       b(i)=1-(i*sin((1000)*0.1))
end do
end