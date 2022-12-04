real,dimension(1200)::x,y
open(1,file='1901-2000all.csv')
open(2,file='data_all.txt')
do i = 1,1200
       read(1,*)x(i),y(i)
       x(i) = i
       write(2,*)x(i),y(i)
end do
end