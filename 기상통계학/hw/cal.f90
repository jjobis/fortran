real,parameter::a = -0.3739,b = 0.00062258
real :: f
!result = f
f = 0
do i = 1789,1800
f = a + b*i + f
end do
f = f/12.
write(*,*)f
end