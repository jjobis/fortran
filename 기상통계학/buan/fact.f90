integer(KIND=8) :: c,co,fact
 read(*,*) co
 c = fact(co)
 write(*,*) c
end

function fact(n) result (f)
       integer(KIND=8) :: f , n
       if (n > 1) then
       f = 1
       do i = 2,n
       f = n * f
       end do
       else if ( n == 1 ) then
       f = 1
       end if
       return
end 
