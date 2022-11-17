만약 숫자가 같다면
n = 1, 588

숫자 하나 비교할때마다 co + 1 
-> co 순위


만약에 숫자가 같다면 같은 수 많큼 -> 1 순위 + 2 순위 + 3 순위

같은 수 n + 
real,dimension(588) :: list
real,dimension(588) :: ranklist
integer :: co , n , w, y
       co = 0 ; n = 0 ; w = 0 ; y = 0
       do i = 1,587
       if ( i >= co ) then
         if (list(i) == list(i+1)) then
         co = co + 2
         y = co
         n = n + 1
         w = c
           do j = co , 587
           if (list(j) == list(j+1))then
           co = co + 2
           n = n + 1
           y = co + y
           do k = w,co
           ranklist(k) = y / (n * 1.0)
           end do
           end if
           end do
         do j = i,co
         ranklist(j) = ((co-1)+co)/2.
         end do
         else if (list(i) /= list(i+1))then
         co = co + 1
         ranklist(i) = co
         end if
       end if
       end do

           