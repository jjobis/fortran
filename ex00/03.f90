!ex03
       real,dimension(4,4)::a
       real,dimension(4)::L,M
       data a/1.4,4.2,7.3,9.0,5.7,7.1,6.3,9.1,2.6,3.8,2.5,6.7,4.5,5.1,7.3,8.2/
       
       do i=1,4
       L(i)=0
              do j=1,4
              L(i)=L(i)+a(i,j)
              end do
       end do

       do i=1,4
       M(i)=1
              do j=1,4
              M(i)=M(i)*a(j,i)
              end do
       end do
       
       do i=1,4
       write(*,10)(a(i,j),j=1,4),L(i)
       end do
10     format(5f10.4)
       write(*,20)(M(i),i=1,4)
20     format(5f10.4)
       end
