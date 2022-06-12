!wordmaker v0.0.1
       integer::word_read_count
       write(*,*)'word_read_count'
       read(*,*)word_read_count
       call word_number_reading(word_read_count)
       end 
!______________________________________________________

       subroutine word_number_reading(a)
       integer::a
       integer,dimension(a)::b
       call read_word_number(a,b)
       call line_change
       do i=1,a
              if(b(i)==1)then
                     write(*,*)'a'
              else if(b(i)==2)then
                     write(*,*)'b'
              else if(b(i)==3)then
                     write(*,*)'c'
              else if(b(i)==4)then
                     write(*,*)'d'
              else if(b(i)==5)then
                     write(*,*)'e'
              else if(b(i)==6)then
                     write(*,*)'f'
              else if(b(i)==7)then
                     write(*,*)'g'
              else if(b(i)==8)then
                     write(*,*)'h'
              else if(b(i)==9)then
                     write(*,*)'i'
              else if(b(i)==10)then
                     write(*,*)'j'
              else if(b(i)==11)then
                     write(*,*)'k'
              else if(b(i)==12)then
                     write(*,*)'l'
              else if(b(i)==13)then
                     write(*,*)'m'
              else if(b(i)==14)then
                     write(*,*)'n'
              else if(b(i)==15)then
                     write(*,*)'o'
              else if(b(i)==16)then
                     write(*,*)'p'
              else if(b(i)==17)then
                     write(*,*)'q'
              else if(b(i)==18)then
                     write(*,*)'r'
              else if(b(i)==19)then
                     write(*,*)'s'
              else if(b(i)==20)then
                     write(*,*)'t'
              else if(b(i)==21)then
                     write(*,*)'u'
              else if(b(i)==22)then
                     write(*,*)'v'
              else if(b(i)==23)then
                     write(*,*)'w'
              else if(b(i)==24)then
                     write(*,*)'x'
              else if(b(i)==25)then
                     write(*,*)'y'
              else if(b(i)==26)then
                     write(*,*)'z'
              end if
       end do
       end subroutine word_number_reading
!______________________________________________________

       subroutine read_word_number(a,b)
       integer::a
       integer,dimension(a)::b
       write(*,*)'read_word_number 1~26 -> a~z'
       do i=1,a
       read(*,*)b(i)
       end do
       end subroutine read_word_number
!______________________________________________________

       subroutine line_change
       write(*,10)
10     format(/)
       end subroutine line_change
!______________________________________________________