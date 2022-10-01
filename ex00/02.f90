!ex02
       real::a
       read(*,*)a
       write(*,10)a,sin(a/180*3.14),a,cos(a/180*3.14)
10     format('sin',f6.2,'=',f6.3,1x,'cos',f5.2,'=',f6.3)
       end