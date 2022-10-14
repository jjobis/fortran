       integer,parameter:: r4 = selected_real_kind(6,37)
       integer,parameter:: r8 = selected_real_kind(15,307)
       real(kind=r8)::k,h,c,v,u,T,Blamda,lamda,y



       open(1,file='blamda.dat')
       k=1.380649e-23 !J·K⁻¹
       h=6.6260715e-34 !J·s
       c=299792458.0 !m·s⁻¹
       write(*,*)k,c,h
       write(*,*)'T='
       read(*,*)T

       do i = 1,1000,1
              lamda = i * 1.0d-8
              !v=h*(c*1.0)
              !u=lamda*k*T
              Blamda = (2.0d0*h*c*c)/(lamda**5.0d0)*(exp((h*c)/(lamda*k*T))-1.0d0)
              write(1,*) lamda,log(Blamda)
       end do

       end

       !lamda = i * 1.0d-8
       !Blamda = (2.0d0*h*c*c)/(lamda**5.0d0)*(exp((h*c)/(lamda*k*T))-1.0d0)
