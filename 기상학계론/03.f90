!planck's law

       integer,parameter:: r8 = selected_real_kind(15,307)
       real(kind=r8)::k,h,c,v,u,T,Blamda,lamda,y,Elamda,a
       real,parameter::ca=3.740d8 !W*m**-2*nm**4
       real,parameter::cb=1.440d4 !nm*K

       open(1,file='Elamda1.dat')
       open(2,file='Elamda2.dat')
       open(3,file='Elamda3.dat')
       
       write(*,*)'T=(K)',ca,cb
       read(*,*)T
       do i =1,10
       a=i*0.1
       lamda=a
       v=lamda*(T*1.0)
       Elamda=ca*exp(-cb/v)/lamda**5
       write(1,10)a,log10(Elamda),exp(-cb/v)
10     format(f5.1,f30.7,f30.7)
       a=0
       end do

       do i =1,10
       a=i*1.0
       lamda=a
       v=lamda*(T*1.0)
       Elamda=ca*exp(-cb/v)/lamda**5
       write(2,20)1+a*0.1,log10(Elamda),exp(-cb/v)
20     format(f5.1,f30.7,f30.7)
       a=0
       end do

       do i =10,100
       a=i*1.0
       lamda=a
       v=lamda*(T*1.0)
       Elamda=ca*exp(-cb/v)/lamda**5
       write(3,30)2+a*0.01,log10(Elamda),exp(-cb/v)
30     format(f5.2,f30.7,f30.7)
       a=0
       end do

       CALL SYSTEM('gnuplot -p Elamda.plt')

       end



