       real::x,y,z,xnew,ynew,znew,xe,ye,ze
       real,parameter::a=10.0,b=28.0,c=2.6666,dt=0.01
       integer::i
       write(*,*)"give the value of x,y,z"
       read(*,*)x,y,z
       xe=x
       ye=y
       ze=z

       open(16,file="chaos1.dat")
       open(17,file="chaos2.dat")

       write(16,10)x,y,z
       do i=1,2000
       xnew=x+a*(y-x)*dt
       x=xnew
       ynew=y+(x*(28-z)-y)*dt
       y=ynew
       znew=z+(x*y-c*z)*dt
       z=znew
       write(16,10)xnew,ynew,znew
10     format(3(f9.3))
       end do

       x=xe
       y=ye
       z=ze

       do i=1,2000
       xnew=x+a*(y-x)*(dt+0.00001)
       x=xnew
       ynew=y+(x*(28-z)-y)*(dt+0.00001)
       y=ynew
       znew=z+(x*y-c*z)*(dt+0.00001)
       z=znew
       write(17,20)xnew,ynew,znew
20     format(3(f9.3))
       end do

       close(16)
       close(17)

       CALL SYSTEM('gnuplot -p chaos.plt')

       end