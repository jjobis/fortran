       real::x,y,z,xnew,ynew,znew
       real,parameter::a=10.0,b=28.0,c=2.6666,dt=0.010001
       integer::i
       print*,"give the value of x,y,z"
       read*,x,y,z
       !print*,"give the value of dt"
       !read*,dt

       open(16,file="chaos2.dat")
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



       close(16)

       end