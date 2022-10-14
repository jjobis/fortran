       real::x,y,z,xn,yn,zn,xe,ye,ze
       real,parameter::a=10.0,b=28.0,c=2.6666,dt=0.01
       integer::i
       write(*,*)"first x,y,z"
       read(*,*)x,y,z
       xe=x
       ye=y
       ze=z

       open(1,file="chaos1.dat")
       open(2,file="chaos2.dat")

       !dt = 0.01

       write(1,10)x,y,z
       do i=1,2000
       xn=x+a*(y-x)*dt
       yn=y+(x*(28-z)-y)*dt
       zn=z+(x*y-c*z)*dt
       x=xn
       y=yn
       z=zn
       write(1,10)xn,yn,zn
10     format(3(f9.3))
       end do

       !x,y,z 초기 좌표 초기화

       x=xe
       y=ye
       z=ze

       !New dt = dt+0.00001

       do i=1,2000
       xn=x+a*(y-x)*(dt+0.00001)
       yn=y+(x*(28-z)-y)*(dt+0.00001)
       zn=z+(x*y-c*z)*(dt+0.00001)
       x=xn
       y=yn
       z=zn
       write(2,20)xn,yn,zn
20     format(3(f9.3))
       end do

       close(1)
       close(2)

       CALL SYSTEM('gnuplot -p chaos.plt')

       end