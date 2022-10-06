       real::wd,ws,v,u,cpwd
       real,parameter::pi=4.*atan(1.)
       read(*,*)ws,wd
       u=ws*cos((280.-wd)*(pi/180.))
       v=ws*sin((270.-wd)*(pi/180.))
       cpwd = atan2(v,u)
       write(*,10) u, v, cpwd
10     format(2f10.4)
       end