       implicit none

       integer npts, i
       parameter (npts = 10001)
       double precision elam(npts),b(npts),x(npts),bt(npts)
       double precision alam1, alam2, dlam, dlamcm
       double precision peak, ppeak, flux, pflux, Teff
       double precision c, h, k, pi, pi4, sigma, eV, a  
       double precision c1, c2, p, uden, prad, phden, Ebar, zeta3
       character*30 outfil

! some constants CODATA (2006)

       c = 2.99792458d10    ! speed of light in the vacuum
       h = 6.62606896d-27    ! Planck's constant
       k = 1.3806504d-16     ! Boltzmann's constant
       eV = 1.602176487d-12  ! ergs/eV
!
       pi = 3.1415926535897932d0  ! you had better know what this is
       pi4 = pi*pi*pi*pi
       c1 = pi * 2.d0*h*c*c ! a useful constant, factor of pi from solid angle
       c2 = ( h * c ) / k      ! another useful constant
       sigma = (pi4/15.d0)*c1/(c2*c2*c2*c2)  ! Stefan-Boltzmann constant
       a = (4.d0)*sigma/c       ! Stefan's constant
       zeta3 = 1.2020569031595d0  ! Riemann zeta function n=3
       p = 4.d0*pi * zeta3 * c / (c2*c2*c2)  ! photon emission constant

! prompt user for input...

       write(*,*)'  set name of output file... '
       write(*,*)' '
       write(*,'(a)')'      output file name... '
       read(*,'(a)') outfil
       write(*,*)' '
111    write(*,*)'  set temperature (kelvins)... '
       write(*,*)' '       
       write(*,'(a)')'      temperature... '
       read(*,*) Teff
       if(Teff .gt. 1.d10) then
         write(*,*)'  that is insane! choose a lower temperature '
         goto 111
       endif
!
!  compute the Wien Displacement Law Peak, and other characteristics
!
       peak =  c2 * 0.2014052d8/Teff  ! Angstroms, peak of F_lambda
       ppeak = c2 * 0.2550571d8/Teff  ! Angstroms, peak of photon flux
       flux = sigma * Teff * Teff * Teff * Teff  ! the surface energy flux F
       pflux = p*Teff*Teff*Teff  ! the surface photon flux
       uden = a*Teff*Teff*Teff*Teff
       prad = uden/3.d0
       Ebar = (pi4/15.d0) * k*Teff/(2.d0*zeta3) / eV
       phden = 8.*pi*(k/(h*c))*(k/(h*c))*(k/(h*c))*2.d0*zeta3&
             *Teff*Teff*Teff

       write(*,*)' '
       write(*,*)'  set wavelength range (Angstroms)... '
       write(*,*)' '
       write(6,222) peak
222    format('your BB lambda_max = ', 1p, e10.4, ' Angstroms')
       write(*,'(a)')'choose a starting wavelength about 5x smaller... '
       read(*,*) alam1
       print *

c the wavelength intervals in Angstroms determined by placing 
c 500 points between starting wavelength and peak wavelength
       dlam = (peak-alam1)/500.d0
       dlamcm = dlam / 1.d8

c compute the ending wavelength in Angstroms
       alam2 = peak + (npts - 1. - 500.) * dlam
       write(6,333) alam2
333   format('your ending wavelength will be ', 1p, e10.4, ' Angstroms')
       write(6,*)'will now create a 10000 point spectrum...'
       print *
c
c  compute blackbody function
c
c do the first point first

       elam(1) = alam1/1.d8  ! want wavelengths in cm for bb flux calc.

       b(1) = c1/(elam(1)*elam(1)*elam(1)*elam(1)*elam(1))
       x(1) = dexp( min (500.d0, c2/(Teff*elam(1)) ) )
       bt(1) = b(1)/(x(1) - 1.0d0)
         if(bt(1) .lt. 1.d-200) then
           bt(1) = 1.d-200
         endif
       bt(1) = bt(1) / 1.0d8   ! flux per Angstrom wavelength interval

c now do the rest

       do i=2,npts

         elam(i) = elam(i-1) + dlamcm  ! wavelength in cm for bb flux calc.

         b(i) = c1/(elam(i)*elam(i)*elam(i)*elam(i)*elam(i))
         x(i) = dexp( min (500.d0, c2/(Teff*elam(i)) ) )
         bt(i) = b(i)/(x(i) - 1.0d0)
           if(bt(i) .lt. 1.d-200) then
             bt(i) = 1.d-200
           endif
         bt(i) = bt(i) / 1.0d8    ! flux per Angstrom wavelength interval

       end do

c
c  write to output file
c
       open(unit=17,file=outfil,status='unknown')

       write(17,17) Teff
 17   format('# cgs blackbody spectrum pi*B_lambda, T = ', 1p,e11.5)

       write(17,18) peak
 18    format('# Energy lambda_max (Wien) = ', 1p, e10.4, ' Angstroms')

       write(17,181) ppeak
 181   format('# Photon lambda_max = ', 1p, e10.4, ' Angstroms')

       write(17,19) flux
 19    format('# cgs BB flux pi*B = ', 1p, e10.4, ' ergs/s/cm^2')

       write(17,39) pflux
 39    format('# cgs BB photon flux = ', 1p, e10.4, ' photons/s/cm^2')

       write(17,191) uden
 191   format('# cgs BB energy density = ', 1p, e10.4, ' ergs/cm^3')

       write(17,192) prad
 192   format('# cgs BB rad. pressure = ', 1p, e10.4, ' dynes/cm^2')

       write(17,193) Ebar
 193   format('# mean energy per photon = ', 1p, e10.4, ' eV')

       write(17,194) phden
 194   format('# photon number density = ', 1p, e10.4, ' photons/cm^3')

       write(17,20)
 20    format('#   lambda(Angstroms)  ergs/s/cm^2/Angstrom') 

       write(17,21) (elam(i)*1.d8,bt(i),i=1,npts)
 21    format(5x,1p,e14.7,5x,e14.7)

       close(unit=17)
       write(6,*)'....all ok, stopping'
       print *

       stop
       end