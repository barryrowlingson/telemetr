      subroutine dscal(n,da,dx,incx)
c***begin prologue  dscal
c***date written   791001   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  d1a6
c***keywords  blas,linear algebra,scale,vector
c***author  lawson, c. l., (jpl)
c           hanson, r. j., (snla)
c           kincaid, d. r., (u. of texas)
c           krogh, f. t., (jpl)
c***purpose  d.p. vector scale x = a*x
c***description
c
c                b l a s  subprogram
c    description of parameters
c
c     --input--
c        n  number of elements in input vector(s)
c       da  double precision scale factor
c       dx  double precision vector with n elements
c     incx  storage spacing between elements of dx
c
c     --output--
c       dx  double precision result (unchanged if n.le.0)
c
c     replace double precision dx by double precision da*dx.
c     for i = 0 to n-1, replace dx(1+i*incx) with  da * dx(1+i*incx)
c***references  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,
c                 *basic linear algebra subprograms for fortran usage*,
c                 algorithm no. 539, transactions on mathematical
c                 software, volume 5, number 3, september 1979, 308-323
c***routines called  (none)
c***end prologue  dscal
c
      double precision da,dx(1)
c***first executable statement  dscal
      if(n.le.0)return
      if(incx.eq.1)goto 20
c
c        code for increments not equal to 1.
c
      ns = n*incx
          do 10 i = 1,ns,incx
          dx(i) = da*dx(i)
   10     continue
      return
c
c        code for increments equal to 1.
c
c
c        clean-up loop so remaining vector length is a multiple of 5.
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
