      integer function idamax(n,dx,incx)
c***begin prologue  idamax
c***date written   791001   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  d1a2
c***keywords  blas,double precision,linear algebra,maximum component,
c             vector
c***author  lawson, c. l., (jpl)
c           hanson, r. j., (snla)
c           kincaid, d. r., (u. of texas)
c           krogh, f. t., (jpl)
c***purpose  find largest component of d.p. vector
c***description
c
c                b l a s  subprogram
c    description of parameters
c
c     --input--
c        n  number of elements in input vector(s)
c       dx  double precision vector with n elements
c     incx  storage spacing between elements of dx
c
c     --output--
c   idamax  smallest index (zero if n .le. 0)
c
c     find smallest index of maximum magnitude of double precision dx.
c     idamax =  first i, i = 1 to n, to minimize  abs(dx(1-incx+i*incx)
c***references  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,
c                 *basic linear algebra subprograms for fortran usage*,
c                 algorithm no. 539, transactions on mathematical
c                 software, volume 5, number 3, september 1979, 308-323
c***routines called  (none)
c***end prologue  idamax
c
      double precision dx(1),dmax,xmag
c***first executable statement  idamax
      idamax = 0
      if(n.le.0) return
      idamax = 1
      if(n.le.1)return
      if(incx.eq.1)goto 20
c
c        code for increments not equal to 1.
c
      dmax = dabs(dx(1))
      ns = n*incx
      ii = 1
          do 10 i = 1,ns,incx
          xmag = dabs(dx(i))
          if(xmag.le.dmax) go to 5
          idamax = ii
          dmax = xmag
    5     ii = ii + 1
   10     continue
      return
c
c        code for increments equal to 1.
c
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
          xmag = dabs(dx(i))
          if(xmag.le.dmax) go to 30
          idamax = i
          dmax = xmag
   30 continue
      return
      end
