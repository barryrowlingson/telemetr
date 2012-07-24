      subroutine daxpy(n,da,dx,incx,dy,incy)
c***begin prologue  daxpy
c***date written   791001   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  d1a7
c***keywords  blas,double precision,linear algebra,triad,vector
c***author  lawson, c. l., (jpl)
c           hanson, r. j., (snla)
c           kincaid, d. r., (u. of texas)
c           krogh, f. t., (jpl)
c***purpose  d.p computation y = a*x + y
c***description
c
c                b l a s  subprogram
c    description of parameters
c
c     --input--
c        n  number of elements in input vector(s)
c       da  double precision scalar multiplier
c       dx  double precision vector with n elements
c     incx  storage spacing between elements of dx
c       dy  double precision vector with n elements
c     incy  storage spacing between elements of dy
c
c     --output--
c       dy  double precision result (unchanged if n .le. 0)
c
c     overwrite double precision dy with double precision da*dx + dy.
c     for i = 0 to n-1, replace  dy(ly+i*incy) with da*dx(lx+i*incx) +
c       dy(ly+i*incy), where lx = 1 if incx .ge. 0, else lx = (-incx)*n
c       and ly is defined in a similar way using incy.
c***references  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,
c                 *basic linear algebra subprograms for fortran usage*,
c                 algorithm no. 539, transactions on mathematical
c                 software, volume 5, number 3, september 1979, 308-323
c***routines called  (none)
c***end prologue  daxpy
c
      double precision dx(1),dy(1),da
c***first executable statement  daxpy
      if(n.le.0.or.da.eq.0.d0) return
      if(incx.eq.incy) if(incx-1) 5,20,60
    5 continue
c
c        code for nonequal or nonpositive increments.
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop so remaining vector length is a multiple of 4.
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
c
c        code for equal, positive, nonunit increments.
c
   60 continue
      ns = n*incx
          do 70 i=1,ns,incx
          dy(i) = da*dx(i) + dy(i)
   70     continue
      return
      end
