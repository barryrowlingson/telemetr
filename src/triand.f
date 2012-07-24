      subroutine triand(tutm,az,itower,ntower,coor,sd,kappa,vc,
     1   ijob,ierr)
      implicit double precision (a-h,o-z)
      double precision tutm(2,*),az(*),coor(2),kappa,vc(2,2),sd
      double precision a(2,2),b(2),z(2),rcond,
     1   si,sistar,ci,cistar,di,zi,mui
      double precision wi,t,tuncnt
      double precision cbar, costu, sumwi, kapinv
      integer ipvt(2),iterno,ijob,ierr,itower(ntower)
      logical inital,convrg
      common /param/ pi
      double precision pi
c   tuncnt - tuning constant for m-estimation
      data tuncnt/1.5/
c
c   calling arguments
c
c   variable   contents
c   tutm       true utm coordinates of tower locations
c   az         bearing read from each of these towers
c   ntower     number of towers from which bearings were recorded
c   coor       estimates of x and y coordinates (output)
c   kappa      depending on ijob, an estimate of kappa
c   vc         variance-covariance matrix of location estimates
c   ijob       if 0, kappa is estimated, otherwise the value of
c              kappa on input is used for estimation of coor
c   ierr       error return
c               0 - no errors, coor, vc, and possibly kappa returned
c               1 - iteration failed to converge, zeros returned for
c                   coor, vc, and possibly kappa
c               2 - singular system, zeros returned for coor, vc, and
c                   possibly kappa.
c               3 - coor probably okay, but kappa and vc wrong.
c               4 - coor and kappa probably okay, but vc suspect.
c
c
c   missing values for any of the estimated parameters are
c   indicated by zeros on return.
c
c-----------------------------------------------------------------------
c
c
c   set kappa from sd
c
      if (ijob.ne.0) then
         kappa=exp((sd*pi/180.d0)**2*(-0.5d0))
         kappa=1.d0/(2.d0*(1.d0-kappa)+(1.-kappa)**2*(0.48794d0
     1      - 0.82905d0*kappa - 1.3915*kappa**2)/kappa)
      else
         kappa=0.d0
      endif
c
c   initial coor to missing values
c
      do 10 i=1,2
         coor(i)=0.d0
         do 10 j=1,2
 10         vc(j,i)=0.d0
      iterno=0
      ierr=1
      convrg=.false.
      inital=.true.
 12   do 15 i=1,2
         b(i)=0.d0
         do 15 j=1,2
 15         a(i,j)=0.d0
      do 20 i=1,ntower
         si=sin(az(itower(i)))
         ci=cos(az(itower(i)))
         zi=si*tutm(1,itower(i))-ci*tutm(2,itower(i))
         if (inital) then
            wi=1.d0
            sistar=si
            cistar=ci
         else
            di=sqrt((coor(1)-tutm(1,itower(i)))**2
     1             +(coor(2)-tutm(2,itower(i)))**2)
            sistar=(coor(2)-tutm(2,itower(i)))/di**3
            cistar=(coor(1)-tutm(1,itower(i)))/di**3
            mui=atan2(coor(2)-tutm(2,itower(i)),
     1                coor(1)-tutm(1,itower(i)))
            costu=cos(az(itower(i))-mui)
            t=sqrt(max(0.d0,2.d0*kappa*(1.d0-costu)))
            if (abs(t).lt.tuncnt*pi) then
               if (abs(t).gt.1.d-5) then
                  wi=tuncnt*sin(t/tuncnt)/t
               else
                  wi=1.d0
               endif
            else
               wi=0.d0
            endif
         endif
         a(1,1)=a(1,1)+wi*si*sistar
         a(2,2)=a(2,2)+wi*ci*cistar
         a(1,2)=a(1,2)-wi*ci*sistar
         a(2,1)=a(2,1)-wi*si*cistar
         b(1)=b(1)+wi*sistar*zi
         b(2)=b(2)-wi*cistar*zi
 20   continue
c
c   calculate (x,y) for this iteration
c
      call dgeco(a,2,2,ipvt,rcond,z)
      if ((1.d0+rcond).eq.1.d0) then
         coor(1)=0.d0
         coor(2)=0.d0
         if (ijob.eq.0) kappa=0.d0
         ierr=2
         if (ijob.eq.0) kappa=0.d0
         return
      endif
      call dgesl(a,2,2,ipvt,b,0)
c
c   calculate kappa for this iteration
c
      if (ijob.eq.0) then
         cbar=0.d0
         sumwi=0.d0
         do 35 i=1,ntower
            mui=atan2(b(2)-tutm(2,itower(i)),b(1)-tutm(1,itower(i)))
            costu=cos(az(itower(i))-mui)
            if (inital) then
               wi=1.d0
            else
               t=sqrt(max(0.d0,2.d0*kappa*(1.d0-costu)))
               if (abs(t).lt.tuncnt*pi) then
                  if (abs(t).gt.1.d-5) then
                     wi=tuncnt*sin(t/tuncnt)/t
                  else
                     wi=1.d0
                  endif
               else
                  wi=0.d0
               endif
            endif
            cbar=cbar+wi*costu
            sumwi=sumwi+wi
 35         continue
         if (sumwi.gt.0.d0) then
            cbar=cbar/sumwi
         else
            cbar=0.5d0
         endif
         kapinv=(2.d0*(1.d0-cbar)+((1.d0-cbar))**2*
     1      (.48794d0-.82905d0*cbar-1.3915d0*cbar*cbar)/cbar)
         if (kapinv.gt.0.d0) then
            kappa=1.d0/kapinv
            ierr=1
         else
            ierr=3
            kappa=500.d0
         endif
      endif
      if (inital) then
         inital=.false.
      else
c
c      check for convergence
c
         if (abs(b(1)-coor(1))/b(1) .lt. 1.d-5
     1      .and. abs(b(2)-coor(2))/b(2) .lt. 1.d-5) then
            convrg=.true.
         endif
      endif
      coor(1)=b(1)
      coor(2)=b(2)
      iterno=iterno+1
      if (.not.convrg .and. iterno.lt.101) go to 12
c
c   convergence achieved -- calculate variance-covariance matrix
c
      ierr=0
      do 25 i=1,ntower
         si=sin(az(itower(i)))
         ci=cos(az(itower(i)))
         di=sqrt((coor(1)-tutm(1,itower(i)))**2
     1           +(coor(2)-tutm(2,itower(i)))**2)
         sistar=(coor(2)-tutm(2,itower(i)))/di**3
         cistar=(coor(1)-tutm(1,itower(i)))/di**3
         mui=atan2(coor(2)-tutm(2,itower(i)),coor(1)-tutm(1,itower(i)))
         costu=cos(az(itower(i))-mui)
         t=sqrt(max(0.d0,2.d0*kappa*(1.d0-costu)))
         if (abs(t).lt.1.d-5) then
            wi=1.d0
         else
            if (abs(t).lt.tuncnt*pi) then
               if (abs(t).gt.1.d-5) then
                  wi=tuncnt*sin(t/tuncnt)/t
               else
                  wi=1.d0
               endif
            else
               wi=0.d0
            endif
         endif
         vc(1,1)=vc(1,1)+wi*si*sistar
         vc(1,2)=vc(1,2)+wi*sistar*ci+cistar*si
         vc(2,2)=vc(2,2)+wi*cistar*ci
 25      continue
      vc(1,2)=vc(1,2)*(-0.5)
      vc(2,1)=vc(1,2)
      call dgeco(vc,2,2,ipvt,rcond,z)
      if ((1.d0+rcond).eq.1.d0) then
         vc(1,1)=0.d0
         vc(1,2)=0.d0
         vc(2,1)=0.d0
         vc(2,2)=0.d0
         ierr=4
         if (ijob.eq.0) kappa=0.d0
         return
      endif
      call dgedi(vc,2,2,ipvt,rcond,z,1)
      vc(1,2)=vc(1,2)/kappa
      vc(2,1)=vc(1,2)
      vc(1,1)=vc(1,1)/kappa
      vc(2,2)=vc(2,2)/kappa
      return
      end
