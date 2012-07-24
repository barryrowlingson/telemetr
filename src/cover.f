      integer function cover(refutm,iloc,coor,vc)
      double precision refutm(3,0:159)
      double precision coor(2),vc(2,2),ci
c   cover=0 means no coverage
      do 10 i=1,2
         if (vc(i,i).le.0.) return
         ci=sqrt(vc(i,i))*1.96
         if ((coor(i)+ci).lt.refutm(i,iloc)
     1       .or. (coor(i)-ci).gt.refutm(i,iloc)) return
 10      continue
      cover=1
      return
      end
