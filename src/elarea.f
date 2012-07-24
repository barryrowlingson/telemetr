      double precision function elarea(vc)
      implicit double precision (a-h,o-z)
      double precision pi
      common /param/ pi
      double precision vc(2,2)
      elarea=vc(1,1)*vc(2,2)-vc(1,2)*vc(2,1)
      if (elarea.gt.0.) then
         elarea=0.0001d0*pi*sqrt(elarea)*5.99
      else
         elarea=0.d0
      endif
      return
      end
