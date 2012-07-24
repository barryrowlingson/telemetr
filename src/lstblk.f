      integer function lstblk(line)
      character*(*) line
      do 10 i=len(line),1,-1
         if (line(i:i).ne.' ') go to 15
 10      continue
      i=1
 15   lstblk=i
      return
      end
