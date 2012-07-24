      SUBROUTINE CVTCAS(STRING)
      CHARACTER STRING*(*)
C
C   CASE CONVERSION ROUTINE
C
      INTEGER WANTA,CHANGA,ICHR,I
      WANTA=ICHAR('A')
      CHANGA=ICHAR('a')
      IF (WANTA.EQ.CHANGA) THEN
         WRITE(*,*) 'Error in case conversion routine -- both '
     1      //'cases the same.'
         STOP 'TRIANG bombed'
      ENDIF
      DO 10 I=1,LEN(STRING)
         ICHR=ICHAR(STRING(I:I))
         IF (ICHR.GE.CHANGA .AND. ICHR.LE.(CHANGA+25)) THEN
            STRING(I:I)=CHAR(ICHR-CHANGA+WANTA)
         ENDIF
 10      CONTINUE
      RETURN
      END
