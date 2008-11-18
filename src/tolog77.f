C Funcion para escribir en el log-file la pulsación de un botón
        SUBROUTINE TOLOG77(NB,CTEXT)
        IMPLICIT NONE
        INTEGER NB
        CHARACTER*(*) CTEXT
C
        INTEGER TRUELEN,TRUEBEG
C
        INTEGER I
        INTEGER L1,L2
        CHARACTER*66 CLINE
        CHARACTER*79 CSEPARATOR
C------------------------------------------------------------------------------
        CLINE(1:1)='#'
        DO I=2,66
          CLINE(I:I)=' '
        END DO
        DO I=1,79
          CSEPARATOR(I:I)='#'
        END DO
C
        L1=TRUEBEG(CTEXT)
        L2=TRUELEN(CTEXT)
        WRITE(77,101) CSEPARATOR
        WRITE(77,'(I3.3,10X,A)') NB,CLINE(1:66-(L2-L1+1))//CTEXT(L1:L2)
        WRITE(77,101) CSEPARATOR
C
101     FORMAT(A)
        END
C
C******************************************************************************
C Funcion para escribir en el log-file una cadena de caracteres
C
        SUBROUTINE TOLOG77_STRING(CSTRING,CTEXT)
        IMPLICIT NONE
        CHARACTER*(*) CSTRING
        CHARACTER*(*) CTEXT
C
        INTEGER TRUELEN,TRUEBEG
C
        INTEGER L1,L2,DL
        INTEGER LL1,LL2
        CHARACTER*12 CBLANK
C------------------------------------------------------------------------------
        CBLANK='            '
        L1=TRUEBEG(CSTRING)
        L2=TRUELEN(CSTRING)
        DL=L2-L1+1
        LL1=TRUEBEG(CTEXT)
        LL2=TRUELEN(CTEXT)
        IF(DL.LT.12)THEN
          WRITE(77,100) CSTRING(L1:L2)
          WRITE(77,100) CBLANK(1:12-DL)
          WRITE(77,100) ' # '
          WRITE(77,101) CTEXT(LL1:LL2)
        ELSE
          WRITE(77,100) CSTRING(L1:L2)
          WRITE(77,100) ' # '
          WRITE(77,101) CTEXT(LL1:LL2)
        END IF
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
