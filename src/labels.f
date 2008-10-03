C
C******************************************************************************
C IMODE=0 pide el nombre de fichero con etiquetas
C IMODE=1 abre el fichero con etiquetas
	SUBROUTINE LABELS(IMODE)
	IMPLICIT NONE
	INTEGER IMODE
C
	INTEGER TRUEBEG,TRUELEN
C
	INTEGER L1,L2
	INTEGER NEXT
	INTEGER NCOLOR,NFONT,OLDCI,OLDCF
	REAL X,Y,ANGLE,FJUST,SIZE,OLDCH
	CHARACTER*255 FILELABELS,CADENA,ETIQUETA
	LOGICAL LBATCH
	LOGICAL LOGFILE
	LOGICAL LLABELS
C
	COMMON/BLKLABELS1/LLABELS
	COMMON/BLKLABELS2/FILELABELS
	COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
	IF(IMODE.EQ.0)THEN
	  WRITE(*,100) 'File must contain: '
	  WRITE(*,101) 'X Y ANGLE FJUST SIZE FONT COLOR LABEL'
	  LOGFILE=.FALSE.
	  DO WHILE(.NOT.LOGFILE)
	    WRITE(*,100) 'File name (<CR>=none) ? '
	    IF(LBATCH)THEN
	      READ(78,101) FILELABELS
	    ELSE
	      READ(*,101) FILELABELS
	      IF(TRUELEN(FILELABELS).EQ.0)THEN
	        RETURN
	      END IF
	    END IF
	    INQUIRE(FILE=FILELABELS,EXIST=LOGFILE)
	    IF(.NOT.LOGFILE)THEN
	      WRITE(*,100) 'ERROR: this file does not exist. '
	      WRITE(*,101) 'Try again.'
	      WRITE(*,100) 'Press <CR> to continue...'
	      IF(LBATCH)THEN
	        WRITE(*,*)
	      ELSE
	        READ(*,*)
	      END IF
	    END IF
	  END DO
	  WRITE(77,101) FILELABELS(1:TRUELEN(FILELABELS))
	  LLABELS=.TRUE.
	  RETURN
	END IF
C
	OPEN(15,FILE=FILELABELS,STATUS='OLD',FORM='FORMATTED',ERR=904)
C
10	READ(15,101,END=900) CADENA
	IF(TRUELEN(CADENA).EQ.0) GOTO 901
        L1=TRUEBEG(CADENA)
        L2=TRUELEN(CADENA)
C
	NEXT=INDEX(CADENA(L1:L2),' ')
        IF(NEXT.EQ.0) GOTO 902
        READ(CADENA(L1:L1+NEXT-2),*,ERR=903) X
        L1=L1+NEXT
        L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
	NEXT=INDEX(CADENA(L1:L2),' ')
        IF(NEXT.EQ.0) GOTO 902
        READ(CADENA(L1:L1+NEXT-2),*,ERR=903) Y
        L1=L1+NEXT
        L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
	NEXT=INDEX(CADENA(L1:L2),' ')
        IF(NEXT.EQ.0) GOTO 902
        READ(CADENA(L1:L1+NEXT-2),*,ERR=903) ANGLE
        L1=L1+NEXT
        L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
	NEXT=INDEX(CADENA(L1:L2),' ')
        IF(NEXT.EQ.0) GOTO 902
        READ(CADENA(L1:L1+NEXT-2),*,ERR=903) FJUST
        L1=L1+NEXT
        L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
	NEXT=INDEX(CADENA(L1:L2),' ')
        IF(NEXT.EQ.0) GOTO 902
        READ(CADENA(L1:L1+NEXT-2),*,ERR=903) SIZE
        L1=L1+NEXT
        L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
	NEXT=INDEX(CADENA(L1:L2),' ')
        IF(NEXT.EQ.0) GOTO 902
        READ(CADENA(L1:L1+NEXT-2),*,ERR=903) NFONT
        L1=L1+NEXT
        L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
	NEXT=INDEX(CADENA(L1:L2),' ')
        IF(NEXT.EQ.0) GOTO 902
        READ(CADENA(L1:L1+NEXT-2),*,ERR=903) NCOLOR
        L1=L1+NEXT
        L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
	ETIQUETA=CADENA(L1:L2)
C
	CALL PGQCF(OLDCF)
	CALL PGQCI(OLDCI)
	CALL PGQCH(OLDCH)
	CALL PGSCF(NFONT)
	CALL PGSCI(NCOLOR)
	CALL PGSCH(SIZE)
	CALL PGPTXT(X,Y,ANGLE,FJUST,ETIQUETA)
	CALL PGSCF(OLDCF)
	CALL PGSCI(OLDCI)
	CALL PGSCH(OLDCH)
C
        GOTO 10

900	CLOSE(15)
	RETURN
C..............................................................................
901	WRITE(*,101) 'ERROR: file contains empty lines'
	CLOSE(15)
	RETURN
C..............................................................................
902	WRITE(*,101) 'ERROR: unexpected end of line.'
	CLOSE(15)
	RETURN
C..............................................................................
903	WRITE(*,101) 'ERROR: while reading number.'
	CLOSE(15)
	RETURN
C..............................................................................
904	WRITE(*,101) 'ERROR: while opening the file.'
	RETURN
C------------------------------------------------------------------------------
100	FORMAT(A,$)
101	FORMAT(A)
	END
