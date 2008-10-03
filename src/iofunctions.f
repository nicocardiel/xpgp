C
C******************************************************************************
C******************************************************************************
C Funciones de entrada/salida por teclado, modificadas para trabajar con un
C fichero de comandos en modo BATH (LBATCH=.TRUE.)
C******************************************************************************
C******************************************************************************
C
	CHARACTER*(*) FUNCTION READC_B(CDEF,CVAL)
	IMPLICIT NONE
	CHARACTER*(*) CDEF,CVAL
C
	INTEGER I,L1,L2
	INTEGER TRUEBEG,TRUELEN
	INTEGER NERR
	CHARACTER*255 CADENA
	LOGICAL LBATCH
C
	COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
	NERR=0
10	IF(CDEF.NE.'@')THEN
	  L1=TRUEBEG(CDEF)
	  IF(L1.NE.0)THEN
	    L2=TRUELEN(CDEF)
	    WRITE(*,100)'['
	    WRITE(*,100)CDEF(L1:L2)
	    WRITE(*,100)'] ? '
	  END IF
	ELSE
	  WRITE(*,100)'? '
	END IF
	IF(LBATCH)THEN
	  READ(78,'(A)',ERR=20) CADENA
	  WRITE(*,'(A)') CADENA(1:TRUELEN(CADENA))
	ELSE
	  READ(*,'(A)',ERR=20) CADENA
	END IF
	IF(CVAL.EQ.'@')THEN
	  IF(TRUELEN(CADENA).EQ.0)THEN
	    IF(CDEF.EQ.'@')THEN
	      GOTO 10
	    END IF
	    CADENA=CDEF(L1:L2)
	  END IF
	ELSE
	  IF(TRUELEN(CADENA).EQ.0)THEN
	    IF(CDEF.EQ.'@')THEN
	      GOTO 10
	    END IF
	    CADENA=CDEF(L1:L2)
	  ELSE
	    DO I=1,TRUELEN(CADENA)
	      IF(INDEX(CVAL,CADENA(I:I)).EQ.0)THEN
	        WRITE(*,101)'ERROR: invalid character(s). Try again.'
		IF(CDEF.EQ.'@') WRITE(*,100)'? '
		NERR=NERR+1
		IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
	        GOTO 10
	      END IF
	    END DO
	  END IF
	END IF
	READC_B=CADENA
	RETURN
20	WRITE(*,101)'ERROR: invalid entry. Try again.'
	IF(CDEF.EQ.'@') WRITE(*,100)'? '
	NERR=NERR+1
	IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
	GOTO 10
100	FORMAT(A,$)
101	FORMAT(A)
	END
C
C******************************************************************************
C
	INTEGER FUNCTION READI_B(CDEF)
	IMPLICIT NONE
	CHARACTER*(*) CDEF
C
	INTEGER I,L1,L2
	INTEGER N
	INTEGER NERR
	INTEGER TRUEBEG,TRUELEN
	CHARACTER*1 C
	CHARACTER*255 CADENA
	LOGICAL LBATCH
C
	COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
	NERR=0
10	IF(CDEF.NE.'@')THEN
	  L1=TRUEBEG(CDEF)
	  IF(L1.NE.0)THEN
	    L2=TRUELEN(CDEF)
	    WRITE(*,100)'['
	    WRITE(*,100)CDEF(L1:L2)
	    WRITE(*,100)'] ? '
	  END IF
	ELSE
	  WRITE(*,100)'? '
	END IF
	IF(LBATCH)THEN
	  READ(78,'(A)',ERR=20)CADENA
	  WRITE(*,'(A)') CADENA(1:TRUELEN(CADENA))
	ELSE
	  READ(*,'(A)',ERR=20)CADENA
	END IF
	IF(TRUELEN(CADENA).EQ.0)THEN
	  IF(CDEF.EQ.'@')THEN
	    GOTO 10
	  END IF
	  CADENA=CDEF
	END IF
	DO I=1,TRUELEN(CADENA)
	  C=CADENA(I:I)
	  IF((INDEX('abcdefghijklmnopqrstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ./',C).NE.0))THEN
	    GOTO 20
	  END IF
	END DO
	READ(CADENA,*,ERR=20) N
	READI_B=N
	RETURN
20	WRITE(*,101)'ERROR: invalid character(s) found in '//
     +   'number. Try again.'
	IF(CDEF.EQ.'@') WRITE(*,100)'? '
	NERR=NERR+1
	IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
	GOTO 10
100	FORMAT(A,$)
101	FORMAT(A)
	END
C
C******************************************************************************
C
	INTEGER FUNCTION READILIM_B(CDEF,N1,N2)
	IMPLICIT NONE
	CHARACTER*(*) CDEF
	INTEGER N1,N2
C
	INTEGER I,L1,L2
	INTEGER N
	INTEGER NERR
	INTEGER TRUEBEG,TRUELEN
	CHARACTER*1 C
	CHARACTER*255 CDUMMY
	CHARACTER*255 CADENA
	LOGICAL LBATCH
C
	COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
	IF(N2.LT.N1)THEN
	  WRITE(*,101)'ERROR: N2.LT.N1 in function: READILIM_B'
	  WRITE(*,101)'=> returned value is 0'
	  READILIM_B=0
	  RETURN
	END IF
	NERR=0
	WRITE(CDUMMY,'(A1,I10,A5,I10,A1)') '(',N1,',...,',N2,')'
	CALL RMBLANK(CDUMMY,CDUMMY,L2)
	WRITE(*,100) ' '//CDUMMY(1:L2)
10	IF(CDEF.NE.'@')THEN
	  L1=TRUEBEG(CDEF)
	  IF(L1.NE.0)THEN
	    L2=TRUELEN(CDEF)
	    WRITE(*,100)' ['
	    WRITE(*,100)CDEF(L1:L2)
	    WRITE(*,100)'] ? '
	  END IF
	ELSE
	  WRITE(*,100)'? '
	END IF
	IF(LBATCH)THEN
	  READ(78,'(A)',ERR=20)CADENA
	  WRITE(*,'(A)') CADENA(1:TRUELEN(CADENA))
	ELSE
	  READ(*,'(A)',ERR=20)CADENA
	END IF
	IF(TRUELEN(CADENA).EQ.0)THEN
	  IF(CDEF.EQ.'@')THEN
	    GOTO 10
	  END IF
	  CADENA=CDEF
	END IF
	DO I=1,TRUELEN(CADENA)
	  C=CADENA(I:I)
	  IF((INDEX('abcdefghijklmnopqrstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ./',C).NE.0))THEN
	    GOTO 20
	  END IF
	END DO
	READ(CADENA,*,ERR=20) N
	READILIM_B=N
C
	IF((N.LT.N1).OR.(N.GT.N2)) GOTO 30
	RETURN
C------------------------------------------------------------------------------
20	WRITE(*,101)'ERROR: invalid character(s) found in '//
     +   'number. Try again.'
C
	NERR=NERR+1
	IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
	GOTO 10
C------------------------------------------------------------------------------
30	WRITE(*,100)'ERROR: invalid number. Valid range is '
	WRITE(CDUMMY,*)N1
	CALL RMBLANK(CDUMMY,CDUMMY,L2)
	WRITE(*,100) CDUMMY(1:L2)//' to '
	WRITE(CDUMMY,*)N2
	CALL RMBLANK(CDUMMY,CDUMMY,L2)
	WRITE(*,101) CDUMMY(1:L2)//'. Try again.'
C
	NERR=NERR+1
	IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
	GOTO 10
100	FORMAT(A,$)
101	FORMAT(A)
	END
C
C******************************************************************************
C
	REAL FUNCTION READF_B(CDEF)
	IMPLICIT NONE
	CHARACTER*(*) CDEF
C
	INTEGER I,L1,L2
	INTEGER NERR
	REAL F
	INTEGER TRUEBEG,TRUELEN
	CHARACTER*1 C
	CHARACTER*255 CADENA
	LOGICAL LBATCH
C
	COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
	NERR=0
10	IF(CDEF.NE.'@')THEN
	  L1=TRUEBEG(CDEF)
	  IF(L1.NE.0)THEN
	    L2=TRUELEN(CDEF)
	    WRITE(*,100)'['
	    WRITE(*,100)CDEF(L1:L2)
	    WRITE(*,100)'] ? '
	  END IF
	ELSE
	  WRITE(*,100)'? '
	END IF
	IF(LBATCH)THEN
	  READ(78,'(A)',ERR=20)CADENA
	  WRITE(*,'(A)') CADENA(1:TRUELEN(CADENA))
	ELSE
	  READ(*,'(A)',ERR=20)CADENA
	END IF
	IF(TRUELEN(CADENA).EQ.0)THEN
	  IF(CDEF.EQ.'@')THEN
	    GOTO 10
	  END IF
	  CADENA=CDEF
	END IF
	DO I=1,TRUELEN(CADENA)
	  C=CADENA(I:I)
	  IF((INDEX('abcfghijklmnoprstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCFGHIJKLMNOPRSTUVWXYZ/',C).NE.0))THEN
	    GOTO 20
	  END IF
	END DO
	READ(CADENA,*,ERR=20) F
	READF_B=F
	RETURN
20	WRITE(*,101)'ERROR: invalid character(s) found in '//
     +   'number. Try again.'
	IF(CDEF.EQ.'@') WRITE(*,100)'? '
	NERR=NERR+1
	IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
	GOTO 10
100	FORMAT(A,$)
101	FORMAT(A)
	END
C
C******************************************************************************
C
	SUBROUTINE RMBLANK(C1,C2,L)
	IMPLICIT NONE
	INTEGER L
	CHARACTER*(*) C1,C2
C
	INTEGER I,K,L0
C------------------------------------------------------------------------------
	K=0
	L0=LEN(C1)
	DO I=1,L0
	  IF(C1(I:I).NE.CHAR(32))THEN
	    K=K+1
	    C2(K:K)=C1(I:I)
	  END IF
	END DO
	L=K
	L0=LEN(C2)
	IF(L.LT.L0)THEN
	  DO I=L+1,L0
	    C2(I:I)=' '
	  END DO
	END IF
	END
