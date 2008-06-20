C
C******************************************************************************
C Carga un nuevo fichero en el buffer NB. Si el fichero se lee correctamente,
C ISTATUS retorna 1. En caso contrario retorna 0. La variable IMODE indica el
C tipo de lectura a realizar:
C IMODE=1: solo se una variable (eje Y, errY); el eje X es el numero de dato
C IMODE=2: se leen X,errX,Y,errY,name
	SUBROUTINE LEENEWFILE(NB,IMODE,ISTATUS)
	IMPLICIT NONE
	INTEGER NB,IMODE,ISTATUS
C
	INTEGER NBUFFMAX                 !numero de buffers de datos diferentes
	PARAMETER (NBUFFMAX=8)
        INCLUDE 'ndatamax.inc'
C
	INTEGER READI_B
	CHARACTER*255 READC_B
	INTEGER REDSYSTEM
	EXTERNAL REDSYSTEM
	INTEGER TRUEBEG,TRUELEN
C
	INTEGER I
	INTEGER L1,L2
	INTEGER ISYSTEM
	INTEGER NSKIP,NDATA
	INTEGER NNAME,NX,NY,NEX,NEY
	INTEGER NDATABUFF(NBUFFMAX)
	INTEGER ISTATUSEXTRAE
	REAL XDATA(NDATAMAX,NBUFFMAX),EXDATA(NDATAMAX,NBUFFMAX)
	REAL YDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
	REAL FEXTRAE
	REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
	REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
	CHARACTER*20 XYNAME(NDATAMAX,NBUFFMAX)  !OJO: tama~no igual que CEXTRAE
	CHARACTER*20 CEXTRAE                    !funcion para extraer la cadena
	CHARACTER*50 DATAKEY(NBUFFMAX)
	CHARACTER*255 INFILE
	CHARACTER*255 CLINEA
	LOGICAL LOGFILE
	LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
	LOGICAL LXYNAME(NBUFFMAX)
	LOGICAL LUNREAD,LNEXTROW
	LOGICAL LBATCH
C
	COMMON/BLKINFILE/INFILE
	COMMON/BLKNDATABUFF/NDATABUFF
	COMMON/BLKXYDATA/XDATA,YDATA
	COMMON/BLKEXYDATA/EXDATA,EYDATA
	COMMON/BLKXYNAME/XYNAME
	COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
	COMMON/BLKLXYERR/LXERR,LYERR
	COMMON/BLKLXYNAME/LXYNAME
	COMMON/BLKLBATCH/LBATCH
        COMMON/BLKSETTINGS8B/DATAKEY
C------------------------------------------------------------------------------
	ISTATUS=0                          !salvo que se demuestre lo contrario
	LUNREAD=.FALSE.
5	WRITE(*,100) 'New input data file name (wildcars allowed) '
	IF(INFILE.EQ.'none')THEN
	  INFILE=READC_B('*','@')
	ELSE
	  INFILE=READC_B(INFILE,'@')
	END IF
	WRITE(77,101) INFILE(1:TRUELEN(INFILE))
        IF((INDEX(INFILE,'*').NE.0).OR.
     +   (INDEX(INFILE,'?').NE.0))THEN
          L1=TRUEBEG(INFILE)
          L2=TRUELEN(INFILE)
          ISYSTEM=REDSYSTEM('ls '//INFILE(L1:L2)//'\0')
	  GOTO 5
	END IF

	INQUIRE(FILE=INFILE,EXIST=LOGFILE)
	IF(LOGFILE)THEN
	  OPEN(10,FILE=INFILE,STATUS='OLD',FORM='FORMATTED')
	  WRITE(*,100) 'No. of initial rows to be skipped...'
	  NSKIP=READI_B('0')
	  IF(NSKIP.LT.0) NSKIP=0
	  WRITE(77,*) NSKIP
	  IF(NSKIP.GT.0)THEN
	    DO I=1,NSKIP
	      READ(10,*,END=901)
	    END DO
	  END IF
	  WRITE(*,100) 'No. of rows to be read (0=ALL)......'
	  NDATA=READI_B('0')
	  WRITE(77,*) NDATA
	  IF(NSKIP+NDATA.GT.NDATAMAX)THEN
	    WRITE(*,101) 'ERROR: this number of data is too large.'
	    WRITE(*,101) 'You must modify the parameter NDATAMAX.'
	    WRITE(*,100) 'Press <CR> to continue...'
	    IF(LBATCH)THEN
	      WRITE(*,*)
	    ELSE
	      READ(*,*)
	    END IF
	    RETURN
	  END IF
C..............................................................................
	  IF(IMODE.EQ.1)THEN
	    LXERR(NB)=.FALSE.
	    WRITE(*,100) 'Column No. for Y data.......................'
	    NY=READI_B('@')
	    WRITE(77,*) NY
	    WRITE(*,100) 'Column No. for err(Y) data (0=NONE, -N) '
	    NEY=READI_B('0')
	    WRITE(77,*) NEY
	    IF(NEY.LT.0)THEN
	      NEY=-NEY
	      LNEXTROW=.TRUE.
	    ELSE
	      LNEXTROW=.FALSE.
	    END IF
	    LYERR(NB)=(NEY.GT.0)
C..............................................................................
	  ELSEIF(IMODE.EQ.2)THEN
	    WRITE(*,100) 'Column No. for X data.......................'
	    NX=READI_B('@')
	    WRITE(77,*) NX
	    WRITE(*,100) 'Column No. for err(X) data (0=NONE, -N) '
	    NEX=READI_B('0')
	    WRITE(77,*) NEX
	    IF(NEX.LT.0)THEN
	      NEX=-NEX
	      LNEXTROW=.TRUE.
	    ELSE
	      LNEXTROW=.FALSE.
	    END IF
	    LXERR(NB)=(NEX.GT.0)
	    WRITE(*,100) 'Column No. for Y data.......................'
	    NY=READI_B('@')
	    WRITE(77,*) NY
	    WRITE(*,100) 'Column No. for err(Y) data (0=NONE, -N) '
	    NEY=READI_B('0')
	    WRITE(77,*) NEY
	    IF(NEY.NE.0)THEN
	      LYERR(NB)=.TRUE.
	      IF(LNEXTROW)THEN
	        IF(NEY.LT.0)THEN
	          NEY=-NEY
	        ELSE
	          WRITE(*,101) 'ERROR: err(Y) must be in different row!'
	          WRITE(*,100) 'Press <CR> to continue'
	          IF(LBATCH)THEN
	            WRITE(*,*)
	          ELSE
	            READ(*,*)
	          END IF
	          RETURN
	        END IF
	      ELSE
	        IF(NEY.LT.0)THEN
	          IF(NEX.GT.0)THEN
	            WRITE(*,100) 'ERROR: '
	            WRITE(*,101) 'err(X) is not in different row!'
	            WRITE(*,100) 'Press <CR> to continue'
	            IF(LBATCH)THEN
	              WRITE(*,*)
	            ELSE
	              READ(*,*)
	            END IF
	            RETURN
	          ELSE
	            NEY=-NEY
	            LNEXTROW=.TRUE.
	          END IF
	        END IF
	      END IF
	    ELSE
	      LYERR(NB)=.FALSE.
	    END IF
	    WRITE(*,100) 'Column No. for names (0=NONE)...........'
	    NNAME=READI_B('0')
	    WRITE(77,*) NNAME
	    LXYNAME(NB)=(NNAME.GT.0)
	  END IF
C------------------------------------------------------------------------------
	  WRITE(*,100) 'Reading file...'
	  I=0
10	  READ(10,101,END=902) CLINEA
	  I=I+1
C..............................................................................
	  IF(IMODE.EQ.1)THEN
C la variable X es el numero de dato
	    XDATA(I,NB)=REAL(I)
C no hay error en esta variable
	    EXDATA(I,NB)=0.
C leemos variable Y
	    YDATA(I,NB)=FEXTRAE(CLINEA,NY,ISTATUSEXTRAE)
	    IF(ISTATUSEXTRAE.EQ.0) GOTO 903
	    IF(ISTATUSEXTRAE.EQ.-1)THEN
	      I=I-1
	      LUNREAD=.TRUE.
	      GOTO 10
	    END IF
C leemos error en variable Y (si procede)
	    IF(NEY.GT.0)THEN
	      IF(LNEXTROW) READ(10,101,END=904) CLINEA
	      EYDATA(I,NB)=FEXTRAE(CLINEA,NEY,ISTATUSEXTRAE)
	      IF(ISTATUSEXTRAE.EQ.0) GOTO 903
	      IF(ISTATUSEXTRAE.EQ.-1)THEN
	        I=I-1
	        LUNREAD=.TRUE.
	        GOTO 10
	      END IF
	    ELSE
	      EYDATA(I,NB)=0.
	    END IF
C..............................................................................
	  ELSEIF(IMODE.EQ.2)THEN
C leemos variable X
	    XDATA(I,NB)=FEXTRAE(CLINEA,NX,ISTATUSEXTRAE)
	    IF(ISTATUSEXTRAE.EQ.0) GOTO 903
	    IF(ISTATUSEXTRAE.EQ.-1)THEN
	      I=I-1
	      LUNREAD=.TRUE.
	      GOTO 10
	    END IF
C leemos variable Y
	    YDATA(I,NB)=FEXTRAE(CLINEA,NY,ISTATUSEXTRAE)
	    IF(ISTATUSEXTRAE.EQ.0) GOTO 903
	    IF(ISTATUSEXTRAE.EQ.-1)THEN
	      I=I-1
	      LUNREAD=.TRUE.
	      GOTO 10
	    END IF
C leemos el nombre (si procede)
	    IF(NNAME.GT.0)THEN
	      XYNAME(I,NB)=CEXTRAE(CLINEA,NNAME,ISTATUSEXTRAE)
	      IF(ISTATUSEXTRAE.EQ.0) GOTO 903
	    ELSE
	      XYNAME(I,NB)=' '
	    END IF
C leemos error en variable X (si procede)
	    IF(NEX.GT.0)THEN
	      IF(LNEXTROW) READ(10,101,END=904) CLINEA
	      EXDATA(I,NB)=FEXTRAE(CLINEA,NEX,ISTATUSEXTRAE)
	      IF(ISTATUSEXTRAE.EQ.0) GOTO 903
	      IF(ISTATUSEXTRAE.EQ.-1)THEN
	        I=I-1
	        LUNREAD=.TRUE.
	        GOTO 10
	      END IF
	    ELSE
	      EXDATA(I,NB)=0.
	    END IF
C leemos error en variable Y (si procede)
	    IF(NEY.GT.0)THEN
	      IF(LNEXTROW)THEN
	        IF(NEX.EQ.0) READ(10,101,END=904) CLINEA
	      END IF
	      EYDATA(I,NB)=FEXTRAE(CLINEA,NEY,ISTATUSEXTRAE)
	      IF(ISTATUSEXTRAE.EQ.0) GOTO 903
	      IF(ISTATUSEXTRAE.EQ.-1)THEN
	        I=I-1
	        LUNREAD=.TRUE.
	        GOTO 10
	      END IF
	    ELSE
	      EYDATA(I,NB)=0.
	    END IF
C..............................................................................
	  END IF
C------------------------------------------------------------------------------
	  IF(I.EQ.NDATA) GOTO 902
	  GOTO 10
	ELSE
	  WRITE(*,101) 'ERROR: this file does not exist.'
	  WRITE(*,100) 'Press <CR> to continue...'
	  INFILE='none'
	  IF(LBATCH)THEN
	    WRITE(*,*)
	  ELSE
	    READ(*,*)
	  END IF
	END IF
	RETURN
C------------------------------------------------------------------------------
901	CLOSE(10)
	WRITE(*,101) 'ERROR: unexpected end of file reached'
	WRITE(*,100) 'Press <CR> to continue...'
	IF(LBATCH)THEN
	  WRITE(*,*)
	ELSE
	  READ(*,*)
	END IF
	RETURN
C..............................................................................
902	CLOSE(10)
	IF(NDATA.EQ.0)THEN
	  IF(I.EQ.0)THEN
	    WRITE(*,*)
	    WRITE(*,101) 'ERROR: unexpected end of file reached'
	    WRITE(*,100) 'Press <CR> to continue...'
	    IF(LBATCH)THEN
	      WRITE(*,*)
	    ELSE
	      READ(*,*)
	    END IF
	    RETURN
	  ELSE
	    NDATABUFF(NB)=I
	    ISTATUS=1
	    WRITE(*,101) 'File read and closed!'
	  END IF
	ELSE
	  IF(I.NE.NDATA)THEN
	    WRITE(*,*)
	    WRITE(*,101) 'ERROR: unexpected end of file reached'
	    WRITE(*,100) 'Press <CR> to continue...'
	    IF(LBATCH)THEN
	      WRITE(*,*)
	    ELSE
	      READ(*,*)
	    END IF
	    RETURN
	  ELSE
	    NDATABUFF(NB)=I
	    ISTATUS=1
	    WRITE(*,101) 'File read and closed!'
	  END IF
	END IF
C calculamos los limites de los datos leidos
	CALL UPDATELIMITS(NB)
C mostramos datos basicos sobre los puntos leidos
	WRITE(*,100) '>>> No. of rows read: '
	WRITE(*,*) NDATABUFF(NB)
	WRITE(*,100) '>>> Xmin............: '
	WRITE(*,*) XMINBUFF(NB)
	WRITE(*,100) '>>> Xmax............: '
	WRITE(*,*) XMAXBUFF(NB)
	WRITE(*,100) '>>> Ymin............: '
	WRITE(*,*) YMINBUFF(NB)
	WRITE(*,100) '>>> Ymax............: '
	WRITE(*,*) YMAXBUFF(NB)
	IF(LUNREAD)THEN
	  WRITE(*,101) 'WARNING: there were unread data'
	END IF
	RETURN
C..............................................................................
903	CLOSE(10)
	WRITE(*,100) 'ERROR: while reading data #'
	WRITE(*,*) I+1
	WRITE(*,100) 'Press <CR> to continue...'
	IF(LBATCH)THEN
	  WRITE(*,*)
	ELSE
	  READ(*,*)
	END IF
	RETURN
C..............................................................................
904	CLOSE(10)
	WRITE(*,100) 'ERROR: while reading error in different row, '
	WRITE(*,100) 'data #'
	WRITE(*,*) I+1
	WRITE(*,100) 'Press <CR> to continue...'
	IF(LBATCH)THEN
	  WRITE(*,*)
	ELSE
	  READ(*,*)
	END IF
	RETURN
C------------------------------------------------------------------------------
100	FORMAT(A,$)
101	FORMAT(A)
	END
