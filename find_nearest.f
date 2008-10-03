C
C******************************************************************************
C
	SUBROUTINE FIND_NEAREST(XC,YC)
	IMPLICIT NONE
	REAL XC,YC
C
        INTEGER NBUFFMAX                 !numero de buffers de datos diferentes
        PARAMETER (NBUFFMAX=8)
        INTEGER NDATAMAX                 !numero maximo de datos en cada buffer
        PARAMETER (NDATAMAX=10000)
C
	INTEGER TRUEBEG,TRUELEN
C
	INTEGER NB,I,IMIN
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER L1,L2
        REAL XDATA(NDATAMAX,NBUFFMAX),YDATA(NDATAMAX,NBUFFMAX)
	REAL DMIN,DNEW
        CHARACTER*20 XYNAME(NDATAMAX,NBUFFMAX)
	CHARACTER*50 CDUMMY
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LXYNAME(NBUFFMAX)
C
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKXYNAME/XYNAME
        COMMON/BLKLXYNAME/LXYNAME
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
C------------------------------------------------------------------------------
	WRITE(*,*)
	DO NB=1,NBUFFMAX
	  IF(LUSEBUFF(NB))THEN
	    IMIN=1
	    DMIN=(XDATA(1,NB)-XC)**2+(YDATA(1,NB)-YC)**2
	    IF(NDATABUFF(NB).GT.1)THEN
	      DO I=2,NDATABUFF(NB)
	        DNEW=(XDATA(I,NB)-XC)**2+(YDATA(I,NB)-YC)**2
	        IF(DNEW.LT.DMIN)THEN
	          DMIN=DNEW
	          IMIN=I
	        END IF
	      END DO
	    END IF
	    WRITE(*,'(A10,I1,$)') '> Buffer #',NB
	    WRITE(*,100) ': nearest point is #'
	    WRITE(CDUMMY,*) IMIN
            L1=TRUEBEG(CDUMMY)
            L2=TRUELEN(CDUMMY)
	    WRITE(*,100) CDUMMY(L1:L2)
            IF(LXYNAME(NB))THEN
              WRITE(*,100) ' Object: '
              L1=TRUEBEG(XYNAME(IMIN,NB))
              L2=TRUELEN(XYNAME(IMIN,NB))
              WRITE(*,100) XYNAME(IMIN,NB)(L1:L2)
            END IF
	    WRITE(*,100) ' at ('
	    WRITE(CDUMMY,*) XDATA(IMIN,NB)
            L1=TRUEBEG(CDUMMY)
            L2=TRUELEN(CDUMMY)
	    WRITE(*,100) CDUMMY(L1:L2)
	    WRITE(*,100) ','
	    WRITE(CDUMMY,*) YDATA(IMIN,NB)
            L1=TRUEBEG(CDUMMY)
            L2=TRUELEN(CDUMMY)
	    WRITE(*,100) CDUMMY(L1:L2)
	    WRITE(*,101) ')'
	  END IF
	END DO
C
100	FORMAT(A,$)
101	FORMAT(A)
	END
