C
C******************************************************************************
C
	SUBROUTINE GIVE_STATISTICS(NB)
	IMPLICIT NONE
	INTEGER NB
C
	INTEGER NBUFFMAX                 !numero de buffers de datos diferentes
	PARAMETER (NBUFFMAX=8)
	INTEGER NDATAMAX
	PARAMETER (NDATAMAX=10000)       !numero maximo de datos en cada buffer
C
	INTEGER I,N
	INTEGER NDATABUFF(NBUFFMAX)
	REAL XDATA(NDATAMAX,NBUFFMAX),EXDATA(NDATAMAX,NBUFFMAX)
	REAL YDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
	REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
	REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
	REAL R,T,ALPHA,FTSTUDENT
	REAL XX(NDATAMAX),YY(NDATAMAX)
	DOUBLE PRECISION X(NDATAMAX),Y(NDATAMAX)
	DOUBLE PRECISION EX(NDATAMAX),EY(NDATAMAX)
	DOUBLE PRECISION XMEAN,XMEANW,YMEAN,YMEANW
	DOUBLE PRECISION VARX,VARY
	DOUBLE PRECISION ERR2,SUMW
	DOUBLE PRECISION SUMX,SUMY,SUMXY
	DOUBLE PRECISION X0,Y0
	LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
	LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
	LOGICAL LXMEANW,LYMEANW
C
	COMMON/BLKXYDATA/XDATA,YDATA
	COMMON/BLKEXYDATA/EXDATA,EYDATA
	COMMON/BLKNDATABUFF/NDATABUFF
	COMMON/BLKLXYERR/LXERR,LYERR
	COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
	COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
C------------------------------------------------------------------------------
	N=NDATABUFF(NB)
	DO I=1,N
	  X(I)=DBLE(XDATA(I,NB))
	  EX(I)=DBLE(EXDATA(I,NB))
	  XX(I)=XDATA(I,NB)
	  Y(I)=DBLE(YDATA(I,NB))
	  EY(I)=DBLE(EYDATA(I,NB))
	  YY(I)=YDATA(I,NB)
	END DO
C..............................................................................
	WRITE(*,'(A,I2,A)') '* Buffer #',NB,':'
C..............................................................................
C numero de puntos y extremos
	WRITE(*,100) '>>> No. of data in buffer: '
	WRITE(*,*) N
	WRITE(*,100) '>>> Xmin, Xmax......: '
	WRITE(*,*) XMINBUFF(NB),XMAXBUFF(NB)
	WRITE(*,100) '>>> Ymin, Ymax......: '
	WRITE(*,*) YMINBUFF(NB),YMAXBUFF(NB)
C..............................................................................
C valores medios y varianzas
	XMEAN=0.D0
	DO I=1,N
	  XMEAN=XMEAN+X(I)
	END DO
	XMEAN=XMEAN/DBLE(N)
	VARX=0.D0
	IF(N.GE.2)THEN
	  DO I=1,N
	    VARX=VARX+(X(I)-XMEAN)*(X(I)-XMEAN)
	  END DO
	  VARX=VARX/DBLE(N-1)
	END IF
	WRITE(*,100) '>>> Xmean, rms......: '
	WRITE(*,*) XMEAN,SQRT(VARX)
c
	YMEAN=0.D0
	DO I=1,N
	  YMEAN=YMEAN+Y(I)
	END DO
	YMEAN=YMEAN/DBLE(N)
	VARY=0.D0
	IF(N.GE.2)THEN
	  DO I=1,N
	    VARY=VARY+(Y(I)-YMEAN)*(Y(I)-YMEAN)
	  END DO
	  VARY=VARY/DBLE(N-1)
	END IF
	WRITE(*,100) '>>> Ymean, rms......: '
	WRITE(*,*) YMEAN,SQRT(VARY)
c
	IF(LXERR(NB))THEN
	  LXMEANW=.TRUE.
	  XMEANW=0.D0
	  SUMW=0.D0
	  DO I=1,N
	    IF(EX(I).GT.0.D0)THEN
	      ERR2=EX(I)*EX(I)
	      XMEANW=XMEANW+X(I)/ERR2
	      SUMW=SUMW+1.D0/ERR2
	    ELSE
	      LXMEANW=.FALSE.
	    END IF
	  END DO
	  IF(LXMEANW)THEN
	    XMEANW=XMEANW/SUMW
	    WRITE(*,100) '>>> Xmean weighted..: '
	    WRITE(*,*) XMEANW
	  ELSE
	    WRITE(*,100) '>>> Xmean weighted..: '
	    WRITE(*,101) 'undefined (there are errX=0)'
	  END IF
	END IF
c
	IF(LYERR(NB))THEN
	  LYMEANW=.TRUE.
	  YMEANW=0.D0
	  SUMW=0.D0
	  DO I=1,N
	    IF(EY(I).GT.0.D0)THEN
	      ERR2=EY(I)*EY(I)
	      YMEANW=YMEANW+Y(I)/ERR2
	      SUMW=SUMW+1.D0/ERR2
	    ELSE
	      LYMEANW=.FALSE.
	    END IF
	  END DO
	  IF(LYMEANW)THEN
	    YMEANW=YMEANW/SUMW
	    WRITE(*,100) '>>> Ymean weighted..: '
	    WRITE(*,*) YMEANW
	  ELSE
	    WRITE(*,100) '>>> Ymean weighted..: '
	    WRITE(*,101) 'undefined (there are errY=0)'
	  END IF
	END IF
C..............................................................................
C Spearman rank correlation test (test no parametrico)
	CALL SPEARMAN(N,XX,YY,R)
	IF(R.EQ.1.0)THEN
	  T=1.E30
	ELSE
	  T=R*SQRT(REAL(N-2)/(1.-R*R))
	END IF
	ALPHA=FTSTUDENT(N-2,T)
	WRITE(*,100) '>>> Spearman rank-order correlation '
	WRITE(*,100) 'coefficient: '
	WRITE(*,*) R
	WRITE(*,100) '>>> t, alpha: '
	WRITE(*,*) T,ALPHA
C..............................................................................
C Pearson Product Moment Correlation (parametric test)
	SUMX=0.D0
	SUMY=0.D0
	SUMXY=0.D0
	DO I=1,N
	  X0=X(I)-XMEAN
	  Y0=Y(I)-YMEAN
	  SUMX=SUMX+X0*X0
	  SUMY=SUMY+Y0*Y0
	  SUMXY=SUMXY+X0*Y0
	END DO
	R=REAL(SUMXY/DSQRT(SUMX*SUMY))
	IF(R.EQ.1.0)THEN
	  T=1.E30
	ELSE
	  T=R*SQRT(REAL(N-2)/(1.-R*R))
	END IF
	ALPHA=FTSTUDENT(N-2,T)
	WRITE(*,100) '>>> Pearson product moment correla. '
	WRITE(*,100) 'coefficient: '
	WRITE(*,*) R
	WRITE(*,100) '>>> t, alpha: '
	WRITE(*,*) T,ALPHA
C..............................................................................
100	FORMAT(A,$)
101	FORMAT(A)
	END
