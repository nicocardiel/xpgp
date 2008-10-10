C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE PSEUDOFIT(XF,YF,EYF,NF,NTERMS,YRMSTOL,NEVALMAX,
C                      WEIGHT,POWER,LUP,TSIGMA,A)
C
C Input: XF,YF,EYF,NF,NTERMS,YRMSTOL,WEIGHT,POWER,LUP,CERR
C Output: A
C
C Calculate the polynomial fit to the upper/lower side of a set of data
C points.
C
C REAL XF(NF),YF(NF) -> data points to be fitted
C INTEGER NF -> number of data points
C INTEGER NTERMS -> number of coeffcients
C REAL YRMSTOL -> stopping criterion for DOWNHILL
C INTEGER NEVALMAX -> allowed maximum number of iterations for DOWNHILL
C REAL WEIGHT -> weighting factor to enhance one side of the fit
C REAL POWER -> power to be used to compute distances
C LOGICAL LUP -> .TRUE.: fit upper side
C                .FALSE.: fit lower side
C REAL A(NTERMS) -> fitted coefficients
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE PSEUDOFIT(XF,YF,EYF,NF,NTERMS,YRMSTOL,NEVALMAX,
     +   WEIGHT,POWER,LUP,TSIGMA,A)
        IMPLICIT NONE
C
        INCLUDE 'ndatamax.inc'
C
        INTEGER NF
        REAL XF(NF),YF(NF),EYF(NF)
        INTEGER NTERMS
        REAL YRMSTOL
        INTEGER NEVALMAX
        REAL WEIGHT
        REAL POWER
        LOGICAL LUP
        REAL TSIGMA
        REAL A(NTERMS)
C
        INTEGER NDEGMAX
        PARAMETER (NDEGMAX=16)
C
        EXTERNAL YFUNK_PSEUDO
        REAL YFUNK_PSEUDO
C
        INTEGER NNF,NNTERMS
        INTEGER J,K
        INTEGER NEVAL
        REAL WWEIGHT
        REAL PPOWER
        REAL XXF(NDATAMAX),YYF(NDATAMAX),EYYF(NDATAMAX)
        REAL X0(NDEGMAX+1),DX0(NDEGMAX+1),X(NDEGMAX+1),DX(NDEGMAX+1)
        REAL CHISQR
        REAL TTSIGMA
        LOGICAL LLUP
C
        COMMON/BLKFUNKPSEUDO0/NNF,NNTERMS
        COMMON/BLKFUNKPSEUDO1/XXF,YYF,EYYF
        COMMON/BLKFUNKPSEUDO2/WWEIGHT,PPOWER
        COMMON/BLKFUNKPSEUDO3/LLUP
        COMMON/BLKFUNKPSEUDO4/TTSIGMA
C------------------------------------------------------------------------------
C protecciones
        IF(NF.GT.NDATAMAX)THEN
          WRITE(*,100) 'NF, NDATAMAX: '
          WRITE(*,*) NF,NDATAMAX
          STOP 'FATAL ERROR: NF.GT.NDATAMAX in PSEUDOFIT.'
        END IF
        IF(NTERMS.GT.NDEGMAX+1)THEN
          WRITE(*,100) 'NTERMS...: '
          WRITE(*,*) NTERMS
          WRITE(*,100) 'NDEGMAX..: '
          WRITE(*,*) NDEGMAX
          STOP 'FATAL ERROR: NTERMS.GT.(NDEGMAX+1) in PSEUDOFIT.'
        END IF
C inicializacion (duplicamos los argumentos de entrada de la subrutina para 
C poder pasar la información mediante COMMON blocks a la funcion a minimizar)
        NNF=NF
        NNTERMS=NTERMS
        WWEIGHT=WEIGHT
        PPOWER=POWER
        TTSIGMA=TSIGMA
        LLUP=LUP
        DO J=1,NF
          XXF(J)=XF(J)
          YYF(J)=YF(J)
          EYYF(J)=EYF(J)
        END DO
C------------------------------------------------------------------------------
C Primero hacemos un ajuste tradicional para obtener una primera estimacion 
C (aunque pasamos array de errores en Y, el ajuste lo hacemos sin pesar)
        CALL POLFIT(XF,YF,EYF,NF,NTERMS,0,A,CHISQR)
C------------------------------------------------------------------------------
C Usamos DOWNHILL para calcular el ajuste final
        DO K=1,NTERMS
          X0(K)=A(K)
          IF(A(K).NE.0.0)THEN
            DX0(K)=0.01*A(K)
          ELSE
            DX0(K)=1.0
          END IF
        END DO
        CALL DOWNHILL(NTERMS,X0,DX0,YFUNK_PSEUDO,1.0,0.5,2.0,YRMSTOL,
     +   X,DX,NEVAL,NEVALMAX)
        DO K=1,NTERMS
          A(K)=X(K)
        END DO
C------------------------------------------------------------------------------
        WRITE(*,101) '*************************************************'
        WRITE(*,101) '* Fit results:'
        WRITE(*,100) 'NEVAL: '
        WRITE(*,*) NEVAL
        DO K=1,NTERMS
          WRITE(*,'(A6,I2.2,A2,$)') '>>> a(',K,')='
          WRITE(*,*) X(K),DX(K)
        END DO
        WRITE(*,101) '-------------------------------------------------'
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        REAL FUNCTION YFUNK_PSEUDO(X)
        IMPLICIT NONE
        INCLUDE 'ndatamax.inc'
C
        INTEGER NDEGMAX
        PARAMETER (NDEGMAX=16)
C
        REAL X(NDEGMAX+1)
C
        REAL FPOLY
C
        INTEGER J
        INTEGER NF,NTERMS
        INTEGER NDEG
        REAL XF(NDATAMAX),YF(NDATAMAX),EYF(NDATAMAX)
        REAL WEIGHT,POWER
        REAL W1,W2
        REAL YPOL
        DOUBLE PRECISION DSUM
        LOGICAL LUP
        REAL TSIGMA
C
        COMMON/BLKFUNKPSEUDO0/NF,NTERMS
        COMMON/BLKFUNKPSEUDO1/XF,YF,EYF
        COMMON/BLKFUNKPSEUDO2/WEIGHT,POWER
        COMMON/BLKFUNKPSEUDO3/LUP
        COMMON/BLKFUNKPSEUDO4/TSIGMA
C------------------------------------------------------------------------------
C comprobacion inicial
        IF(TSIGMA.LT.0.0)THEN
          WRITE(*,101) 'FATAL ERROR: invalid TSIGMA (must be >= 0.0)'
          WRITE(*,100) 'TSGIMA='
          WRITE(*,*) TSIGMA
          STOP
        END IF
C------------------------------------------------------------------------------
        DSUM=0.D0
        NDEG=NTERMS-1
C------------------------------------------------------------------------------
        IF(TSIGMA.EQ.0.0)THEN !.....................................sin errores
          IF(LUP)THEN
            W1=1.0
            W2=WEIGHT
          ELSE
            W1=WEIGHT
            W2=1.0
          END IF
          DO J=1,NF
            YPOL=FPOLY(NDEG,X,XF(J))
            IF(YPOL.GE.YF(J))THEN
              DSUM=DSUM+W1*((YPOL-YF(J))**POWER)
            ELSE
              DSUM=DSUM+W2*((YF(J)-YPOL)**POWER)
            END IF
          END DO
        ELSE !......................................................con errores
          IF(LUP)THEN
            !aqui tenemos que usar ABS() porque podemos tener argumentos
            !negativos debido a que el IF() lo estamos calculando
            !considerando las barras de error
            W1=1.0
            W2=WEIGHT
            DO J=1,NF
              YPOL=FPOLY(NDEG,X,XF(J))
              IF(YPOL.GE.YF(J)-TSIGMA*EYF(J))THEN !.......aqui usamos signo "-"
                DSUM=DSUM+W1*(ABS(YPOL-YF(J))**POWER)
              ELSE
                DSUM=DSUM+W2*(ABS(YF(J)-YPOL)**POWER)
              END IF
            END DO
          ELSE
            W1=WEIGHT
            W2=1.0
            DO J=1,NF
              YPOL=FPOLY(NDEG,X,XF(J))
              IF(YPOL.GE.YF(J)+TSIGMA*EYF(J))THEN !.......aqui usamos signo "+"
                DSUM=DSUM+W1*(ABS(YPOL-YF(J))**POWER)
              ELSE
                DSUM=DSUM+W2*(ABS(YF(J)-YPOL)**POWER)
              END IF
            END DO
          END IF
        END IF
C
        YFUNK_PSEUDO=REAL(DSUM)
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
