C
C******************************************************************************
C Ajusta un polinomio generico por minimos cuadrados (sin errores)
        SUBROUTINE FITPOL(N,X,Y,NDEG,IFCOEF,B,VARB,CHISQR,SR2,COVAR)
        IMPLICIT NONE
C
        INCLUDE 'ndatamax.inc'
        INCLUDE 'ndegmax.inc'
C
        INTEGER N
        DOUBLE PRECISION X(N),Y(N)
        INTEGER NDEG
        LOGICAL IFCOEF(NDEGMAX+1)
        DOUBLE PRECISION B(NDEGMAX+1),VARB(NDEGMAX+1)
        DOUBLE PRECISION CHISQR,SR2
        DOUBLE PRECISION COVAR(NDEGMAX+1,NDEGMAX+1)
C
        INTEGER K
        INTEGER I,J,L
        INTEGER II,JJ
        INTEGER NCOEF
        INTEGER ORDER(NDEGMAX+1)
        INTEGER IOK,IPAR
        DOUBLE PRECISION A(NDEGMAX+1,NDEGMAX+1)
        DOUBLE PRECISION G(NDEGMAX+1)
        DOUBLE PRECISION C1(NDEGMAX+1),C2(NDEGMAX+1)
        DOUBLE PRECISION SCALEROW(NDEGMAX+1)
        DOUBLE PRECISION DSUM
        DOUBLE PRECISION B_(NDEGMAX+1)
        DOUBLE PRECISION X0,Y0
C------------------------------------------------------------------------------
        IF(N.GT.NDATAMAX)THEN
          STOP 'FATAL ERROR: N.GT.NDATAMAX in FITPOL'
        END IF
        IF(NDEG.GT.NDEGMAX)THEN
          STOP 'FATAL ERROR: NDEG.GT.NDEGMAX in FITPOL'
        END IF
C------------------------------------------------------------------------------
C numero de coeficientes a ajustar
        NCOEF=0
        DO I=1,NDEG+1
          IF(IFCOEF(I)) NCOEF=NCOEF+1
        END DO
C------------------------------------------------------------------------------
C creamos matrices para plantear el sistema de ecuaciones a resolver
        II=0
        DO I=1,NDEG+1
          IF(IFCOEF(I))THEN
            II=II+1
            JJ=0
            DO J=1,NDEG+1
              IF(IFCOEF(J))THEN
                JJ=JJ+1
                IF((I.EQ.1).AND.(J.EQ.1))THEN
                  A(1,1)=DBLE(N)
                ELSE
                  A(JJ,II)=0.D0
                  DO K=1,N
                    A(JJ,II)=A(JJ,II)+(X(K)**(I+J-2))
                  END DO
                END IF
              END IF
            END DO
            G(II)=0.D0
            DO K=1,N
              DSUM=Y(K)
              DO L=1,NDEG+1
                IF(.NOT.IFCOEF(L))THEN
                  IF(L.EQ.1)THEN
                    DSUM=DSUM-B(L)
                  ELSE
                    DSUM=DSUM-(B(L)*X(K)**(L-1))
                  END IF
                END IF
              END DO
              IF(I.EQ.1)THEN
                G(II)=G(II)+DSUM
              ELSE
                G(II)=G(II)+DSUM*(X(K)**(I-1))
              END IF
            END DO
          END IF
        END DO
C------------------------------------------------------------------------------
C resolvemos el sistema de ecuaciones
        CALL LUDCMP(A,NCOEF,NDEGMAX+1,ORDER,SCALEROW,IOK,IPAR)
        CALL LUSOLV(A,NCOEF,NDEGMAX+1,ORDER,SCALEROW,G,B_)
C------------------------------------------------------------------------------
C introducimos los coeficientes calculados en la matriz de salida, sin
C modificar los coeficientes fijos
        II=0
        DO I=1,NDEG+1
          IF(IFCOEF(I))THEN
            II=II+1
            B(I)=B_(II)
          END IF
        END DO
C------------------------------------------------------------------------------
C varianza residual y Chi-cuadrado
        SR2=0.D0
        DO K=1,N
          X0=X(K)
          Y0=B(NDEG+1)
          DO L=NDEG,1,-1
            Y0=Y0*X0+B(L)
          END DO
          SR2=SR2+(Y(K)-Y0)*(Y(K)-Y0)
        END DO
        CHISQR=SR2
        SR2=SR2/DBLE(N-(NDEG+1))
C------------------------------------------------------------------------------
C calculamos la matriz de varianza-covarianza (=matriz inversa de A)
        DO I=1,NCOEF
          DO J=1,NCOEF
            IF(I.EQ.J)THEN
              C1(J)=1.D0
            ELSE
              C1(J)=0.D0
            END IF
          END DO
          CALL LUSOLV(A,NCOEF,NDEGMAX+1,ORDER,SCALEROW,C1,C2)
          DO J=1,NCOEF
            COVAR(J,I)=C2(J)
          END DO
        END DO
C------------------------------------------------------------------------------
C introducimos las varianzas calculadas en la matriz de salida
        II=0
        DO I=1,NDEG+1
          IF(IFCOEF(I))THEN
            II=II+1
            VARB(I)=COVAR(II,II)*SR2
          ELSE
            VARB(I)=0.D0
          END IF
        END DO
C------------------------------------------------------------------------------
        END