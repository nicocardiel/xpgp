        SUBROUTINE SPEARMAN(N,X,Y,R)
        IMPLICIT NONE
        INTEGER N
        REAL X(N),Y(N)
        REAL R
C
        INCLUDE 'ndatamax.inc'
C
        INTEGER I
        INTEGER K1,K2,K
        INTEGER ISORTX(NDATAMAX),ISORTY(NDATAMAX)
        REAL XSORTED(NDATAMAX),YSORTED(NDATAMAX)
        REAL RANKX(NDATAMAX),RANKY(NDATAMAX)
        REAL SUM
C------------------------------------------------------------------------------
        IF(N.GT.NDATAMAX)THEN
          WRITE(*,100) 'N,NDATAMAX: '
          WRITE(*,*) N,NDATAMAX
          WRITE(*,100) '>>> FATAL ERROR in subroutine SPEARMAN: '
          WRITE(*,101) 'N.GT.NDATAMAX'
          STOP
        END IF
C------------------------------------------------------------------------------
C establecemos el rank de la matriz X
        DO I=1,N
          ISORTX(I)=I
          XSORTED(I)=X(I)
        END DO
        CALL ORDENA1F1I(N,XSORTED,ISORTX)
        DO I=1,N
          RANKX(ISORTX(I))=REAL(N-I+1)
        END DO
C establecemos el rank de la matriz Y
        DO I=1,N
          ISORTY(I)=I
          YSORTED(I)=Y(I)
        END DO
        CALL ORDENA1F1I(N,YSORTED,ISORTY)
        DO I=1,N
          RANKY(ISORTY(I))=REAL(N-I+1)
        END DO
C matriz X:
C comprobamos si existe alguna repeticion y, en ese caso, cambiamos el rank
        K1=1
        K2=1
10      CONTINUE
        IF(K2.LT.N)THEN
          IF(X(K2).EQ.X(K2+1))THEN
            K2=K2+1
          ELSE
            IF(K2.GT.K1)THEN
              SUM=0.
              DO K=K1,K2
                SUM=SUM+RANKX(K)
              END DO
              SUM=SUM/REAL(K2-K1+1)
              DO K=K1,K2
                RANKX(K)=SUM
              END DO
            END IF
            K2=K2+1
            IF(K2.GE.N) GOTO 12
            K1=K2
          END IF
          GOTO 10
        ELSE
          SUM=0.
          DO K=K1,K2
            SUM=SUM+RANKX(K)
          END DO
          SUM=SUM/REAL(K2-K1+1)
          DO K=K1,K2
            RANKX(K)=SUM
          END DO
        END IF
12      CONTINUE
C matriz Y:
C comprobamos si existe alguna repeticion y, en ese caso, cambiamos el rank
        K1=1
        K2=1
20      CONTINUE
        IF(K2.LT.N)THEN
          IF(Y(K2).EQ.Y(K2+1))THEN
            K2=K2+1
          ELSE
            IF(K2.GT.K1)THEN
              SUM=0.
              DO K=K1,K2
                SUM=SUM+RANKY(K)
              END DO
              SUM=SUM/REAL(K2-K1+1)
              DO K=K1,K2
                RANKY(K)=SUM
              END DO
            END IF
            K2=K2+1
            IF(K2.GE.N) GOTO 22
            K1=K2
          END IF
          GOTO 20
        ELSE
          SUM=0.
          DO K=K1,K2
            SUM=SUM+RANKY(K)
          END DO
          SUM=SUM/REAL(K2-K1+1)
          DO K=K1,K2
            RANKY(K)=SUM
          END DO
        END IF
22      CONTINUE
C mostramos resultado de la ordenacion
ccc     WRITE(*,101) 'I, X, X rank, Y, Y rank: '
ccc     DO I=1,N
ccc       WRITE(*,*) I,X(I),RANKX(I),Y(I),RANKY(I)
ccc     END DO
C calculamos el coeficiente
        SUM=0.
        DO I=1,N
          SUM=SUM+(RANKX(I)-RANKY(I))*(RANKX(I)-RANKY(I))
        END DO
ccc     WRITE(*,100) 'SUM: '
ccc     WRITE(*,*) SUM
        R=1.0-6.0*SUM/(REAL(N)*(REAL(N*N)-1.0))
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
