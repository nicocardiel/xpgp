C funcion que calcula la probabilidad acumulada de una distribucion normal
C******************************************************************************
C
        REAL FUNCTION KS_FUNC(X)
        IMPLICIT NONE
        REAL X
C
        IF(X.LT.0.0)THEN
          KS_FUNC=0.5-0.5*ERF(-X)
        ELSE
          KS_FUNC=0.5+0.5*ERF(X)
        END IF
C
        END
