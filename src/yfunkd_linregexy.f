C
C******************************************************************************
C Funcion a minimizar para ajustar una recta por minimos cuadrados,
C considerando la existencia de errores en las dos variables.
        DOUBLE PRECISION FUNCTION YFUNKD_LINREGEXY(A)
        IMPLICIT NONE
        DOUBLE PRECISION A(2)
C parametros
        INCLUDE 'ndatamax.inc'
C variables globales a la funcion y transmitidas mediante COMMONS
        INTEGER N
        DOUBLE PRECISION X(NDATAMAX),Y(NDATAMAX)
        DOUBLE PRECISION EX(NDATAMAX),EY(NDATAMAX)
C variables locales
        INTEGER I
        DOUBLE PRECISION SUM
C COMMONs
        COMMON/BLKYFUNKDLINREGEXY1/N
        COMMON/BLKYFUNKDLINREGEXY2/X,Y,EX,EY
C------------------------------------------------------------------------------
        SUM=0.D0
        DO I=1,N
          SUM=SUM+(Y(I)-A(1)-A(2)*X(I))*(Y(I)-A(1)-A(2)*X(I))/
     >     (EY(I)*EY(I)+A(2)*A(2)*EX(I)*EX(I))
        END DO
        YFUNKD_LINREGEXY=SUM
C
        END
