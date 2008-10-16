C Funcion para minimizar las coordenadas Y de todos los Knots
        REAL FUNCTION YFUNK_SPLFIT(X)
        IMPLICIT NONE
        INCLUDE 'ndatamax.inc'
        INTEGER NKNOTSMAX
        PARAMETER (NKNOTSMAX=20)
        REAL X(NKNOTSMAX)
C
        INTEGER I
        INTEGER NF,ND
        INTEGER I0
        REAL XF(NDATAMAX),YF(NDATAMAX),YF0
        REAL XDD(NKNOTSMAX)
        REAL S(NKNOTSMAX),A(NKNOTSMAX),B(NKNOTSMAX),C(NKNOTSMAX)
        DOUBLE PRECISION F
C
        COMMON/BLKSPLFUNK1/NF
        COMMON/BLKSPLFUNK2/XF,YF
        COMMON/BLKSPLFUNK3/ND
        COMMON/BLKSPLFUNK4/XDD
c------------------------------------------------------------------------------
        CALL CUBSPL(XDD,X,ND,1,S,A,B,C)                                !IMODE=1
        F=0.D0
        I0=1                     !la primera vez busca en el inicio de la tabla
        DO I=1,NF
          CALL CUBSPLX(XDD,X,A,B,C,ND,I0,XF(I),YF0)
          F=F+(DBLE(YF0)-DBLE(YF(I)))*(DBLE(YF0)-DBLE(YF(I)))
        END DO
        F=F/DBLE(NF)
        YFUNK_SPLFIT=REAL(F)
C
        END
