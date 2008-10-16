C Funcion para minimizar la coordenada X de un solo Knot
        REAL FUNCTION YFUNK_SPLFIT1(X)
        IMPLICIT NONE
        INCLUDE 'ndatamax.inc'
        INTEGER NKNOTSMAX
        PARAMETER (NKNOTSMAX=20)
        REAL X(NKNOTSMAX)
C
        INTEGER I
        INTEGER I0
        INTEGER NF,ND
        INTEGER NREF
        REAL XF(NDATAMAX),YF(NDATAMAX),YF0
        REAL XDD(NKNOTSMAX)
        REAL S(NKNOTSMAX),A(NKNOTSMAX),B(NKNOTSMAX),C(NKNOTSMAX)
        REAL YD(NKNOTSMAX)
        DOUBLE PRECISION F
C
        COMMON/BLKSPLFUNK1/NF
        COMMON/BLKSPLFUNK2/XF,YF
        COMMON/BLKSPLFUNK3/ND
        COMMON/BLKSPLFUNK4/XDD
        COMMON/BLKSPLFUNK5/YD
        COMMON/BLKSPLFUNK6/NREF
C------------------------------------------------------------------------------
        IF(X(1).LE.XDD(NREF-1)) GOTO 900
        IF(X(1).GE.XDD(NREF+1)) GOTO 900
        XDD(NREF)=X(1)
        CALL CUBSPL(XDD,YD,ND,1,S,A,B,C) !IMODE=1
        F=0.D0
        I0=1                     !la primera vez busca en el inicio de la tabla
        DO I=1,NF
          CALL CUBSPLX(XDD,YD,A,B,C,ND,I0,XF(I),YF0)
          F=F+(DBLE(YF0)-DBLE(YF(I)))*(DBLE(YF0)-DBLE(YF(I)))
        END DO
        F=F/DBLE(NF)
        YFUNK_SPLFIT1=REAL(F)
        RETURN
C
900     YFUNK_SPLFIT1=1.E20
        END
