C******************************************************************************
C Version 20-September-1999 
C------------------------------------------------------------------------------
C Copyright: N. Cardiel, Departamento de Astrofisica
C            Facultad de Ciencias Fisicas
C            Universidad Complutense de Madrid, 28040-Madrid, Spain
C            E-mail: ncl@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This program is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
C
C     SUBROUTINE LINREGEXY(N,X,Y,EX,EY,A,B,VARA,VARB)
C
C
C Linear regresion fit weighting with the errors EX, EY. The routine initially
C performs a typical unweighted OLS(Y|X). The coefficients from this first fit
C are then employed to derive (numerically ---DOWNHILL---) the straight-line
C data fit with errors in both coordinates.
C 
C
C******************************************************************************
C Input:    N        : number of data pairs
C           X(N),Y(N): data pairs
C           EY(N)    : errors
C Output:   A        : intercept coefficient
C           B        : slope
C           VARA     : variance of the intercept coefficient
C           VARB     : variance of the slope
C******************************************************************************
C
        SUBROUTINE LINREGEXY(N,X,Y,EX,EY,A,B,VARA,VARB)
        IMPLICIT NONE
C routine parameters
        INTEGER N
        DOUBLE PRECISION X(N),Y(N),EX(N),EY(N)
        DOUBLE PRECISION A,B
        DOUBLE PRECISION VARA,VARB
C parametros
        INCLUDE 'ndatamax.inc'
C funciones externas
        DOUBLE PRECISION YFUNKD_LINREGEXY
        EXTERNAL YFUNKD_LINREGEXY
C local variables
        INTEGER I
        INTEGER NEVAL,NN
        DOUBLE PRECISION A_(6),B_(6),VARA_(6),VARB_(6)
        DOUBLE PRECISION X0(2),DX0(2)
        DOUBLE PRECISION ALPHA,BETA,GAMMA
        DOUBLE PRECISION YRMSTOL
        DOUBLE PRECISION XF(2),DXF(2)
        DOUBLE PRECISION XX(NDATAMAX),YY(NDATAMAX)
        DOUBLE PRECISION EXX(NDATAMAX),EYY(NDATAMAX)
C
        COMMON/BLKYFUNKDLINREGEXY1/NN
        COMMON/BLKYFUNKDLINREGEXY2/XX,YY,EXX,EYY
C------------------------------------------------------------------------------
C proteccion
        IF(N.GT.NDATAMAX)THEN
          WRITE(*,100) 'ERROR: number of points to be fitted with '
          WRITE(*,101) 'LINREGEXY too large.'
          WRITE(*,101) 'You must redim NMAX in this subroutine.'
          STOP
        END IF
C------------------------------------------------------------------------------
C duplicamos las variables para poder pasarlas a la funcion a minimizar
C mediante COMMONs
        NN=N
        DO I=1,NN
          XX(I)=X(I)
          YY(I)=Y(I)
          EXX(I)=EX(I)
          EYY(I)=EY(I)
        END DO
C------------------------------------------------------------------------------
C realizamos un ajuste normal OLS(Y|X)
        CALL LINREG(N,X,Y,A_,B_,VARA_,VARB_)
C------------------------------------------------------------------------------
C establecemos valores para la llamada a DOWNHILL
        X0(1)=A_(1)
        X0(2)=B_(1)
        DX0(1)=SQRT(VARA_(1))/2.
        DX0(2)=SQRT(VARB_(1))/2.
        ALPHA=1.0D0
        BETA =0.5D0
        GAMMA=2.0D0
        YRMSTOL=0.D0
        DO I=1,N
          YRMSTOL=YRMSTOL+
     >     (Y(I)-A_(1)-B_(1)*X(I))*(Y(I)-A_(1)-B_(1)*X(I))/
     >     (EY(I)*EY(I)+B_(1)*B_(1)*EX(I)*EX(I))
        END DO
        YRMSTOL=YRMSTOL/10000.D0
C------------------------------------------------------------------------------
C llamamos a downwhill
        CALL DOWNHILLD(2,X0,DX0,YFUNKD_LINREGEXY,ALPHA,BETA,GAMMA,
     >   YRMSTOL,XF,DXF,NEVAL)
        A=XF(1)
        B=XF(2)
        VARA=DXF(1)*DXF(1)
        VARB=DXF(2)*DXF(2)
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
