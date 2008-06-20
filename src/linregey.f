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
C     SUBROUTINE LINREGEY(N,X,Y,EY,A,B,VARA,VARB)
C
C
C Linear regresion fit weighting with the errors EY. The formulae are identical
C to those shown in "Data reduction and error analysis for Physical Sciences",
C Philip R. Bevington, chapter 6.
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
	SUBROUTINE LINREGEY(N,X,Y,EY,A,B,VARA,VARB)
	IMPLICIT NONE
C routine parameters
	INTEGER N
	DOUBLE PRECISION X(N),Y(N),EY(N)
	DOUBLE PRECISION A,B
	DOUBLE PRECISION VARA,VARB
C local variables
	INTEGER I
	DOUBLE PRECISION DELTA,SERR
	DOUBLE PRECISION SX,SY,SXX,SXY
C------------------------------------------------------------------------------
	SERR=0.D0
	DO I=1,N
	  SERR=SERR+1.D0/(EY(I)*EY(I))
	END DO
C
	SX=0.D0
	DO I=1,N
	  SX=SX+X(I)/(EY(I)*EY(I))
	END DO
C
	SY=0.D0
	DO I=1,N
	  SY=SY+Y(I)/(EY(I)*EY(I))
	END DO
C
	SXX=0.D0
	DO I=1,N
	  SXX=SXX+X(I)*X(I)/(EY(I)*EY(I))
	END DO
C
	SXY=0.D0
	DO I=1,N
	  SXY=SXY+X(I)*Y(I)/(EY(I)*EY(I))
	END DO
C
	DELTA=SERR*SXX-SX*SX
C------------------------------------------------------------------------------
	B=(SERR*SXY-SX*SY)/DELTA
	VARB=SERR/DELTA
	A=SY/SERR-B*SX/SERR
	VARA=SXX/DELTA
C------------------------------------------------------------------------------
	END
