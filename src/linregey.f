C------------------------------------------------------------------------------
C Copyright 2008 Nicolas Cardiel
C
C This file is part of xpgp.
C 
C Xpgp is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C 
C Xpgp is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C 
C You should have received a copy of the GNU General Public License
C along with xpgp. If not, see <http://www.gnu.org/licenses/>.
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
