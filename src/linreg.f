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
C                  SUBROUTINE LINREG(N,X,Y,A_,B_,VARA_,VARB_)
C
C
C Linear regresion fits using the six methods described in Isobe et al., 1990,
C ApJ, 364, 104-113, and in Feigelson and Babu 1992, 1992, ApJ, 397, 55-67
C
C These methods concentrates on problems where the intrinsic scatter of the
C data dominates any errors arising from the measurement process.
C
C Five different fits are obtained:
C
C (1) OLS(X|Y): ordinary least-squares regression of X on Y
C (2) OLS(Y|X): ordinary least-squares regression of Y on X
C (3) OLS bisector (bisector of the OLS(Y|X) and OLS(X|Y)
C (4) Orthogonal regression
C (5) Reduced major-axis
C (6) Mean OLS
C
C******************************************************************************
C Input:    N        : number of data pairs
C           X(N),Y(N): data pairs
C Output:   A_(6)    : intercept coefficients for the six methods
C           B_(6)    : slopes for the six methods
C           VARA_(6) : variances of the intercept coefficients
C           VARB_(6) : variances of the slopes
C******************************************************************************
C
        SUBROUTINE LINREG(N,X,Y,A_,B_,VARA_,VARB_)
        IMPLICIT NONE
C routine parameters
        INTEGER N
        DOUBLE PRECISION X(N),Y(N)
        DOUBLE PRECISION A_(6),B_(6)
        DOUBLE PRECISION VARA_(6),VARB_(6)
C additional functions
        DOUBLE PRECISION DDSIGN
C local variables
        INTEGER I,K
        DOUBLE PRECISION XMED,YMED
        DOUBLE PRECISION SXX,SYY,SXY
        DOUBLE PRECISION B1,B2,B3,B4,B5,B6
        DOUBLE PRECISION VARB1,VARB2,VARB3,VARB4,VARB5,VARB6
        DOUBLE PRECISION COVB1B2
        DOUBLE PRECISION G(2,5)
        DOUBLE PRECISION G1,G2
        DOUBLE PRECISION DSUM,X0,Y0
C------------------------------------------------------------------------------
C Equations 2a, 2b, 3a, 3b and 4 (page 105)
        XMED=0.D0
        DO I=1,N
          XMED=XMED+X(I)
        END DO
        XMED=XMED/DBLE(N)
C
        YMED=0.D0
        DO I=1,N
          YMED=YMED+Y(I)
        END DO
        YMED=YMED/DBLE(N)
C
        SXX=0.D0
        DO I=1,N
          SXX=SXX+(X(I)-XMED)*(X(I)-XMED)
        END DO
C
        SYY=0.D0
        DO I=1,N
          SYY=SYY+(Y(I)-YMED)*(Y(I)-YMED)
        END DO
C
        SXY=0.D0
        DO I=1,N
          SXY=SXY+(X(I)-XMED)*(Y(I)-YMED)
        END DO
C------------------------------------------------------------------------------
C Estimate of Slopes and Their Variances (Table 1, page 106)
C------------------------------------------------------------------------------
C OLS(Y|X): ordinary least-squares regression of X on Y
        B1=SXY/SXX
        VARB1=0.D0
        DO I=1,N
          VARB1=VARB1+(X(I)-XMED)*(X(I)-XMED)*
     >     (Y(I)-B1*X(I)-YMED+B1*XMED)*
     >     (Y(I)-B1*X(I)-YMED+B1*XMED)
        END DO
        VARB1=VARB1/(SXX*SXX)
C------------------------------------------------------------------------------
C OLS(X|Y): ordinary least-squares regression of Y on X
        B2=SYY/SXY
        VARB2=0.D0
        DO I=1,N
          VARB2=VARB2+(Y(I)-YMED)*(Y(I)-YMED)*
     >     (Y(I)-B2*X(I)-YMED+B2*XMED)*
     >     (Y(I)-B2*X(I)-YMED+B2*XMED)
        END DO
        VARB1=VARB1/(SXY*SXY)
C------------------------------------------------------------------------------
C An estimate of covariance term is given by
        COVB1B2=0.D0
        DO I=1,N
          COVB1B2=COVB1B2+(X(I)-XMED)*(Y(I)-YMED)*
     >     (Y(I)-YMED-B1*(X(I)-XMED))*(Y(I)-YMED-B2*(X(I)-XMED))
        END DO
        COVB1B2=COVB1B2/(B1*SXX*SXX)
C------------------------------------------------------------------------------
C OLS bisector (bisector of the OLS(Y|X) and OLS(X|Y)
        B3=(B1*B2-1.D0+DSQRT((1.D0+B1*B1)*(1.D0+B2*B2)))/(B1+B2)
        VARB3=B3*B3/((B1+B2)*(B1+B2)*(1.D0+B1*B1)*(1.D0+B2*B2))*
     >   ((1.D0+B2*B2)*(1.D0+B2*B2)*VARB1+
     >    2.D0*(1.D0+B1*B1)*(1.D0+B2*B2)*COVB1B2+
     >    (1.D0+B1*B1)*(1.D0+B1*B1)*VARB2)
C------------------------------------------------------------------------------
C Orthogonal regression
        B4=0.5D0*
     >   (B2-1.D0/B1+
     >    DDSIGN(SXY)*DSQRT(4.D0+(B2-1.D0/B1)*(B2-1.D0/B1)))
        VARB4=B4*B4/(4.D0*B1*B1+(B1*B2-1.D0)*(B1*B2-1.D0))*
     >   (VARB1/(B1*B1)+2.D0*COVB1B2+B1*B1*VARB2)
C------------------------------------------------------------------------------
C Reduced major-axis
        B5=DDSIGN(SXY)*DSQRT(B1*B2)
        VARB5=(B2/B1*VARB1+2.D0*COVB1B2+B1/B2*VARB2)/4.D0
C------------------------------------------------------------------------------
C Mean OLS
        B6=(B1+B2)/2.D0
        VARB6=0.D0                                             !still undefined
C------------------------------------------------------------------------------
        B_(1)=B1
        B_(2)=B2
        B_(3)=B3
        B_(4)=B4
        B_(5)=B5
        B_(6)=B6
        VARB_(1)=VARB1
        VARB_(2)=VARB2
        VARB_(3)=VARB3
        VARB_(4)=VARB4
        VARB_(5)=VARB5
        VARB_(6)=VARB6
C------------------------------------------------------------------------------
C         Estimation of Intercept Coefficients and Their Variances
C------------------------------------------------------------------------------
        G1=B3/((B1+B2)*DSQRT((1.D0+B1*B1)*(1.D0+B2*B2)))
        G2=B4/DSQRT(4.D0*B1*B1+(B1*B2-1.D0)*(B1*B2-1.D0))
        G(1,1)=1.D0
        G(1,2)=0.D0
        G(1,3)=G1*(1.D0+B2*B2)
        G(1,4)=G2/DABS(B1)
        G(1,5)=0.5D0*DSQRT(B2/B1)
        G(2,1)=0.D0
        G(2,2)=1.D0
        G(2,3)=G1*(1.D0+B1*B1)
        G(2,4)=DABS(B1)*G2
        G(2,5)=0.5D0*DSQRT(B1/B2)
C------------------------------------------------------------------------------
        DO K=1,5
          A_(K)=YMED-B_(K)*XMED
          DSUM=0.D0
          DO I=1,N
            X0=X(I)-XMED
            Y0=Y(I)-YMED
            DSUM=DSUM+
     >       (Y0-B_(K)*X0-DBLE(N)*XMED*
     >        (G(1,K)/SXX*X0*(Y0-B1*X0)+G(2,K)/SXY*Y0*(Y0-B2*X0)))*
     >       (Y0-B_(K)*X0-DBLE(N)*XMED*
     >        (G(1,K)/SXX*X0*(Y0-B1*X0)+G(2,K)/SXY*Y0*(Y0-B2*X0)))
          END DO
          VARA_(K)=DSUM/DBLE(N*N)
        END DO
C------------------------------------------------------------------------------
        A_(6)=(A_(1)+A_(2))/2.D0
        VARA_(6)=0.D0                                          !still undefined
        END
C
C******************************************************************************
C Retorna el signo del argumento
        DOUBLE PRECISION FUNCTION DDSIGN(X)
        IMPLICIT NONE
        DOUBLE PRECISION X
C
        IF(X.GT.0.D0)THEN
          DDSIGN=1.D0
        ELSEIF(X.LT.0.D0)THEN
          DDSIGN=-1.D0
        ELSE
          DDSIGN=0.D0
        END IF
C
        END
