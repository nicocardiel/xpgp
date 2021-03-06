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
C******************************************************************************
C Calcula el nivel de significacion de la distribucion F de Fisher con N1 y N2
C grados de libertad (area de la cola derecha calculada a partir de la
C abcisa X).
        REAL FUNCTION FFFISHER(N1,N2,X)
        IMPLICIT NONE
        INTEGER N1,N2
        REAL X
        REAL BETAI
C
        FFFISHER=BETAI(REAL(N2)/2.,REAL(N1)/2.,
     +   REAL(N2)/(REAL(N2)+REAL(N1)*X))
        END
C******************************************************************************
C Calcula el nivel de significacion de la distribucion t de Student con N
C grados de libertad (area de la cola derecha calculada a partir de la
C abcisa X).
        REAL FUNCTION FTSTUDENT(N,X)
        IMPLICIT NONE
        INTEGER N
        REAL X
        REAL BETAI
C
        FTSTUDENT=0.5*BETAI(REAL(N)/2.,0.5,REAL(N)/(REAL(N)+X*X))
        END
C******************************************************************************
C Calcula la abcisa a cuya derecha la distribucion t de Student con N grados de
C libertad cubre un area (nivel de significacion) alpha
        REAL FUNCTION FTSTUDENTI(N,ALPHA)
        IMPLICIT NONE
        INTEGER N
        REAL ALPHA
        REAL INVIERTE_FTSTUDENT
C
        FTSTUDENTI=INVIERTE_FTSTUDENT(N,ALPHA)
        END
C******************************************************************************
C Calcula el area de la cola derecha de la curva normal tipificada a partir de
C la abcisa X.
        REAL FUNCTION FNORTIP(X)
        IMPLICIT NONE
        REAL X
        REAL GAMMQ
C
        FNORTIP=0.5*GAMMQ(0.5,X*X/2.)
        END
C******************************************************************************
C Calcula el nivel de significacion de la distribucion chi-cuadrado con N
C grados de libertad (area de la cola derecha calculada a partir de la 
C abcisa X).
        REAL FUNCTION FCHISQR(N,X)
        IMPLICIT NONE
        INTEGER N
        REAL X
        REAL GAMMQ
C
        FCHISQR=GAMMQ(REAL(N)/2.,X/2.)
        END
C
C******************************************************************************
C******************************************************************************
      FUNCTION GAMMP(A,X)
      IF(X.LT.0..OR.A.LE.0.) STOP 'PAUSE'
      IF(X.LT.A+1.)THEN
        CALL GSER(GAMSER,A,X,GLN)
        GAMMP=GAMSER
      ELSE
        CALL GCF(GAMMCF,A,X,GLN)
        GAMMP=1.-GAMMCF
      ENDIF
      RETURN
      END
C******************************************************************************
      FUNCTION GAMMQ(A,X)
      IF(X.LT.0..OR.A.LE.0.) STOP 'PAUSE'
      IF(X.LT.A+1.)THEN
        CALL GSER(GAMSER,A,X,GLN)
        GAMMQ=1.-GAMSER
      ELSE
        CALL GCF(GAMMCF,A,X,GLN)
        GAMMQ=GAMMCF
      ENDIF
      RETURN
      END
C******************************************************************************
      SUBROUTINE GSER(GAMSER,A,X,GLN)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      GLN=GAMMLN(A)
      IF(X.LE.0.)THEN
        IF(X.LT.0.) STOP 'PAUSE'
        GAMSER=0.
        RETURN
      ENDIF
      AP=A
      SUM=1./A
      DEL=SUM
      DO 11 N=1,ITMAX
        AP=AP+1.
        DEL=DEL*X/AP
        SUM=SUM+DEL
        IF(ABS(DEL).LT.ABS(SUM)*EPS)GO TO 1
11    CONTINUE
      STOP 'PAUSE: A too large, ITMAX too small'
1     GAMSER=SUM*EXP(-X+A*LOG(X)-GLN)
      RETURN
      END
C******************************************************************************
      SUBROUTINE GCF(GAMMCF,A,X,GLN)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      GLN=GAMMLN(A)
      GOLD=0.
      A0=1.
      A1=X
      B0=0.
      B1=1.
      FAC=1.
      DO 11 N=1,ITMAX
        AN=FLOAT(N)
        ANA=AN-A
        A0=(A1+A0*ANA)*FAC
        B0=(B1+B0*ANA)*FAC
        ANF=AN*FAC
        A1=X*A0+ANF*A1
        B1=X*B0+ANF*B1
        IF(A1.NE.0.)THEN
          FAC=1./A1
          G=B1*FAC
          IF(ABS((G-GOLD)/G).LT.EPS)GO TO 1
          GOLD=G
        ENDIF
11    CONTINUE
      STOP 'PAUSE: A too large, ITMAX too small'
1     GAMMCF=EXP(-X+A*ALOG(X)-GLN)*G
      RETURN
      END
C******************************************************************************
      real FUNCTION GAMMLN(XX)
      REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      X=XX-ONE
      TMP=X+FPF
      TMP=(X+HALF)*LOG(TMP)-TMP
      SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER)
      RETURN
      END
C******************************************************************************
      real FUNCTION BETAI(A,B,X)
      real a,b,x,gammln,betacf
      IF(X.LT.0..OR.X.GT.1.) STOP 'PAUSE: bad argument X in BETAI'
      IF(X.EQ.0..OR.X.EQ.1.)THEN
        BT=0.
      ELSE
        BT=EXP(GAMMLN(A+B)-GAMMLN(A)-GAMMLN(B)
     *      +A*ALOG(X)+B*ALOG(1.-X))
      ENDIF
      IF(X.LT.(A+1.)/(A+B+2.))THEN
        BETAI=BT*BETACF(A,B,X)/A
        RETURN
      ELSE
        BETAI=1.-BT*BETACF(B,A,1.-X)/B
        RETURN
      ENDIF
      END
C******************************************************************************
      real FUNCTION BETACF(A,B,X)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      AM=1.
      BM=1.
      AZ=1.
      QAB=A+B
      QAP=A+1.
      QAM=A-1.
      BZ=1.-QAB*X/QAP
      DO 11 M=1,ITMAX
        EM=M
        TEM=EM+EM
        D=EM*(B-M)*X/((QAM+TEM)*(A+TEM))
        AP=AZ+D*AM
        BP=BZ+D*BM
        D=-(A+EM)*(QAB+EM)*X/((A+TEM)*(QAP+TEM))
        APP=AP+D*AZ
        BPP=BP+D*BZ
        AOLD=AZ
        AM=AP/BPP
        BM=BP/BPP
        AZ=APP/BPP
        BZ=1.
        IF(ABS(AZ-AOLD).LT.EPS*ABS(AZ))GO TO 1
11    CONTINUE
      STOP 'PAUSE: A or B too big, or ITMAX too small'
1     BETACF=AZ
      RETURN
      END
C******************************************************************************
      real FUNCTION ERF(X)
      IF(X.LT.0.)THEN
        ERF=-GAMMP(.5,X**2)
      ELSE
        ERF=GAMMP(.5,X**2)
      ENDIF
      RETURN
      END
C******************************************************************************
C******************************************************************************
        REAL FUNCTION INVIERTE_FTSTUDENT(N,ALPHA)
        IMPLICIT NONE
        INTEGER N
        REAL ALPHA
C
        REAL FUNK_IFTSTUDENT
        EXTERNAL FUNK_IFTSTUDENT
C
        INTEGER NDEG_OF_FREEDOM
        INTEGER NEVAL
        REAL X0,DX0
        REAL XF,DXF
        REAL SIGNIFICACION
C
        COMMON/BLKIFTSTUDENT1/NDEG_OF_FREEDOM
        COMMON/BLKIFTSTUDENT2/SIGNIFICACION
C------------------------------------------------------------------------------
C Segun el valor de alfa, hacemos una estimacion del punto inicial para
C DOWNHILL, usando el valor de la funcion t de student para infinitos grados 
C de libertad (distribucion normal).
        IF(ALPHA.GT.0.50)THEN
          X0=0.0
        ELSEIF(ALPHA.GT.0.40)THEN
          X0=0.253
        ELSEIF(ALPHA.GT.0.30)THEN
          X0=0.524
        ELSEIF(ALPHA.GT.0.20)THEN
          X0=0.842
        ELSEIF(ALPHA.GT.0.10)THEN
          X0=1.282
        ELSEIF(ALPHA.GT.0.050)THEN
          X0=1.645
        ELSEIF(ALPHA.GT.0.025)THEN
          X0=1.960
        ELSEIF(ALPHA.GT.0.010)THEN
          X0=2.326
        ELSEIF(ALPHA.GT.0.005)THEN
          X0=2.576
        ELSEIF(ALPHA.GT.0.001)THEN
          X0=3.090
        ELSE
          X0=3.291
        END IF
        DX0=.1
C------------------------------------------------------------------------------
        NDEG_OF_FREEDOM=N
        SIGNIFICACION=ALPHA
        CALL DOWNHILL(1,X0,DX0,FUNK_IFTSTUDENT,1.0,0.5,2.0,1.E-7,
     >   XF,DXF,NEVAL,5000)
        INVIERTE_FTSTUDENT=XF
        RETURN
        END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        REAL FUNCTION FUNK_IFTSTUDENT(X)
        IMPLICIT NONE
        REAL X
C
        REAL FTSTUDENT
C
        INTEGER N
        REAL ALPHA
        COMMON/BLKIFTSTUDENT1/N
        COMMON/BLKIFTSTUDENT2/ALPHA
C------------------------------------------------------------------------------	
        FUNK_IFTSTUDENT=ABS(ALPHA-FTSTUDENT(N,X))
        END
