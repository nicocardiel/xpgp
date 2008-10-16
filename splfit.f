C------------------------------------------------------------------------------
C Version 14-October-2008                                        File: splfit.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel & J. Gorgas, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: ncl@astrax.fis.ucm.es or fjg@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE SPLFIT(N,X,Y,EY,ND,XD,YRMSTOL,NEVALMAX,NSEED,
C                   WEIGHT,POWER,LUP,TSIGMA,
C                   NOUT,XOUT,YOUT,XMIN,XMAX,YKNOT,ASPL,BSPL,CSPL)
C
C Input: N,X,Y,EY,ND,XD,YRMSTOL,NEVALMAX,NSEED
C Input: WEIGHT,POWER,LUP,TSIGMA,NOUT,XMIN,XMAX
C Output: XOUT,YOUT,YKNOT,ASPL,BSPL,CSPL
C
C Least-squares fit to splines, using ND knots located at XD(). The maximum 
C number of knots if defined in the parameter NKNOTSMAX.
C Input data are X(N), Y(N). XOUT(NOUT), YOUT(NOUT) are the output values which
C are computed in the range from XMIN to XMAX. The knot location determines the
C X(),Y() range employed in the fit (which is performed in the interval from
C XD(1) to XD(ND)). The subroutine also fits the boundary of the data
C depending on the values of WEIGHT, POWER, LUP and TSIGMA.
C
C INTEGER N -> initial number of points in input data
C REAL    X(N) -> sorted input data
C REAL    Y(N) -> input data
C REAL    EY(N) -> input data errors
C INTEGER ND -> number of knots
C REAL    XD(ND) -> X location of each knot
C REAL    YRMSTOL ->  stopping criterion for DOWNHILL
C INTEGER NEVALMAX -> maximum number of allowed iterations in DOWNHILL
C INTEGER NSEED -> seed for random numbers
C REAL    WEIGHT -> for boundary fitting
C REAL    POWER -> for boundary fitting
C LOGICAL LUP -> .TRUE. for upper-limit, .FALSE. for lower-limit
C REAL    TSIGMA -> times sigma for boundary fitting
C INTEGER NOUT -> number of points in output
C REAL    XOUT(NOUT) -> output data
C REAL    YOUT(NOUT) -> output data
C REAL    XMIN -> = XOUT(1)
C REAL    XMAX -> = XOUT(NOUT)
C REAL    YKNOT(ND) -> Y location of each knot
C REAL    ASPL(ND) -> fit coefficients
C REAL    BSPL(ND) -> fit coefficients
C REAL    CSPL(ND) -> fit coefficients
C
Comment
C------------------------------------------------------------------------------
C Esta subrutina requiere POLFIT y DOWNHILL
C         del ajuste final.
        SUBROUTINE SPLFIT(N,X,Y,EY,ND,XD,YRMSTOL,NEVALMAX,NSEED,
     +   WEIGHT,POWER,LUP,TSIGMA,
     +   NOUT,XOUT,YOUT,XMIN,XMAX,YKNOT,ASPL,BSPL,CSPL)
        IMPLICIT NONE
        INCLUDE 'ndatamax.inc'
        INTEGER READI_B
        INTEGER READILIM_B
        REAL READF_B
        CHARACTER*255 READC_B
C NKNOTSMAX- numero maximo de nodos
        INTEGER NKNOTSMAX                      !cambiar tambien en funcion FUNK
        PARAMETER (NKNOTSMAX=20)
C
        INTEGER N
        REAL X(N),Y(N),EY(N)
        INTEGER ND
        REAL XD(ND)
        REAL YRMSTOL
        INTEGER NEVALMAX
        INTEGER NSEED
        REAL WEIGHT
        REAL POWER
        LOGICAL LUP
        REAL TSIGMA
        INTEGER NOUT
        REAL XOUT(NOUT),YOUT(NOUT)
        REAL XMIN,XMAX
        REAL YKNOT(NKNOTSMAX)
C
        EXTERNAL YFUNK_SPLFIT,YFUNK_SPLFIT1,YFUNK_SPLFIT2,YFUNK_SPLFIT3
        REAL YFUNK_SPLFIT,YFUNK_SPLFIT1,YFUNK_SPLFIT2,YFUNK_SPLFIT3
C
        INTEGER I,J,K,H,L
        INTEGER NF
        INTEGER NEVAL
        INTEGER NDD,NREF
        INTEGER NITER,NITERT
        INTEGER NNSEED,NRANND(NKNOTSMAX)
        INTEGER I0SPL
        REAL XF(NDATAMAX),YF(NDATAMAX),EYF(NDATAMAX)
        REAL YD(NKNOTSMAX)
        REAL MEANF,RMSF
        REAL ASPL(NKNOTSMAX),BSPL(NKNOTSMAX),CSPL(NKNOTSMAX)
        REAL SSPL(NKNOTSMAX)
        REAL XDD(NKNOTSMAX),XX(NKNOTSMAX),DXX(NKNOTSMAX)
        REAL XX0(NKNOTSMAX),DXX0(NKNOTSMAX)
        REAL WWEIGHT
        REAL PPOWER
        REAL TTSIGMA
        REAL A(4),CHISQR,FDUMMY
        REAL SIGMA
        REAL YMINP,YMAXP,XMINP,XMAXP
        CHARACTER*1 CREF
        CHARACTER*20 CDUMMY
        LOGICAL LLUP
C
        COMMON/BLKSPLNSEED/NNSEED
        COMMON/BLKSPLFUNK1/NF
        COMMON/BLKSPLFUNK2/XF,YF,EYF
        COMMON/BLKSPLFUNK3/NDD
        COMMON/BLKSPLFUNK4/XDD
        COMMON/BLKSPLFUNK5/YD
        COMMON/BLKSPLFUNK6/NREF
        COMMON/BLKSPLFUNK7/WWEIGHT,PPOWER,TTSIGMA
        COMMON/BLKSPLFUNK8/LLUP
C------------------------------------------------------------------------------
C Inicializacion (duplicamos argumentos de entrada de la subrutina para
C poder pasar la informacion mediante COMMON blocks a las funciones)
        NNSEED=NSEED
        WWEIGHT=WEIGHT
        PPOWER=POWER
        TTSIGMA=TSIGMA
        LLUP=LUP
C
        call pgqwin(xminp,xmaxp,yminp,ymaxp)
C------------------------------------------------------------------------------
        NITER=0
        NITERT=0
        IF(ND.GT.NKNOTSMAX)THEN
          WRITE(*,*)
          WRITE(*,101)'ERROR in SPLFIT: '
          WRITE(*,110)'>>> No. of Knots: ',ND
          WRITE(*,110)'>>> Maximum No. of Knots: ',NKNOTSMAX
          WRITE(*,*)
          GOTO 900
        END IF
        DO I=1,ND
          YD(I)=0.
        END DO
        DO I=1,NOUT
          XOUT(I)=XMIN+(XMAX-XMIN)*REAL(I-1)/REAL(NOUT-1)
        END DO
C ajuste inicial a polinomio de grado 3
        J=1
        DO WHILE(X(J).LT.XD(1))
          J=J+1
        END DO
        I=2
        DO WHILE(I.LE.ND)
          K=1
          DO WHILE((X(J).LE.XD(I)).AND.(J.LE.N))
            XF(K)=X(J)
            YF(K)=Y(J)
            EYF(K)=EY(J)
            K=K+1
            IF(K.GT.NDATAMAX)THEN
              WRITE(*,*)
              WRITE(*,101)'ERROR in SPLFIT:'
              WRITE(*,101)'>>> No. of points to fit > NDATAMAX'
              WRITE(*,*)
              GOTO 900
            END IF
            J=J+1
          END DO
          IF(K.LT.4)THEN            !tomamos el valor medio de los puntos si no
            FDUMMY=0.               !hay suficientes para el ajuste polinomico
            DO H=NINT(XD(I-1)),NINT(XD(I))
              FDUMMY=FDUMMY+YF(H)
            END DO
            FDUMMY=FDUMMY/REAL(NINT(XD(I))-NINT(XD(I-1))+1)
            YD(I-1)=YD(I-1)+FDUMMY
            YD(I)=YD(I)+FDUMMY
          ELSE
            NF=K-1
            CALL POLFIT(XF,YF,YF,NF,4,0,A,CHISQR)
            YD(I-1)=YD(I-1)+A(1)+A(2)*XD(I-1)+A(3)*XD(I-1)*XD(I-1)+
     +            A(4)*XD(I-1)*XD(I-1)*XD(I-1)
            YD(I)=YD(I)+A(1)+A(2)*XD(I)+A(3)*XD(I)*XD(I)+
     +            A(4)*XD(I)*XD(I)*XD(I)
          END IF
          I=I+1
          IF(X(J-1).EQ.XD(I-1)) J=J-1
        END DO
        DO I=2,ND-1
          YD(I)=YD(I)/2.
        END DO
C
        J=1
        DO WHILE(X(J).LT.XD(1))
          J=J+1
        END DO
        I=1
        DO WHILE((X(J).LE.XD(ND)).AND.(J.LE.N))
          XF(I)=X(J)
          YF(I)=Y(J)
          EYF(I)=EY(J)
          I=I+1
          IF(I.GT.NDATAMAX)THEN
            WRITE(*,*)
            WRITE(*,101)'ERROR in SPLFIT:'
            WRITE(*,101)'Please, redim NDATAMAX'
            WRITE(*,*)
            GOTO 900
          END IF
          J=J+1
        END DO
        NF=I-1
C
        NDD=ND
        DO I=1,ND
          XDD(I)=XD(I)
        END DO
C calculamos media y rms de YD
        MEANF=0.
        DO I=1,ND
          MEANF=MEANF+YD(I)
        END DO
        MEANF=MEANF/REAL(ND)
        RMSF=0.
        DO I=1,ND
          RMSF=RMSF+(MEANF-YD(I))*(MEANF-YD(I))
        END DO
        RMSF=SQRT(RMSF/REAL(ND-1))
C valores iniciales para DOWNHILL
        DO I=1,ND
          XX0(I)=YD(I)
          IF(RMSF.GT.0.0)THEN
            DXX0(I)=RMSF/3.
          ELSEIF(YD(I).GT.0.0)THEN
            DXX0(I)=0.05*YD(I)
          ELSE
            DXX0(I)=1.0                        !que remedio; no tenemos ni idea
          END IF
        END DO
C llamamos a DOWNHILL
        WRITE(*,100)'Running DOWNHILL...'
        CALL DOWNHILL(ND,XX0,DXX0,YFUNK_SPLFIT,1.0,0.5,2.0,YRMSTOL,
     +   XX,DXX,NEVAL,NEVALMAX)
        WRITE(*,110)'      no. of function evaluations: ',NEVAL
        DO J=1,ND
          YD(J)=XX(J)
        END DO
        SIGMA=SQRT(YFUNK_SPLFIT(YD))
20      CALL CUBSPL(XD,YD,ND,1,SSPL,ASPL,BSPL,CSPL)                    !IMODE=1
        I0SPL=1                  !la primera vez busca en el inicio de la tabla
        DO K=1,NOUT
          CALL CUBSPLX(XD,YD,ASPL,BSPL,CSPL,ND,I0SPL,XOUT(K),YOUT(K))
        END DO
C si estamos iterando seguimos con las iteraciones
        IF((NITERT.NE.0).AND.(NITER.LT.NITERT)) GOTO 24
C
c dibujamos ajuste final
        call pgsci(0)
        call pgline(nout,xout,yout)
        do i=1,nd
          call pgpoint(1,xd(i),yd(i),17)
          write(cdummy,*)i
          call rmblank(cdummy,cdummy,k)
          call pgptext(xd(i),yd(i)+(ymaxp-yminp)/25.,0.,0.5,
     +     cdummy(1:k))
        end do
        write(*,100)'sigma of the fit: '
        write(*,*)sigma
C mostramos los puntos del ajuste
        DO I=1,ND
          WRITE(*,100) '>>> Knot #, xknot, yknot: '
          WRITE(*,*) I,XD(I),YD(I)
          YKNOT(I)=YD(I)
        END DO
C si el numero de Knots es solo 2 (los extremos) no se permite refinar el
C ajuste
        IF(ND.EQ.2) RETURN
C Si se quiere refinamos el ajuste
        WRITE(*,100) 'Refine the fit (y/n) '
        CREF(1:1)=READC_B('n','yn')
        WRITE(77,112) CREF,'# Refine the fit (y/n)?'
        IF(CREF.EQ.'n') RETURN
21      WRITE(*,101)'(1) Refine X and Y position-> 1 Knot'
        WRITE(*,101)'(2) Refine X-position  -----> 1 Knot'
        WRITE(*,101)'(3) Refine Y-position  -----> 1 Knot'
        WRITE(*,101)'(W) Refine X and Y position-> all Knots'
        WRITE(*,101)'(0) Exit'
        WRITE(*,100)'Option '
        CREF(1:1)=READC_B('0','0123Ww')
        WRITE(77,112) CREF,'# Option: 1=X&Y, 2=X, 3=Y, w=all knots'
        IF(CREF.EQ.'w')CREF='W'
        IF(CREF.EQ.'0')THEN
          call pgsci(0)
          call pgslw(3)
          call pgline(nout,xout,yout)
          do i=1,nd
            call pgpoint(1,xd(i),yd(i),17)
            write(cdummy,*)i
            call rmblank(cdummy,cdummy,k)
            call pgptext(xd(i),yd(i)+(ymaxp-yminp)/25.,0.,0.5,
     +       cdummy(1:k))
          end do
          call pgslw(1)
          RETURN
        END IF
        NITER=0
        IF(CREF.NE.'W')THEN
!22        WRITE(*,100)'Knot number to be refined '
          WRITE(*,100)'Knot number to be refined '
          NREF=READILIM_B('@',1,ND)
          WRITE(77,111) NREF,'# Knot number to be refined'
          IF((NREF.EQ.1).OR.(NREF.EQ.ND))THEN
            IF(CREF.NE.'3')THEN
              WRITE(*,101)'WARNING: 1st & last knot only can be '//
     +         'refined by recalculating their Y-value.'
              GOTO 21
            END IF
          END IF
        ELSE
          WRITE(*,100)'How many iterations '
          NITERT=READI_B('1')
          WRITE(77,111) NITERT,'# Number of iterations'
          NITER=0
        END IF
C pintamos con otro color la curva que va a quedar desfasada
        call pgsci(15)
        call pgline(nout,xout,yout)
        do i=1,nd
          call pgpoint(1,xd(i),yd(i),17)
          write(cdummy,*)i
          call rmblank(cdummy,cdummy,k)
          call pgptext(xd(i),yd(i)+(ymaxp-yminp)/25.,0.,0.5,
     +     cdummy(1:k))
        end do
        call pgsci(0)
C
        WRITE(CDUMMY,*)YRMSTOL
        WRITE(*,100)'YRMSTOL for DOWNHILL '
        YRMSTOL=READF_B(CDUMMY)
        WRITE(77,*) YRMSTOL,'# YRMSTOL for DOWNHILL'
C -> REFINAMOS x e y ----------------------------------------------------------
24      IF(CREF.EQ.'1')THEN
          WRITE(*,100)'Valor inicial  en X,Y: '
          WRITE(*,*) XD(NREF),YD(NREF)
          XX0(1)=XD(NREF)                      !valores iniciales para DOWNHILL
          IF(XD(NREF-1).NE.XD(NREF))THEN
            DXX0(1)=(XD(NREF)-XD(NREF-1))*0.05
          ELSEIF(XD(NREF+1).NE.XD(NREF))THEN
            DXX0(1)=(XD(NREF)-XD(NREF+1))*0.05
          ELSE
            DXX0(1)=1. !que remedio
          END IF
          XX0(2)=YD(NREF)
          IF(YD(NREF).NE.0.0)THEN
            DXX0(2)=YD(NREF)*0.05
          ELSEIF(YD(NREF-1).NE.YD(NREF))THEN
            DXX0(2)=(YD(NREF)-YD(NREF-1))*0.2
          ELSEIF(YD(NREF+1).NE.YD(NREF))THEN
            DXX0(2)=(YD(NREF)-YD(NREF+1))*0.2
          ELSE
            DXX0(2)=1. !que remedio
          END IF
          WRITE(*,100)'Running DOWNHILL...'
          CALL DOWNHILL(2,XX0,DXX0,YFUNK_SPLFIT3,1.0,0.5,2.0,YRMSTOL,
     +     XX,DXX,NEVAL)
          WRITE(*,110)'      no. of function evaluations: ',NEVAL
          XD(NREF)=XX(1)
          YD(NREF)=XX(2)
          WRITE(*,100)'Valor refinado en X,Y: '
          WRITE(*,*) XD(NREF),YD(NREF)
          SIGMA=SQRT(YFUNK_SPLFIT3(XX))
          DO I=1,ND            !actualizamos XDD para futuras llamadas a FUNK's
            XDD(I)=XD(I)
          END DO
C -> REFINAMOS x --------------------------------------------------------------
        ELSEIF(CREF.EQ.'2')THEN
          WRITE(*,100)'Valor inicial  en X: '
          WRITE(*,*) XD(NREF)
          XX0(1)=XD(NREF)                      !valores iniciales para DOWNHILL
          IF(XD(NREF-1).NE.XD(NREF))THEN
            DXX0(1)=(XD(NREF)-XD(NREF-1))*0.05
          ELSEIF(XD(NREF+1).NE.XD(NREF))THEN
            DXX0(1)=(XD(NREF)-XD(NREF+1))*0.05
          ELSE
            DXX0(1)=1. !que remedio
          END IF
          WRITE(*,100)'Running DOWNHILL...'
          CALL DOWNHILL(1,XX0,DXX0,YFUNK_SPLFIT1,1.0,0.5,2.0,YRMSTOL,
     +     XX,DXX,NEVAL)
          WRITE(*,110)'      no. of function evaluations: ',NEVAL
          XD(NREF)=XX(1)
          SIGMA=SQRT(YFUNK_SPLFIT1(XX))
          DO I=1,ND            !actualizamos XDD para futuras llamadas a FUNK's
            XDD(I)=XD(I)
          END DO
C -> REFINAMOS y --------------------------------------------------------------
        ELSEIF(CREF.EQ.'3')THEN
          WRITE(*,100)'Valor inicial  en Y: '
          WRITE(*,*) YD(NREF)
          XX0(1)=YD(NREF)                      !valores iniciales para DOWNHILL
          IF(YD(NREF).NE.0.0)THEN
            DXX0(1)=YD(NREF)*0.05
          ELSEIF(YD(1).NE.YD(ND))THEN
            DXX0(1)=(YD(1)-YD(ND))/5.
          ELSE
            DXX0(1)=1. !que remedio
          END IF
          WRITE(*,100)'Running DOWNHILL...'
          CALL DOWNHILL(1,XX0,DXX0,YFUNK_SPLFIT2,1.0,0.5,2.0,YRMSTOL,
     +     XX,DXX,NEVAL)
          WRITE(*,110)'      no. of function evaluations: ',NEVAL
          YD(NREF)=XX(1)
          SIGMA=SQRT(YFUNK_SPLFIT2(XX))
          WRITE(*,100)'Valor refinado en Y: '
          WRITE(*,*) YD(NREF)
C -> refinamos todos los nodos-------------------------------------------------
        ELSEIF(CREF.EQ.'W')THEN
          CALL RANSPL(ND,NRANND)            !ordenamos los Knots aleatoriamente
          NITER=NITER+1
          WRITE(*,109)'>>> ITERATION #',NITER
          WRITE(*,100)'     --> '
          DO I=1,ND-1                !mostramos el orden aleatorio de los Knots
            WRITE(CDUMMY,*)NRANND(I)
            CALL RMBLANK(CDUMMY,CDUMMY,L)
            WRITE(*,100)CDUMMY(1:L)//','
          END DO
          WRITE(CDUMMY,*)NRANND(ND)
          CALL RMBLANK(CDUMMY,CDUMMY,L)
          WRITE(*,101)CDUMMY(1:L)
          DO H=1,ND
            NREF=NRANND(H)
            IF((NREF.EQ.1).OR.(NREF.EQ.ND))THEN            !refinamos solo en Y
              XX0(1)=YD(NREF)
              IF(YD(NREF).NE.0.0)THEN
                DXX0(1)=YD(NREF)*0.05
              ELSEIF(YD(1).NE.YD(ND))THEN
                DXX0(1)=(YD(1)-YD(ND))/5.
              ELSE
                DXX0(1)=1. !que remedio
              END IF
              CALL DOWNHILL(1,XX0,DXX0,YFUNK_SPLFIT2,1.0,0.5,2.0,
     +         YRMSTOL,XX,DXX,NEVAL)
              YD(NREF)=XX(1)
              SIGMA=SQRT(YFUNK_SPLFIT2(XX))
            ELSE                                            !refinamos en X e Y
              XX0(1)=XD(NREF)                  !valores iniciales para DOWNHILL
              IF(XD(NREF-1).NE.XD(NREF))THEN
                DXX0(1)=(XD(NREF)-XD(NREF-1))*0.05
              ELSEIF(XD(NREF+1).NE.XD(NREF))THEN
                DXX0(1)=(XD(NREF)-XD(NREF+1))*0.05
              ELSE
                DXX0(1)=1. !que remedio
              END IF
              XX0(2)=YD(NREF)
              IF(YD(NREF).NE.0.0)THEN
                DXX0(2)=YD(NREF)*0.05
              ELSEIF(YD(NREF-1).NE.YD(NREF))THEN
                DXX0(2)=(YD(NREF)-YD(NREF-1))*0.2
              ELSEIF(YD(NREF+1).NE.YD(NREF))THEN
                DXX0(2)=(YD(NREF)-YD(NREF+1))*0.2
              ELSE
                DXX0(2)=1. !que remedio
              END IF
              WRITE(*,100)'Running DOWNHILL...'
              CALL DOWNHILL(2,XX0,DXX0,YFUNK_SPLFIT3,1.0,0.5,2.0,
     +         YRMSTOL,XX,DXX,NEVAL)
              WRITE(*,110)'      no. of function evaluations: ',NEVAL
              XD(NREF)=XX(1)
              YD(NREF)=XX(2)
              SIGMA=SQRT(YFUNK_SPLFIT3(XX))
              DO I=1,ND        !actualizamos XDD para futuras llamadas a FUNK's
                XDD(I)=XD(I)
              END DO
            END IF
          END DO
        END IF
C------------------------------------------------------------------------------
        GOTO 20
C------------------------------------------------------------------------------
900     DO K=1,NOUT
          YOUT(K)=0.
        END DO
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
109     FORMAT(A,I6,$)
110     FORMAT(A,I6)
111     FORMAT(I12,1X,A)
112     FORMAT(11X,A1,1X,A)
        END
