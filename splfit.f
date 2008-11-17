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
C
        INCLUDE 'ndatamax.inc'
        INCLUDE 'nknotsmax.inc'
C
        INTEGER READI_B
        INTEGER READILIM_B
        REAL READF_B
        REAL FPOLY
        CHARACTER*255 READC_B
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
        INTEGER NDELETE
        INTEGER ND_
        INTEGER INEW
        REAL XF(NDATAMAX),YF(NDATAMAX),EYF(NDATAMAX)
        REAL YD(NKNOTSMAX),DYD(NKNOTSMAX)
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
        REAL DELTAX
        REAL XD_(NKNOTSMAX),YD_(NKNOTSMAX),DYD_(NKNOTSMAX)
        REAL XNEW,YNEW
        CHARACTER*1 CREF
        CHARACTER*1 CMERGE
        CHARACTER*1 CREPEAT
        CHARACTER*80 CDUMMY
        LOGICAL LLUP
        LOGICAL LOOP,LOOP_
        LOGICAL LMERGE(NKNOTSMAX),LMERGE_ANY
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
          YD(I)=0. !vamos a estimar los valores en el eje Y para cada knot
        END DO
        DO I=1,NOUT
          XOUT(I)=XMIN+(XMAX-XMIN)*REAL(I-1)/REAL(NOUT-1)
        END DO
C------------------------------------------------------------------------------
C ajuste inicial a polinomios de grado 2 a cada region entre cada 2 knots
        DO I=1,ND-1
          !determinamos puntos a ajustar en cada intervalo
          K=0
          DO J=1,N
            IF((X(J).GE.XD(I)).AND.(X(J).LE.XD(I+1)))THEN
              K=K+1
              IF(K.GT.NDATAMAX)THEN
                WRITE(*,*)
                WRITE(*,101)'ERROR in SPLFIT:'
                WRITE(*,101)'>>> No. of points to fit > NDATAMAX'
                WRITE(*,*)
                GOTO 900
              END IF
              XF(K)=X(J)
              YF(K)=Y(J)
              EYF(K)=EY(J)
            END IF
          END DO
          NF=K
          !ajustamos cada intervalo
          IF(NF.EQ.0)THEN !si no hay puntos, algo va mal
            WRITE(*,101) 'FATAL ERROR in subroutine SPLFIT'
            WRITE(*,100) '--> No. of points for initial fit=0 between'//
     +       ' knots: '
            WRITE(*,*) I,I+1
            STOP '(note: this must be handled in a future)'
          ELSEIF(NF.LT.4)THEN       !tomamos el valor medio de los puntos si no
            FDUMMY=0.               !hay suficientes para el ajuste polinomico
            DO J=1,NF
              FDUMMY=FDUMMY+YF(J)
            END DO
            FDUMMY=FDUMMY/REAL(NF)
            YD(I)=YD(I)+FDUMMY
            YD(I+1)=YD(I+1)+FDUMMY
          ELSE
            CALL POLFIT(XF,YF,YF,NF,3,0,A,CHISQR,.TRUE.,-1.,1.,-1.,1.)
            YD(I)=YD(I)+FPOLY(2,A,XD(I))
            YD(I+1)=YD(I+1)+FPOLY(2,A,XD(I+1))
          END IF
        END DO
        DO I=2,ND-1 !en los extremos (I=1, I=ND) solo se ha calculado un valor
          YD(I)=YD(I)/2. !en el resto hay dos estimaciones que promediamos
        END DO
C ajustamos los splines que pasan por los knots iniciales
        CALL CUBSPL(XD,YD,ND,1,SSPL,ASPL,BSPL,CSPL)                    !IMODE=1
        I0SPL=1                  !la primera vez busca en el inicio de la tabla
        DO K=1,NOUT
          CALL CUBSPLX(XD,YD,ASPL,BSPL,CSPL,ND,I0SPL,XOUT(K),YOUT(K))
        END DO
        call pgsci(15)
        call pgpoint(nd,xd,yd,5)
        call pgline(nout,xout,yout)
        call pgsci(1)
C------------------------------------------------------------------------------
C definimos todos los datos a ajustar
        K=0
        DO J=1,N
          IF((X(J).GE.XD(1)).AND.(X(J).LE.XD(ND)))THEN
            K=K+1
            IF(K.GT.NDATAMAX)THEN
              WRITE(*,*)
              WRITE(*,101)'ERROR in SPLFIT:'
              WRITE(*,101)'>>> No. of points to fit > NDATAMAX'
              WRITE(*,*)
              GOTO 900
            END IF
            XF(K)=X(J)
            YF(K)=Y(J)
            EYF(K)=EY(J)
          END IF
        END DO
        NF=K
C------------------------------------------------------------------------------
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
C------------------------------------------------------------------------------
C valores iniciales para DOWNHILL
        DO I=1,ND
          XX0(I)=YD(I)
          IF(RMSF.GT.0.0)THEN
            DXX0(I)=RMSF/3.
          ELSEIF(ABS(YD(I)).GT.0.0)THEN
            DXX0(I)=0.05*ABS(YD(I))
          ELSE
            DXX0(I)=1.0                        !que remedio; no tenemos ni idea
          END IF
        END DO
C------------------------------------------------------------------------------
C llamamos a DOWNHILL y minimizamos la posicion en Y de todos los knots
10      NDD=ND           !ND para el COMMON block
        DO I=1,ND
          XDD(I)=XD(I)   !XD para el COMMON block
        END DO
        do i=1,nd
          print*,'downhill_1> ',i,xx0(i),dxx0(i)
        end do
        WRITE(*,100) 'Running DOWNHILL...'
        CALL DOWNHILL(ND,XX0,DXX0,YFUNK_SPLFIT,1.0,0.5,2.0,YRMSTOL,
     +   XX,DXX,NEVAL,NEVALMAX)
        WRITE(*,110) ' NEVAL: ',NEVAL
        DO J=1,ND
          YD(J)=XX(J)
          DYD(J)=DXX(J)
          print*,'downhill_2> ',j,xx(j),dxx(j)
        END DO
        SIGMA=SQRT(YFUNK_SPLFIT(YD))
20      CALL CUBSPL(XD,YD,ND,1,SSPL,ASPL,BSPL,CSPL)                    !IMODE=1
        I0SPL=1                  !la primera vez busca en el inicio de la tabla
        DO K=1,NOUT
          CALL CUBSPLX(XD,YD,ASPL,BSPL,CSPL,ND,I0SPL,XOUT(K),YOUT(K))
        END DO
C si estamos iterando seguimos con las iteraciones
        IF((NITERT.NE.0).AND.(NITER.LT.NITERT)) GOTO 24
C dibujamos ajuste final
        call pgsci(0)
        call pgline(nout,xout,yout)
        do i=1,nd
          call pgpoint(1,xd(i),yd(i),17)
          write(cdummy,*)i
          call rmblank(cdummy,cdummy,k)
          call pgptext(xd(i),yd(i)+(ymaxp-yminp)/25.,0.,0.5,
     +     cdummy(1:k))
        end do
        WRITE(*,*)
        WRITE(*,100)'Chi^2 of the fit: '
        WRITE(*,*) SIGMA
        WRITE(*,*)
C mostramos los coeficientes del ajuste
        DO I=1,ND-1
          WRITE(*,100) '>>> A,B,C coeff. #'
          WRITE(*,'(I2.2,A1,I2.2,A2,$)') I,'-',I+1,': '
          WRITE(*,*) ASPL(I),BSPL(I),CSPL(I)
        END DO
        WRITE(*,*)
C mostramos los knots del ajuste
        DO I=1,ND
          WRITE(*,100) '>>> Knot #'
          WRITE(*,'(I2.2,$)') I
          WRITE(*,100) ', (Xknot,Yknot): '
          WRITE(*,*) XD(I),YD(I)
          YKNOT(I)=YD(I)
        END DO
C si el numero de Knots es solo 2 (los extremos) no se refina el ajuste
        IF(ND.EQ.2) RETURN
C Si se quiere refinamos el ajuste
21      WRITE(*,*)
        WRITE(*,101)'(1) Refine X and Y position-> 1 Knot'
        WRITE(*,101)'(2) Refine X-position  -----> 1 Knot'
        WRITE(*,101)'(3) Refine Y-position  -----> 1 Knot'
        WRITE(*,101)'(A) Add a single new knot'
        IF(ND.GT.2)THEN
          WRITE(*,101)'(D) Delete single knot'
          WRITE(*,101)'(M) Merge "touching" knots'
        END IF
        WRITE(*,101)'(R) Refine X and Y position-> all Knots'
        WRITE(*,101)'(0) EXIT'
        WRITE(*,100)'Option '
        IF(ND.GT.2)THEN
          CREF(1:1)=READC_B('0','0123AaDdMmRr')
        ELSE
          CREF(1:1)=READC_B('0','0123AaRr')
        END IF
        WRITE(77,112) CREF,
     +   '# Option: 1=X&Y, 2=X, 3=Y, a=add, d=delete, m=merge, r=refine'
        IF(CREF.EQ.'a')CREF='A'
        IF(CREF.EQ.'d')CREF='D'
        IF(CREF.EQ.'m')CREF='M'
        IF(CREF.EQ.'r')CREF='R'
C..............................................................................
        IF(CREF.EQ.'0')THEN
          RETURN
C..............................................................................
        ELSEIF(CREF.EQ.'A')THEN
          IF(ND.EQ.NKNOTSMAX)THEN
            WRITE(*,100) 'Current number of knots: '
            WRITE(*,*) ND
            WRITE(*,101) 'ERROR: sorry, the number of knots is '//
     +       'already the maximum allowed value'
            GOTO 21
          END IF
C
          WRITE(*,101) 'Enter (x,y) coordinates of the new knot:'
          WRITE(*,100) 'New x coordinate'
          XNEW=READF_B('@')
          !estimamos el valor para X=XNEW en el ultimo ajuste
          WRITE(*,100) 'New y coordinate '
          I0SPL=1
          CALL CUBSPLX(XD,YD,ASPL,BSPL,CSPL,ND,I0SPL,XNEW,YNEW)
          WRITE(CDUMMY,*) YNEW
          YNEW=READF_B(CDUMMY)
C
          INEW=1
          CALL BINSEARCH(XD,ND,XNEW,INEW)
C
          IF(INEW.EQ.0)THEN !.................XNEW esta a la izquierda de XD(1)
            XD_(1)=XNEW
            YD_(1)=YNEW
            DYD_(1)=DYD(1) !usamos el del knot siguiente
            DO I=1,ND
              XD_(I+1)=XD(I)
              YD_(I+1)=YD(I)
              DYD_(I+1)=DYD(I)
            END DO
          ELSE !................................XNEW esta a la derecha de XD(1)
            DO I=1,INEW
              XD_(I)=XD(I)
              YD_(I)=YD(I)
              DYD_(I)=DYD(I)
            END DO
            XD_(INEW+1)=XNEW
            YD_(INEW+1)=YNEW
            DYD_(INEW+1)=DYD(INEW) !usamos el del knot anterior
            IF(INEW.LT.ND)THEN
              DO I=INEW+1,ND
                XD_(I+1)=XD(I)
                YD_(I+1)=YD(I)
                DYD_(I+1)=DYD(I)
              END DO
            END IF
          END IF
          ND_=ND+1
C..............................................................................
        ELSEIF(CREF.EQ.'D')THEN
          WRITE(*,100) 'Knot number to be deleted'
          NDELETE=READILIM_B('@',2,ND-1) !no podemos eliminar los extremos
          WRITE(77,111) NDELETE,'# knot number to be deleted'
          ND_=ND-1
          DO I=1,ND-1
            IF(I.LT.NDELETE)THEN
              XD_(I)=XD(I)
              YD_(I)=YD(I)
              DYD_(I)=DYD(I)
            ELSE
              XD_(I)=XD(I+1)
              YD_(I)=YD(I+1)
              DYD_(I)=DYD(I+1)
            END IF
          END DO
C..............................................................................
        ELSEIF(CREF.EQ.'M')THEN
          DO I=1,ND
            LMERGE(I)=.FALSE.
          END DO
          LMERGE_ANY=.FALSE.
          LOOP=.TRUE.
          DO WHILE(LOOP)
            WRITE(*,*)
            LOOP_=.TRUE.
            DO WHILE(LOOP_)
              WRITE(*,100) 'Delta_X to merge knots '
              WRITE(CDUMMY,*) (XD(ND)-XD(1))/REAL(N)
              DELTAX=READF_B(CDUMMY)
              WRITE(77,*) DELTAX,'# Delta_X to merge knots'
              IF(DELTAX.GE.(XD(ND)-XD(1))/2.0)THEN
                WRITE(*,101) 'ERROR: Delta_X is too large!'
                WRITE(*,100) '       Delta_X must be < '
                WRITE(*,*) (XD(ND)-XD(1))/2.0
                WRITE(*,*)
              ELSE
                LOOP_=.FALSE.
              END IF
            END DO
            DO I=1,ND-1
              IF((XD(I+1)-XD(I)).LE.DELTAX)THEN
                WRITE(*,100) 'Knots suitable for merging: '
                WRITE(*,'(I2,2X,I2)') I,I+1
                LMERGE(I)=.TRUE.
                LMERGE_ANY=.TRUE.
              END IF
            END DO
            IF(LMERGE_ANY)THEN
              WRITE(*,100) 'Are you proceeding with merging (y/n) '
              CMERGE(1:1)=READC_B('y','yn')
              WRITE(77,112) CMERGE,'# are you proceeding with merging?'
              IF(CMERGE.EQ.'n') GOTO 21
              !fusionamos los knots (nota: LMERGE(ND)=.FALSE. siempre)
              CALL MERGE_KNOTS(ND,XD,YD,DYD,LMERGE,ND_,XD_,YD_,DYD_)
              LOOP=.FALSE.
            ELSE
              WRITE(*,101) 'WARNING: No "touching" knots found!'
              WRITE(*,100) 'Do you want to modify Delta_X (y/n) '
              CREPEAT(1:1)=READC_B('y','yn')
              WRITE(77,112) CREPEAT,'# modify Delta_X?'
              IF(CREPEAT(1:1).EQ.'n') GOTO 21
            END IF
          END DO
        END IF
C------------------------------------------------------------------------------
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
C------------------------------------------------------------------------------
C si hemos cambiado el numero de knots, repetimos el ajuste
        IF((CREF.EQ.'A').OR.(CREF.EQ.'D').OR.(CREF.EQ.'M'))THEN
          ND=ND_
          DO I=1,ND_
            XD(I)=XD_(I)
            YD(I)=YD_(I)
            DYD(I)=DYD_(I)
          END DO
          DO I=1,ND
            XX0(I)=YD(I)
            DXX0(I)=DYD(I)
          END DO
          GOTO 10
        END IF
C------------------------------------------------------------------------------
        NITER=0
        IF(CREF.NE.'R')THEN
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
C------------------------------------------------------------------------------
C -> REFINAMOS x e y ----------------------------------------------------------
24      IF(CREF.EQ.'1')THEN
          XX0(1)=XD(NREF)                      !valores iniciales para DOWNHILL
          IF(XD(NREF-1).NE.XD(NREF+1))THEN
            DXX0(1)=(XD(NREF+1)-XD(NREF-1))*0.5
          ELSE
            DXX0(1)=1. !que remedio
          END IF
          XX0(2)=YD(NREF)
          IF(YD(NREF-1).NE.YD(NREF+1))THEN
            DXX0(2)=(YD(NREF+1)-YD(NREF-1))*0.5
          ELSEIF(YD(NREF).NE.0.0)THEN
            DXX0(2)=YD(NREF)*0.05
          ELSE
            DXX0(2)=1. !que remedio
          END IF
          WRITE(*,130) 'knot#',NREF,'> '
          WRITE(*,100) 'Initial values X,Y: '
          WRITE(*,*) XX0(1),XX0(2)
          CALL DOWNHILL(2,XX0,DXX0,YFUNK_SPLFIT3,1.0,0.5,2.0,YRMSTOL,
     +     XX,DXX,NEVAL)
          WRITE(*,131) 'knot#',NREF,'> NEVAL: ',NEVAL
          XD(NREF)=XX(1)
          YD(NREF)=XX(2)
          WRITE(*,130) 'knot#',NREF,'> '
          WRITE(*,100) 'Refined values X,Y: '
          WRITE(*,*) XX(1),XX(2)
          SIGMA=SQRT(YFUNK_SPLFIT3(XX))
          DO I=1,ND            !actualizamos XDD para futuras llamadas a FUNK's
            XDD(I)=XD(I)
          END DO
C -> REFINAMOS x --------------------------------------------------------------
        ELSEIF(CREF.EQ.'2')THEN
          WRITE(*,100)'Valor inicial  en X: '
          WRITE(*,*) XD(NREF)
          XX0(1)=XD(NREF)                      !valores iniciales para DOWNHILL
          IF(XD(NREF-1).NE.XD(NREF+1))THEN
            DXX0(1)=(XD(NREF+1)-XD(NREF-1))*0.5
          ELSE
            DXX0(1)=1. !que remedio
          END IF
          WRITE(*,130) 'knot#',NREF,'> '
          WRITE(*,100) 'Initial value X: '
          WRITE(*,*) XX0(1)
          CALL DOWNHILL(1,XX0,DXX0,YFUNK_SPLFIT1,1.0,0.5,2.0,YRMSTOL,
     +     XX,DXX,NEVAL)
          WRITE(*,131) 'knot#',NREF,'> NEVAL: ',NEVAL
          XD(NREF)=XX(1)
          WRITE(*,130) 'knot#',NREF,'> '
          WRITE(*,100) 'Refined value X: '
          WRITE(*,*) XX(1)
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
          WRITE(*,130) 'knot#',NREF,'> '
          WRITE(*,100) 'Initial value Y: '
          WRITE(*,*) XX0(1)
          CALL DOWNHILL(1,XX0,DXX0,YFUNK_SPLFIT2,1.0,0.5,2.0,YRMSTOL,
     +     XX,DXX,NEVAL)
          WRITE(*,131) 'knot#',NREF,'> NEVAL: ',NEVAL
          YD(NREF)=XX(1)
          WRITE(*,130) 'knot#',NREF,'> '
          WRITE(*,100) 'Refined value Y: '
          WRITE(*,*) XX(1)
          SIGMA=SQRT(YFUNK_SPLFIT2(XX))
          WRITE(*,100)'Valor refinado en Y: '
          WRITE(*,*) YD(NREF)
C -> refinamos todos los nodos-------------------------------------------------
        ELSEIF(CREF.EQ.'R')THEN
          CALL RANSPL(ND,NRANND)            !ordenamos los Knots aleatoriamente
          NITER=NITER+1
          WRITE(*,*)
          WRITE(*,109)'>>> ITERATION #',NITER
          WRITE(*,100)' --> '
          DO I=1,ND-1                !mostramos el orden aleatorio de los Knots
            WRITE(CDUMMY,*)NRANND(I)
            CALL RMBLANK(CDUMMY,CDUMMY,L)
            WRITE(*,100)CDUMMY(1:L)//','
          END DO
          WRITE(CDUMMY,*)NRANND(ND)
          CALL RMBLANK(CDUMMY,CDUMMY,L)
          WRITE(*,101)CDUMMY(1:L)
          DO H=1,ND
            WRITE(*,*)
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
              WRITE(*,130) 'knot#',NREF,'> '
              WRITE(*,100) 'Initial value Y: '
              WRITE(*,*) XX0(1)
              CALL DOWNHILL(1,XX0,DXX0,YFUNK_SPLFIT2,1.0,0.5,2.0,
     +         YRMSTOL,XX,DXX,NEVAL)
              WRITE(*,131) 'knot#',NREF,'> NEVAL: ',NEVAL
              YD(NREF)=XX(1)
              WRITE(*,130) 'knot#',NREF,'> '
              WRITE(*,100) 'Refined value Y: '
              WRITE(*,*) XX(1)
              SIGMA=SQRT(YFUNK_SPLFIT2(XX))
            ELSE                                            !refinamos en X e Y
              XX0(1)=XD(NREF)                  !valores iniciales para DOWNHILL
              IF(XD(NREF-1).NE.XD(NREF+1))THEN
                DXX0(1)=(XD(NREF+1)-XD(NREF-1))*0.5
              ELSE
                DXX0(1)=1. !que remedio
              END IF
              XX0(2)=YD(NREF)
              IF(YD(NREF-1).NE.YD(NREF+1))THEN
                DXX0(2)=(YD(NREF+1)-YD(NREF-1))*0.5
              ELSEIF(YD(NREF).NE.0.0)THEN
                DXX0(2)=YD(NREF)*0.05
              ELSE
                DXX0(2)=1. !que remedio
              END IF
              WRITE(*,130) 'knot#',NREF,'> '
              WRITE(*,100) 'Initial values X,Y: '
              WRITE(*,*) XX0(1),XX0(2)
              CALL DOWNHILL(2,XX0,DXX0,YFUNK_SPLFIT3,1.0,0.5,2.0,
     +         YRMSTOL,XX,DXX,NEVAL)
              WRITE(*,131) 'knot#',NREF,'> NEVAL: ',NEVAL
              XD(NREF)=XX(1)
              YD(NREF)=XX(2)
              WRITE(*,130) 'knot#',NREF,'> '
              WRITE(*,100) 'Refined values X,Y: '
              WRITE(*,*) XX(1),XX(2)
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
130     FORMAT(A5,I2.2,A2,$)
131     FORMAT(A5,I2.2,A9,I8)
        END
