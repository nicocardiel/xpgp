C
C******************************************************************************
C Prepara los datos para la realizacion de regresiones lineales.
C La subrutina tambien realiza estimaciones de los errores
C mediante simulacines de Monte Carlo:
C  <b> bootstrap: se generan muestras de N elementos, en las cuales algunos
C                 datos aparecen duplicados y otros no aparecen. No se tiene
C                 en cuenta los errores.
C  <c> jackknife: se generan N muestras de N-1 elementos, en las cuales se
C                 elimina, de forma sucesiva, uno de los puntos. No se tiene
C                 en cuenta los errores.
C  <d> bootstrap+err: se generan muestras de N elementos, en las cuales cada
C                 punto se modifica aleatoriamente en funcion de sus errores
C                 (asumiendo que estos siguien una distribucion normal).
C
        SUBROUTINE SLINREG(NB0)
        IMPLICIT NONE
        INTEGER NB0
C
        INCLUDE 'nsimul.inc'
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
        INTEGER READILIM_B
        REAL READF_B
        REAL RANRED
        CHARACTER*255 READC_B
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
        DOUBLE PRECISION PI
        PARAMETER (  PI=3.141592653589793D0) !pi
        DOUBLE PRECISION SQR2
        PARAMETER (SQR2=1.414213562373095D0) !sqrt(2)
C
        INTEGER NSEED,IRAN
        INTEGER L1,L2
        INTEGER I,K,NSIMUL
        INTEGER NPLOT
        INTEGER NF,NDATA
        INTEGER NB
        INTEGER IEXPAND
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        INTEGER OLDLS,OLDCI
        REAL XMIN,XMAX,YMIN,YMAX
        REAL XMIN0,XMAX0,XMINF,XMAXF
        REAL XX(2),YY(2),X0,Y0
        REAL XDATA(NDATAMAX,NBUFFMAX),EXDATA(NDATAMAX,NBUFFMAX)
        REAL YDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        DOUBLE PRECISION XF(NDATAMAX),YF(NDATAMAX)
        DOUBLE PRECISION EXF(NDATAMAX),EYF(NDATAMAX)
        DOUBLE PRECISION XMINFIT,XMAXFIT
        DOUBLE PRECISION A(6),VARA(6),B(6),VARB(6)
        DOUBLE PRECISION AA(6),VARAA(6),BB(6),VARBB(6)
        DOUBLE PRECISION A_BOOT(6,NDATAMAX),B_BOOT(6,NDATAMAX)
        DOUBLE PRECISION AVEA_BOOT(6),AVEB_BOOT(6)
        DOUBLE PRECISION VARA_BOOT(6),VARB_BOOT(6)
        DOUBLE PRECISION A_JACK(6,NDATAMAX),B_JACK(6,NDATAMAX)
        DOUBLE PRECISION AVEA_JACK(6),AVEB_JACK(6)
        DOUBLE PRECISION VARA_JACK(6),VARB_JACK(6)
        DOUBLE PRECISION A_BRAN(6,NDATAMAX),B_BRAN(6,NDATAMAX)
        DOUBLE PRECISION AVEA_BRAN(6),AVEB_BRAN(6)
        DOUBLE PRECISION VARA_BRAN(6),VARB_BRAN(6)
        DOUBLE PRECISION A_ERRY,VARA_ERRY,B_ERRY,VARB_ERRY
        DOUBLE PRECISION A_EYSI(NDATAMAX),B_EYSI(NDATAMAX)
        DOUBLE PRECISION AVEA_EYSI,VARA_EYSI,AVEB_EYSI,VARB_EYSI
        DOUBLE PRECISION A_ERXY,VARA_ERXY,B_ERXY,VARB_ERXY
        DOUBLE PRECISION A_EXYS(NDATAMAX),B_EXYS(NDATAMAX)
        DOUBLE PRECISION AVEA_EXYS,VARA_EXYS,AVEB_EXYS,VARB_EXYS
        DOUBLE PRECISION RAN1,RAN2
        CHARACTER*1 CWHICH
        CHARACTER*50 CXMINF,CXMAXF
        CHARACTER*50 DATAKEY(NBUFFMAX),DATAKEY_
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LBATCH
        LOGICAL LANYPLOT
C
        COMMON/BLKLXYERR/LXERR,LYERR
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKBUFFERS3/NSYMBBUFF,LWBUFF
        COMMON/BLKSETTINGS1/XMIN,XMAX,YMIN,YMAX
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKLBATCH/LBATCH
        COMMON/BLKSETTINGS7/IEXPAND
        COMMON/BLKSETTINGS8B/DATAKEY
C------------------------------------------------------------------------------
        NSEED=-1
        NF=NDATABUFF(NB0)
        IF(NF.LT.2)THEN
          WRITE(*,100) 'ERROR: number of points for fit: '
          WRITE(*,*) NF
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          RETURN
        END IF
C------------------------------------------------------------------------------
        WRITE(*,100) 'Working...'
C Perform the linear regresion
        DO I=1,NF
          XF(I)=DBLE(XDATA(I,NB0))
          YF(I)=DBLE(YDATA(I,NB0))
        END DO
        CALL LINREG(NF,XF,YF,A,B,VARA,VARB)
C------------------------------------------------------------------------------
C Bootstrap: resampling procedure to evaluate regression coefficients & errors
C Nota: comprobamos que, al menos, hay dos puntos distintos para realizar el
C ajuste
        NSIMUL=0
        DO WHILE(NSIMUL.LT.NSIMULMAX)
          DO I=1,NF
            IRAN=INT(REAL(NF)*RANRED(NSEED))+1
            XF(I)=DBLE(XDATA(IRAN,NB0))
            YF(I)=DBLE(YDATA(IRAN,NB0))
          END DO
          XMINFIT=XF(1)
          XMAXFIT=XMINFIT
          DO I=2,NF
            IF(XMINFIT.GT.XF(I)) XMINFIT=XF(I)
            IF(XMAXFIT.LT.XF(I)) XMAXFIT=XF(I)
          END DO
          IF(XMINFIT.NE.XMAXFIT)THEN
            NSIMUL=NSIMUL+1
            CALL LINREG(NF,XF,YF,AA,BB,VARAA,VARBB)
            DO K=1,6
              A_BOOT(K,NSIMUL)=AA(K)
              B_BOOT(K,NSIMUL)=BB(K)
            END DO
          END IF
        END DO
C mean and standard deviation of the regression coefficients
        DO K=1,6
          AVEA_BOOT(K)=0.D0
          DO NSIMUL=1,NSIMULMAX
            AVEA_BOOT(K)=AVEA_BOOT(K)+A_BOOT(K,NSIMUL)
          END DO
          AVEA_BOOT(K)=AVEA_BOOT(K)/DBLE(NSIMULMAX)
          VARA_BOOT(K)=0.D0
          DO NSIMUL=1,NSIMULMAX
            VARA_BOOT(K)=VARA_BOOT(K)+
     >       (A_BOOT(K,NSIMUL)-AVEA_BOOT(K))*
     >       (A_BOOT(K,NSIMUL)-AVEA_BOOT(K))
          END DO
          VARA_BOOT(K)=VARA_BOOT(K)/DBLE(NSIMULMAX-1)
          AVEB_BOOT(K)=0.D0
          DO NSIMUL=1,NSIMULMAX
            AVEB_BOOT(K)=AVEB_BOOT(K)+B_BOOT(K,NSIMUL)
          END DO
          AVEB_BOOT(K)=AVEB_BOOT(K)/DBLE(NSIMULMAX)
          VARB_BOOT(K)=0.D0
          DO NSIMUL=1,NSIMULMAX
            VARB_BOOT(K)=VARB_BOOT(K)+
     >       (B_BOOT(K,NSIMUL)-AVEB_BOOT(K))*
     >       (B_BOOT(K,NSIMUL)-AVEB_BOOT(K))
          END DO
          VARB_BOOT(K)=VARB_BOOT(K)/DBLE(NSIMULMAX-1)
        END DO
C------------------------------------------------------------------------------
C Jackknife: resampling procedure to evaluate regression coefficients & errors
        IF(NF.GE.3)THEN
          NSIMUL=0
          DO WHILE(NSIMUL.LT.NF)
            DO I=1,NF
              IF(I.LT.NSIMUL)THEN
                XF(I)=DBLE(XDATA(I,NB0))
                YF(I)=DBLE(YDATA(I,NB0))
              ELSEIF(I.GT.NSIMUL)THEN
                XF(I-1)=DBLE(XDATA(I,NB0))
                YF(I-1)=DBLE(YDATA(I,NB0))
              END IF
            END DO
            XMINFIT=XF(1)
            XMAXFIT=XMINFIT
            DO I=2,NF-1
              IF(XMINFIT.GT.XF(I)) XMINFIT=XF(I)
              IF(XMAXFIT.LT.XF(I)) XMAXFIT=XF(I)
            END DO
            IF(XMINFIT.NE.XMAXFIT)THEN
              NSIMUL=NSIMUL+1
              CALL LINREG(NF-1,XF,YF,AA,BB,VARAA,VARBB)
              DO K=1,6
                A_JACK(K,NSIMUL)=AA(K)
                B_JACK(K,NSIMUL)=BB(K)
              END DO
            END IF
          END DO
C mean and standard deviation of the regression coefficients
          DO K=1,6
            AVEA_JACK(K)=0.D0
            DO NSIMUL=1,NF
              AVEA_JACK(K)=AVEA_JACK(K)+A_JACK(K,NSIMUL)
            END DO
            AVEA_JACK(K)=AVEA_JACK(K)/DBLE(NF)
            VARA_JACK(K)=0.D0
            DO NSIMUL=1,NF
              VARA_JACK(K)=VARA_JACK(K)+
     >         (A_JACK(K,NSIMUL)-AVEA_JACK(K))*
     >         (A_JACK(K,NSIMUL)-AVEA_JACK(K))
            END DO
            VARA_JACK(K)=VARA_JACK(K)/DBLE(NF-1)
            AVEB_JACK(K)=0.D0
            DO NSIMUL=1,NF
              AVEB_JACK(K)=AVEB_JACK(K)+B_JACK(K,NSIMUL)
            END DO
            AVEB_JACK(K)=AVEB_JACK(K)/DBLE(NF)
            VARB_JACK(K)=0.D0
            DO NSIMUL=1,NF
              VARB_JACK(K)=VARB_JACK(K)+
     >         (B_JACK(K,NSIMUL)-AVEB_JACK(K))*
     >         (B_JACK(K,NSIMUL)-AVEB_JACK(K))
            END DO
            VARB_JACK(K)=VARB_JACK(K)/DBLE(NF-1)
          END DO
c..............................................................................
        ELSE
          DO K=1,6
            AVEA_JACK(K)=0.D0
            VARA_JACK(K)=0.D0
            AVEB_JACK(K)=0.D0
            VARB_JACK(K)=0.D0
          END DO
        END IF
C------------------------------------------------------------------------------
C Bootstrap+err: resampling procedure to evaluate regression coefficients &
C errors using the errors in both variables
        IF(LXERR(NB0).OR.LYERR(NB0))THEN
          NSIMUL=0
          DO WHILE(NSIMUL.LT.NSIMULMAX)
            DO I=1,NF
              XF(I)=DBLE(XDATA(I,NB0))
              IF(LXERR(NB0))THEN
                RAN1=DBLE(RANRED(NSEED))
                RAN2=DBLE(RANRED(NSEED))
                XF(I)=XF(I)+SQR2*DBLE(EXDATA(I,NB0))*
     >           DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
              END IF
              YF(I)=DBLE(YDATA(I,NB0))
              IF(LYERR(NB0))THEN
                RAN1=DBLE(RANRED(NSEED))
                RAN2=DBLE(RANRED(NSEED))
                YF(I)=YF(I)+SQR2*DBLE(EYDATA(I,NB0))*
     >           DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
              END IF
            END DO
            XMINFIT=XF(1)
            XMAXFIT=XMINFIT
            DO I=2,NF
              IF(XMINFIT.GT.XF(I)) XMINFIT=XF(I)
              IF(XMAXFIT.LT.XF(I)) XMAXFIT=XF(I)
            END DO
            IF(XMINFIT.NE.XMAXFIT)THEN
              NSIMUL=NSIMUL+1
              CALL LINREG(NF,XF,YF,AA,BB,VARAA,VARBB)
              DO K=1,6
                A_BRAN(K,NSIMUL)=AA(K)
                B_BRAN(K,NSIMUL)=BB(K)
              END DO
            END IF
          END DO
C mean and standard deviation of the regression coefficients
          DO K=1,6
            AVEA_BRAN(K)=0.D0
            DO NSIMUL=1,NSIMULMAX
              AVEA_BRAN(K)=AVEA_BRAN(K)+A_BRAN(K,NSIMUL)
            END DO
            AVEA_BRAN(K)=AVEA_BRAN(K)/DBLE(NSIMULMAX)
            VARA_BRAN(K)=0.D0
            DO NSIMUL=1,NSIMULMAX
              VARA_BRAN(K)=VARA_BRAN(K)+
     >         (A_BRAN(K,NSIMUL)-AVEA_BRAN(K))*
     >         (A_BRAN(K,NSIMUL)-AVEA_BRAN(K))
            END DO
            VARA_BRAN(K)=VARA_BRAN(K)/DBLE(NSIMULMAX-1)
            AVEB_BRAN(K)=0.D0
            DO NSIMUL=1,NSIMULMAX
              AVEB_BRAN(K)=AVEB_BRAN(K)+B_BRAN(K,NSIMUL)
            END DO
            AVEB_BRAN(K)=AVEB_BRAN(K)/DBLE(NSIMULMAX)
            VARB_BRAN(K)=0.D0
            DO NSIMUL=1,NSIMULMAX
              VARB_BRAN(K)=VARB_BRAN(K)+
     >         (B_BRAN(K,NSIMUL)-AVEB_BRAN(K))*
     >         (B_BRAN(K,NSIMUL)-AVEB_BRAN(K))
            END DO
            VARB_BRAN(K)=VARB_BRAN(K)/DBLE(NSIMULMAX-1)
          END DO
        ELSE
          DO K=1,6
            AVEA_BRAN(K)=A(K)
            AVEB_BRAN(K)=B(K)
            VARA_BRAN(K)=0.D0
            VARB_BRAN(K)=0.D0
          END DO
        END IF
C------------------------------------------------------------------------------
C Ajuste pesado con errores en Y
        IF(LYERR(NB0))THEN
          DO I=1,NF
            XF(I)=DBLE(XDATA(I,NB0))
            YF(I)=DBLE(YDATA(I,NB0))
            EYF(I)=DBLE(EYDATA(I,NB0))
          END DO
          CALL LINREGEY(NF,XF,YF,EYF,A_ERRY,B_ERRY,VARA_ERRY,VARB_ERRY)
        ELSE
          A_ERRY=A(1)
          B_ERRY=B(1)
          VARA_ERRY=0.D0
          VARB_ERRY=0.D0
        END IF
C------------------------------------------------------------------------------
C Idem, pero simulando muestras distintas con los errores
        IF(LYERR(NB0))THEN
          DO NSIMUL=1,NSIMULMAX
            DO I=1,NF
              XF(I)=DBLE(XDATA(I,NB0))
              YF(I)=DBLE(YDATA(I,NB0))
              EYF(I)=DBLE(EYDATA(I,NB0))
              RAN1=DBLE(RANRED(NSEED))
              RAN2=DBLE(RANRED(NSEED))
              YF(I)=YF(I)+SQR2*DBLE(EYF(I))*
     >         DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
            END DO
            CALL LINREGEY(NF,XF,YF,EYF,AA(1),BB(1),VARAA(1),VARBB(1))
            A_EYSI(NSIMUL)=AA(1)
            B_EYSI(NSIMUL)=BB(1)
          END DO
          AVEA_EYSI=0.D0
          DO NSIMUL=1,NSIMULMAX
            AVEA_EYSI=AVEA_EYSI+A_EYSI(NSIMUL)
          END DO
          AVEA_EYSI=AVEA_EYSI/DBLE(NSIMULMAX)
          VARA_EYSI=0.D0
          DO NSIMUL=1,NSIMULMAX
            VARA_EYSI=VARA_EYSI+
     >       (A_EYSI(NSIMUL)-AVEA_EYSI)*
     >       (A_EYSI(NSIMUL)-AVEA_EYSI)
          END DO
          VARA_EYSI=VARA_EYSI/DBLE(NSIMULMAX-1)
          AVEB_EYSI=0.D0
          DO NSIMUL=1,NSIMULMAX
            AVEB_EYSI=AVEB_EYSI+B_EYSI(NSIMUL)
          END DO
          AVEB_EYSI=AVEB_EYSI/DBLE(NSIMULMAX)
          VARB_EYSI=0.D0
          DO NSIMUL=1,NSIMULMAX
            VARB_EYSI=VARB_EYSI+
     >       (B_EYSI(NSIMUL)-AVEB_EYSI)*
     >       (B_EYSI(NSIMUL)-AVEB_EYSI)
          END DO
          VARB_EYSI=VARB_EYSI/DBLE(NSIMULMAX-1)
        ELSE
          AVEA_EYSI=A(1)
          AVEB_EYSI=B(1)
          VARA_EYSI=0.D0
          VARB_EYSI=0.D0
        END IF
C------------------------------------------------------------------------------
C Ajuste pesado con errores en X e Y (usando un metodo numerico ---DOWNHILL---)
C Nota: si no hay errores en X, este ajuste es identico a un ajuste pesado
C minimizando la distancia en Y. No es necesario realizar la simulacion
C numerica (que de hecho conduce a interminables iteraciones de DOWNHILL).
        IF(LXERR(NB0))THEN
          DO I=1,NF
            XF(I)=DBLE(XDATA(I,NB0))
            YF(I)=DBLE(YDATA(I,NB0))
            IF(LXERR(NB0))THEN
              EXF(I)=DBLE(EXDATA(I,NB0))
            ELSE
              EXF(I)=0.D0
            END IF
            IF(LYERR(NB0))THEN
              EYF(I)=DBLE(EYDATA(I,NB0))
            ELSE
              EYF(I)=0.D0
            END IF
          END DO
          CALL LINREGEXY(NF,XF,YF,EXF,EYF,
     >     A_ERXY,B_ERXY,VARA_ERXY,VARB_ERXY)
        ELSEIF(LYERR(NB0))THEN
          A_ERXY=A_ERRY
          B_ERXY=B_ERRY
          VARA_ERXY=VARA_ERRY
          VARB_ERXY=VARB_ERRY
        ELSE
          A_ERXY=A(4)
          B_ERXY=B(4)
          VARA_ERXY=0.D0
          VARB_ERXY=0.D0
        END IF
C Idem, pero simulando muestras distintas con los errores
        IF(LXERR(NB0))THEN
          DO NSIMUL=1,NSIMULMAX
            DO I=1,NF
              XF(I)=DBLE(XDATA(I,NB0))
              YF(I)=DBLE(YDATA(I,NB0))
              IF(LXERR(NB0))THEN
                EXF(I)=DBLE(EXDATA(I,NB0))
                RAN1=DBLE(RANRED(NSEED))
                RAN2=DBLE(RANRED(NSEED))
                XF(I)=XF(I)+SQR2*DBLE(EXF(I))*
     >           DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
              ELSE
                EXF(I)=0.D0
              END IF
              IF(LXERR(NB0))THEN
                EYF(I)=DBLE(EYDATA(I,NB0))
                RAN1=DBLE(RANRED(NSEED))
                RAN2=DBLE(RANRED(NSEED))
                YF(I)=YF(I)+SQR2*DBLE(EYF(I))*
     >           DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
              ELSE
                EYF(I)=0.D0
              END IF
            END DO
            CALL LINREGEXY(NF,XF,YF,EXF,EYF,
     >       AA(1),BB(1),VARAA(1),VARBB(1))
            A_EXYS(NSIMUL)=AA(1)
            B_EXYS(NSIMUL)=BB(1)
          END DO
          AVEA_EXYS=0.D0
          DO NSIMUL=1,NSIMULMAX
            AVEA_EXYS=AVEA_EXYS+A_EXYS(NSIMUL)
          END DO
          AVEA_EXYS=AVEA_EXYS/DBLE(NSIMULMAX)
          VARA_EXYS=0.D0
          DO NSIMUL=1,NSIMULMAX
            VARA_EXYS=VARA_EXYS+
     >       (A_EXYS(NSIMUL)-AVEA_EXYS)*
     >       (A_EXYS(NSIMUL)-AVEA_EXYS)
          END DO
          VARA_EXYS=VARA_EXYS/DBLE(NSIMULMAX-1)
          AVEB_EXYS=0.D0
          DO NSIMUL=1,NSIMULMAX
            AVEB_EXYS=AVEB_EXYS+B_EXYS(NSIMUL)
          END DO
          AVEB_EXYS=AVEB_EXYS/DBLE(NSIMULMAX)
          VARB_EXYS=0.D0
          DO NSIMUL=1,NSIMULMAX
            VARB_EXYS=VARB_EXYS+
     >       (B_EXYS(NSIMUL)-AVEB_EXYS)*
     >       (B_EXYS(NSIMUL)-AVEB_EXYS)
          END DO
          VARB_EXYS=VARB_EXYS/DBLE(NSIMULMAX-1)
        ELSEIF(LYERR(NB0))THEN
          AVEA_EXYS=AVEA_EYSI
          AVEB_EXYS=AVEB_EYSI
          VARA_EXYS=0.D0
          VARB_EXYS=0.D0
        ELSE
          AVEA_EXYS=A(4)
          AVEB_EXYS=B(4)
          VARA_EXYS=0.D0
          VARB_EXYS=0.D0
        END IF
C------------------------------------------------------------------------------
C determinamos los limites para realizar la representacion grafica 
        XMIN0=(1.+REAL(IEXPAND)/100.)*XMIN+REAL(IEXPAND)/100.*XMAX
        XMIN0=XMIN0/(1.+2.*REAL(IEXPAND)/100.)
        XMAX0=(1.+REAL(IEXPAND)/100.)*XMAX+REAL(IEXPAND)/100.*XMIN
        XMAX0=XMAX0/(1.+2.*REAL(IEXPAND)/100.)
        IF(XMIN.LT.XMIN0)THEN
          XMINF=XMIN0
        ELSE
          XMINF=XMIN
        END IF
        IF(XMAX.GT.XMAX0)THEN
          XMAXF=XMAX0
        ELSE
          XMAXF=XMAX
        END IF
        WRITE(CXMINF,*) XMINF
        WRITE(CXMAXF,*) XMAXF
C dibujamos los ajustes
        WRITE(*,101) '...OK!'
        NPLOT=1
        LANYPLOT=.FALSE.               !todavia no hemos dibujado ningun ajuste
        CALL PGQLS(OLDLS)
        CALL PGQCI(OLDCI)
        DO WHILE(NPLOT.NE.0)
          WRITE(*,*)
          WRITE(*,100) 'Linear correlation coefficient r: '
          WRITE(*,*) SQRT(B(1)/B(2))
          WRITE(*,100) 'Determination coefficient   r**2: '
          WRITE(*,*) B(1)/B(2)
          WRITE(*,*)
          WRITE(*,100) '<1> OLS(Y|X)              '
          WRITE(*,100) '<2> OLS(X|Y)              '
          WRITE(*,101) '<3> OLS bisector '
          WRITE(*,100) '<4> Orthogonal            '
          WRITE(*,100) '<5> Reduced major axis    '
          WRITE(*,101) '<6> Mean OLS'
          WRITE(*,*)
          WRITE(*,100) 'No. of linear fit to be plotted (0=none)'
          NPLOT=READILIM_B('0',0,6)
          WRITE(77,111) NPLOT,'# No. of linear fit (1...6, 0=none)'
          IF(NPLOT.NE.0)THEN
            IF(NPLOT.EQ.1)THEN
              WRITE(*,101) '<1> OLS(Y|X)'
            ELSEIF(NPLOT.EQ.2)THEN
              WRITE(*,101) '<2> OLS(X|Y)'
            ELSEIF(NPLOT.EQ.3)THEN
              WRITE(*,101) '<3> OLS bisector'
            ELSEIF(NPLOT.EQ.4)THEN
              WRITE(*,101) '<4> Orthogonal'
            ELSEIF(NPLOT.EQ.5)THEN
              WRITE(*,101) '<5> Reduced major axis'
            ELSEIF(NPLOT.EQ.6)THEN
              WRITE(*,101) '<6> Mean OLS'
            END IF
            WRITE(*,100) '     formulae: <a> '
            WRITE(*,*) A(NPLOT),DSQRT(VARA(NPLOT)),
     >       B(NPLOT),DSQRT(VARB(NPLOT))
            WRITE(*,100) '    bootstrap: <b> '
            WRITE(*,*) AVEA_BOOT(NPLOT),DSQRT(VARA_BOOT(NPLOT)),
     >       AVEB_BOOT(NPLOT),DSQRT(VARB_BOOT(NPLOT))
            WRITE(*,100) '    jackknife: <c> '
            WRITE(*,*) AVEA_JACK(NPLOT),DSQRT(VARA_JACK(NPLOT)),
     >       AVEB_JACK(NPLOT),DSQRT(VARB_JACK(NPLOT))
            WRITE(*,100) 'bootstrap+err: <d> '
            WRITE(*,*) AVEA_BRAN(NPLOT),DSQRT(VARA_BRAN(NPLOT)),
     >       AVEB_BRAN(NPLOT),DSQRT(VARB_BRAN(NPLOT))
            IF(NPLOT.EQ.1)THEN
              WRITE(*,100) ' weighting EY: <e> '
              WRITE(*,*) A_ERRY,DSQRT(VARA_ERRY),B_ERRY,DSQRT(VARB_ERRY)
              WRITE(*,100) 'weig.EY+boot.: <f> '
              WRITE(*,*) AVEA_EYSI,DSQRT(VARA_EYSI),
     >         AVEB_EYSI,DSQRT(VARB_EYSI)
            ELSEIF(NPLOT.EQ.4)THEN
              WRITE(*,100) 'weig. EX & EY: <e> '
              WRITE(*,*) A_ERXY,DSQRT(VARA_ERXY),B_ERXY,DSQRT(VARB_ERXY)
              WRITE(*,100) 'we. EX,EY+boo: <f> '
              WRITE(*,*) AVEA_EXYS,DSQRT(VARA_EXYS),
     >         AVEB_EXYS,DSQRT(VARB_EXYS)
            END IF
            IF((NPLOT.EQ.1).OR.(NPLOT.EQ.4))THEN
              WRITE(*,100) 'Which one (a/b/c/d/e/f) '
              CWHICH(1:1)=READC_B('a','abcdef')
            ELSE
              WRITE(*,100) 'Which one (a/b/c/d) '
              CWHICH(1:1)=READC_B('a','abcd')
            END IF
            WRITE(77,112) CWHICH,'# type of fit (a/b/c/d(e/f))'
            IF(LANYPLOT)THEN
              CALL PGSLS(2)
              CALL PGSCI(1)
              CALL PGLINE(2,XX,YY)
            END IF
            XX(1)=XMINF
            XX(2)=XMAXF
            IF(CWHICH.EQ.'a')THEN
              YY(1)=A(NPLOT)+B(NPLOT)*XX(1)
              YY(2)=A(NPLOT)+B(NPLOT)*XX(2)
            ELSEIF(CWHICH.EQ.'b')THEN
              YY(1)=AVEA_BOOT(NPLOT)+AVEB_BOOT(NPLOT)*XX(1)
              YY(2)=AVEA_BOOT(NPLOT)+AVEB_BOOT(NPLOT)*XX(2)
            ELSEIF(CWHICH.EQ.'c')THEN
              YY(1)=AVEA_JACK(NPLOT)+AVEB_JACK(NPLOT)*XX(1)
              YY(2)=AVEA_JACK(NPLOT)+AVEB_JACK(NPLOT)*XX(2)
            ELSEIF(CWHICH.EQ.'d')THEN
              YY(1)=AVEA_BRAN(NPLOT)+AVEB_BRAN(NPLOT)*XX(1)
              YY(2)=AVEA_BRAN(NPLOT)+AVEB_BRAN(NPLOT)*XX(2)
            ELSEIF(CWHICH.EQ.'e')THEN
              IF(NPLOT.EQ.1)THEN
                YY(1)=A_ERRY+B_ERRY*XX(1)
                YY(2)=A_ERRY+B_ERRY*XX(2)
              ELSEIF(NPLOT.EQ.4)THEN
                YY(1)=A_ERXY+B_ERXY*XX(1)
                YY(2)=A_ERXY+B_ERXY*XX(2)
              END IF
            ELSEIF(CWHICH.EQ.'f')THEN
              IF(NPLOT.EQ.1)THEN
                YY(1)=AVEA_EYSI+AVEB_EYSI*XX(1)
                YY(2)=AVEA_EYSI+AVEB_EYSI*XX(2)
              ELSEIF(NPLOT.EQ.4)THEN
                YY(1)=AVEA_EXYS+AVEB_EXYS*XX(1)
                YY(2)=AVEA_EXYS+AVEB_EXYS*XX(2)
              END IF
            END IF
            CALL PGSLS(1)
            CALL PGSCI(0)
            CALL PGLINE(2,XX,YY)
            LANYPLOT=.TRUE.
          END IF
        END DO
        CALL PGSLS(OLDLS)
        CALL PGSCI(OLDCI)
C------------------------------------------------------------------------------
C si se desea, podemos almacenar los ajustes en buffers
        NB=1
        DO WHILE(NB.NE.0)
          WRITE(*,100) 'Buffer # to store fit.......... (0=none)'
          NB=READILIM_B('0',0,NBUFFMAX)
          WRITE(77,111) NB,'# Buffer number to store fit'
          IF(NB.NE.0)THEN
            WRITE(*,100) 'No. of linear fit'
            NPLOT=READILIM_B('@',1,6)
            WRITE(77,111) NPLOT,'# No. of linear fit'
            IF((NPLOT.EQ.1).OR.(NPLOT.EQ.4))THEN
              WRITE(*,100) 'Which one (a/b/c/d/e/f) '
              CWHICH(1:1)=READC_B('a','abcdef')
            ELSE
              WRITE(*,100) 'Which one (a/b/c/d) '
              CWHICH(1:1)=READC_B('a','abcd')
            END IF
            WRITE(77,112) CWHICH,'# type of fit (a/b/c/d(e/f))'
            WRITE(*,100) 'Xmin '
            XMINF=READF_B(CXMINF)
            WRITE(77,*) XMINF,'# Xmin'
            WRITE(*,100) 'Xmax '
            XMAXF=READF_B(CXMAXF)
            WRITE(77,*) XMAXF,'# Xmax'
            WRITE(*,100) 'No. of points'
            NDATA=READILIM_B('1000',2,NDATAMAX)
            WRITE(77,111) NDATA,'# No. of points'
            DO I=1,NDATA
              X0=XMINF+(XMAXF-XMINF)*REAL(I-1)/REAL(NDATA-1)
              Y0=0 !avoid compilation warning
              IF(CWHICH.EQ.'a')THEN
                Y0=A(NPLOT)+B(NPLOT)*X0
              ELSEIF(CWHICH.EQ.'b')THEN
                Y0=AVEA_BOOT(NPLOT)+AVEB_BOOT(NPLOT)*X0
              ELSEIF(CWHICH.EQ.'c')THEN
                Y0=AVEA_JACK(NPLOT)+AVEB_JACK(NPLOT)*X0
              ELSEIF(CWHICH.EQ.'d')THEN
                Y0=AVEA_BRAN(NPLOT)+AVEB_BRAN(NPLOT)*X0
              ELSEIF(CWHICH.EQ.'e')THEN
                IF(NPLOT.EQ.1)THEN
                  Y0=A_ERRY+B_ERRY*X0
                ELSEIF(NPLOT.EQ.4)THEN
                  Y0=A_ERXY+B_ERXY*X0
                END IF
              ELSEIF(CWHICH.EQ.'f')THEN
                IF(NPLOT.EQ.1)THEN
                  Y0=AVEA_EYSI+AVEB_EYSI*X0
                ELSEIF(NPLOT.EQ.4)THEN
                  Y0=AVEA_EXYS+AVEB_EXYS*X0
                END IF
              ELSE
                WRITE(*,101) 'CWHICH='//CWHICH
                WRITE(*,101) 'FATAL ERROR in subroutine SLINREG: '//
     +           'invalid CWHICH option'
                STOP
              END IF
              XDATA(I,NB)=X0
              EXDATA(I,NB)=0.
              YDATA(I,NB)=Y0
              EYDATA(I,NB)=0.
            END DO
            NDATABUFF(NB)=NDATA
            LXERR(NB)=.FALSE.
            LYERR(NB)=.FALSE.
            CALL UPDATELIMITS(NB)
            NSYMBBUFF(NB)=1001 !linea continua
            LDEFBUFF(NB)=.TRUE.
            LUSEBUFF(NB)=.TRUE.
            WRITE(*,100) 'Key label '
            DATAKEY_(1:50)=READC_B('linear fit','@')
            DATAKEY(NB)=DATAKEY_
            L1=TRUEBEG(DATAKEY(NB))
            L2=TRUELEN(DATAKEY(NB))
            CALL TOLOG77_STRING(DATAKEY(NB)(L1:L2),'Key label')
          END IF
        END DO
C------------------------------------------------------------------------------
C si se desea podemos tambien almacenar los residuos en otro buffer
        NB=1
        DO WHILE(NB.NE.0)
          WRITE(*,100) 'Buffer # to store residuals.... (0=none)'
          NB=READILIM_B('0',0,NBUFFMAX)
          WRITE(77,111) NB,'# Buffer number to store residuals (0=none)'
          IF(NB.NE.0)THEN
            WRITE(*,100) 'No. of linear fit'
            NPLOT=READILIM_B('@',1,6)
            WRITE(77,111) NPLOT,'# No. of linear fit'
            IF((NPLOT.EQ.1).OR.(NPLOT.EQ.4))THEN
              WRITE(*,100) 'Which one (a/b/c/d/e/f) '
              CWHICH(1:1)=READC_B('a','abcdef')
            ELSE
              WRITE(*,100) 'Which one (a/b/c/d) '
              CWHICH(1:1)=READC_B('a','abcd')
            END IF
            WRITE(77,112) CWHICH,'# type of fit (a/b/c/d(e/f))'
            NDATABUFF(NB)=NDATABUFF(NB0)
            DO I=1,NDATABUFF(NB)
              X0=XDATA(I,NB0)
              Y0=0 !avoid compilation warning
              IF(CWHICH.EQ.'a')THEN
                Y0=A(NPLOT)+B(NPLOT)*X0
              ELSEIF(CWHICH.EQ.'b')THEN
                Y0=AVEA_BOOT(NPLOT)+AVEB_BOOT(NPLOT)*X0
              ELSEIF(CWHICH.EQ.'c')THEN
                Y0=AVEA_JACK(NPLOT)+AVEB_JACK(NPLOT)*X0
              ELSEIF(CWHICH.EQ.'d')THEN
                Y0=AVEA_BRAN(NPLOT)+AVEB_BRAN(NPLOT)*X0
              ELSEIF(CWHICH.EQ.'e')THEN
                IF(NPLOT.EQ.1)THEN
                  Y0=A_ERRY+B_ERRY*X0
                ELSEIF(NPLOT.EQ.4)THEN
                  Y0=A_ERXY+B_ERXY*X0
                END IF
              ELSEIF(CWHICH.EQ.'f')THEN
                IF(NPLOT.EQ.1)THEN
                  Y0=AVEA_EYSI+AVEB_EYSI*X0
                ELSEIF(NPLOT.EQ.4)THEN
                  Y0=AVEA_EXYS+AVEB_EXYS*X0
                END IF
              ELSE
                WRITE(*,101) 'CWHICH='//CWHICH
                WRITE(*,101) 'FATAL ERROR in subroutine SLINREG: '//
     +           'invalid CWHICH option'
                STOP
              END IF
              XDATA(I,NB)=X0
              EXDATA(I,NB)=0.
              YDATA(I,NB)=YDATA(I,NB0)-Y0
              EYDATA(I,NB)=0.
            END DO
            LXERR(NB)=.FALSE.
            LYERR(NB)=.FALSE.
            NSYMBBUFF(NB)=NSYMBBUFF(NB0)
            CALL UPDATELIMITS(NB)
            LDEFBUFF(NB)=.TRUE.
            LUSEBUFF(NB)=.TRUE.
            WRITE(*,100) 'Key label '
            DATAKEY_(1:50)=READC_B('residuals','@')
            DATAKEY(NB)=DATAKEY_
            L1=TRUEBEG(DATAKEY(NB))
            L2=TRUELEN(DATAKEY(NB))
            CALL TOLOG77_STRING(DATAKEY(NB)(L1:L2),'Key label')
          END IF
        END DO
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
112     FORMAT(11X,A1,1X,A)
        END
