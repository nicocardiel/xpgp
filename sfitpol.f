C
C******************************************************************************
C Prepara los datos para los ajustes polinomicos a los datos del buffer NB0
        SUBROUTINE SFITPOL(NB0)
        IMPLICIT NONE
        INTEGER NB0
C
        INCLUDE 'xpgpdir.inc'
        INCLUDE 'nsimul.inc'
C
        INTEGER NBUFFMAX                 !numero de buffers de datos diferentes
        PARAMETER (NBUFFMAX=8)
        INCLUDE 'ndatamax.inc'
        INTEGER NDEGMAX         !grado maximo del polinomio que puede ajustarse
        PARAMETER (NDEGMAX=16)
        INTEGER NPLOTMAX
        PARAMETER (NPLOTMAX=1000)  !numero maximo de puntos para dibujar ajuste
        DOUBLE PRECISION PI
        PARAMETER (  PI=3.141592653589793D0) !pi
        DOUBLE PRECISION SQR2
        PARAMETER (SQR2=1.414213562373095D0) !sqrt(2)
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
        INTEGER READILIM_B
        INTEGER SYSTEMFUNCTION
        REAL READF_B
        REAL FCHISQR
        REAL FFFISHER
        REAL FTSTUDENTI
        REAL RANRED
        REAL FMEAN0
        CHARACTER*255 READC_B
        CHARACTER*255 XPGPDIR_
C
        INTEGER I,J,K,KK,N,II,JJ
        INTEGER L1,L2,LL1,LL2
        INTEGER N1,N2
        INTEGER NF
        INTEGER NB,NBDEG(NDEGMAX+1)
        INTEGER NB1,NB2
        INTEGER NDATABUFF(NBUFFMAX),NDATA,NREMOVED
        INTEGER NDEG,NDEGUP,NCOEF,NCOEF_FORCED
        INTEGER IEXPAND
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        INTEGER NSIMUL,NSEED,IRAN
        INTEGER ICSAVE,ISYSTEM
        REAL XC,YC
        REAL XDATA(NDATAMAX,NBUFFMAX),YDATA(NDATAMAX,NBUFFMAX)
        REAL EXDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL YTEXTOLD
        REAL XMIN,XMAX,YMIN,YMAX
        REAL XMIN0,XMAX0,XMINF,XMAXF
        REAL XP(NPLOTMAX),YP(NPLOTMAX)
        REAL YP1(NPLOTMAX),YP2(NPLOTMAX)
        REAL RESI(NDATAMAX),RESI_(NDATAMAX),RESI_MEAN,RESI_STDV
        REAL F
        REAL FALPHA,FACTOR_TSTUDENT
        REAL KS_D,KS_PROB
        REAL KS_FUNC
        EXTERNAL KS_FUNC
        DOUBLE PRECISION XF(NDATAMAX),YF(NDATAMAX)
        DOUBLE PRECISION EXF(NDATAMAX),EYF(NDATAMAX)
        DOUBLE PRECISION A(NDEGMAX+1),VARA(NDEGMAX+1)
        DOUBLE PRECISION WEIGHT(NDATAMAX)
        DOUBLE PRECISION A_SIMU(NDEGMAX+1,NDATAMAX)
        DOUBLE PRECISION CHISQR,SR2,VAR_EXPECTED
        DOUBLE PRECISION X0,Y0,ERRY0,ERRX0,YY0
        DOUBLE PRECISION YMED,SUMERR,VAR_EXP,VAR_TOT,R2
        DOUBLE PRECISION RAN1,RAN2
        DOUBLE PRECISION COVAR(NDEGMAX+1,NDEGMAX+1)
        DOUBLE PRECISION XX1(NDEGMAX+1),XX2(NDEGMAX+1),XX3(NDEGMAX+1)
        CHARACTER*1 CH,CFIT,CSAVE
        CHARACTER*50 CXMINF,CXMAXF
        CHARACTER*50 CDUMMY
        CHARACTER*50 DATAKEY(NBUFFMAX)
        CHARACTER*255 FORTRAN_FILE
        CHARACTER*255 COMANDO
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LBATCH
        LOGICAL IFCOEF(NDEGMAX+1)
        LOGICAL LERR
        LOGICAL LANYPLOT
        LOGICAL LFITOK
        LOGICAL LOGFILE,LOK
C
        COMMON/BLKSETTINGS1/XMIN,XMAX,YMIN,YMAX
        COMMON/BLKSETTINGS7/IEXPAND
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKBUFFERS3/NSYMBBUFF,LWBUFF
        COMMON/BLKLXYERR/LXERR,LYERR
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKLBATCH/LBATCH
        COMMON/BLKSETTINGS8B/DATAKEY
C------------------------------------------------------------------------------
        DATA(NBDEG(K),K=1,NDEGMAX+1)/
     +   17,18,19,20,21,22,
     +   25,26,27,28,29,30,
     +   33,34,35,36,37/
C
        NSEED=-1
        NF=NDATABUFF(NB0)
C
        DO K=1,NDEGMAX+1
          IFCOEF(K)=.FALSE.
        END DO
C------------------------------------------------------------------------------
C mostramos botones con todos los grados disponibles
        DO K=1,NDEGMAX+1
          WRITE(CDUMMY,*) K-1
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(NBDEG(K),'degree '//CDUMMY(L1:L2),0)
        END DO
        CALL BUTTON(38,'go >>>',0)
        CALL BUTTON(38,'go >>>',3)
C seleccionamos grado inicial del polinomio
        WRITE(*,*)
        WRITE(*,100) 'Select polynomial degree...'
        NB=0
        DO WHILE(NB.EQ.0)
          IF(LBATCH)THEN
            READ(78,*) NB
          ELSE
            CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
            CALL IFBUTTON(XC,YC,NB)
          END IF
          WRITE(77,*) NB
          NDEG=-1   !grado ficticio para controlar pulsacion de boton con grado
          DO K=1,NDEGMAX+1
            IF(NB.EQ.NBDEG(K)) NDEG=K-1
          END DO
          IF(NDEG.EQ.-1) NB=0
        END DO
        WRITE(*,*)
C mostramos seleccion inicial de coeficiones para el grado elegido
        CALL BUTTQYTEXT(YTEXTOLD)
        CALL BUTTSYTEXT(0.45)
        DO K=1,NDEGMAX+1
          WRITE(CDUMMY,*) K-1
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(NBDEG(K),'a\\d'//CDUMMY(L1:L2),0)
          IF(NDEG.GE.K-1) CALL BUTTON(NBDEG(K),'a\\d'//CDUMMY(L1:L2),1)
        END DO
        CALL BUTTSYTEXT(YTEXTOLD)
        CALL BUTTON(38,'go >>>',0)
        CALL BUTTON(38,'go >>>',-3)
C
        DO K=1,NDEG+1
          IFCOEF(K)=.TRUE.
        END DO
C permitimos refinar el numero de coeficientes a ajustar
        WRITE(*,100) 'Select/unselect coefficients and click [go >>>] '
        WRITE(*,100) 'to proceed...'
        NB=0
        DO WHILE(NB.NE.38)
          IF(LBATCH)THEN
            READ(78,*) NB
          ELSE
            CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
            CALL IFBUTTON(XC,YC,NB)
          END IF
          WRITE(77,*) NB
          IF(NB.EQ.38)THEN
            NCOEF=0
            DO K=1,NDEGMAX+1
              IF(IFCOEF(K)) NCOEF=NCOEF+1
            END DO
            LERR=.FALSE.
            IF(NCOEF.EQ.0)THEN
              WRITE(*,*)
              WRITE(*,101) 'ERROR: number of coefficients = 0!'
              WRITE(*,100) 'Press <CR> to continue...'
              LERR=.TRUE.
            END IF
            IF(NCOEF.GT.NF)THEN
              WRITE(*,*)
              WRITE(*,100) 'ERROR: number of coefficients > '
              WRITE(*,101) 'number of points'
              WRITE(*,100) 'Press <CR> to continue...'
              LERR=.TRUE.
            END IF
            IF(LERR)THEN
              IF(LBATCH)THEN
                WRITE(*,*)
              ELSE
                READ(*,*)
              END IF
              DO K=1,NDEGMAX+1
                WRITE(CDUMMY,*) K-1
                L1=TRUEBEG(CDUMMY)
                L2=TRUELEN(CDUMMY)
                CALL BUTTON(NBDEG(K),'a\\d'//CDUMMY(L1:L2),-1)
              END DO
              CALL BUTTON(38,'go >>>',-1)
              RETURN
            END IF
          ELSE
            DO K=1,NDEGMAX+1
              IF(NB.EQ.NBDEG(K))THEN
                WRITE(CDUMMY,*) K-1
                L1=TRUEBEG(CDUMMY)
                L2=TRUELEN(CDUMMY)
                CALL BUTTQYTEXT(YTEXTOLD)
                CALL BUTTSYTEXT(0.45)
                IF(IFCOEF(K))THEN
                  IFCOEF(K)=.FALSE.
                  CALL BUTTON(NBDEG(K),'a\\d'//CDUMMY(L1:L2),0)
                ELSE
                  IFCOEF(K)=.TRUE.
                  CALL BUTTON(NBDEG(K),'a\\d'//CDUMMY(L1:L2),1)
                END IF
                CALL BUTTSYTEXT(YTEXTOLD)
              END IF
            END DO
          END IF
        END DO
        WRITE(*,*)
C------------------------------------------------------------------------------
C determinamos grado final del polinomio
        DO K=1,NDEGMAX+1
          IF(IFCOEF(K)) NDEGUP=K-1
        END DO
        IF(NDEGUP.GT.NDEG) NDEG=NDEGUP
        NCOEF_FORCED=NDEG+1-NCOEF !numero de coeficientes prefijados (forzados)
C------------------------------------------------------------------------------
C solicitamos el valor de los coeficientes no utilizados
        DO K=1,NDEG+1
          IF(.NOT.IFCOEF(K))THEN
            IF(K.LE.10)THEN
              WRITE(*,'(A3,I1,A2,$)') ' a(',K-1,') '
            ELSE
              WRITE(*,'(A2,I2,A2,$)') 'a(',K-1,') '
            END IF
            A(K)=READF_B('@')
            WRITE(77,*) A(K)
          END IF
        END DO
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C determinamos los limites para dibujar los ajustes
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
C------------------------------------------------------------------------------
C mostramos los coeficientes (tanto los calculados como los fijos)
        WRITE(*,*)
        WRITE(*,101) '<a> original data (no errors)'
        WRITE(*,101) '<b> bootstrap with original data (no errors)'
        WRITE(*,101) '<c> jackknife with original data (no errors)'
        WRITE(*,101) '<d> bootstrap+EY+EX with original data'
        WRITE(*,101) '<e> weighting EY with original data (Bevington)'
        WRITE(*,101) '<f> weighting EY with original data (modified)'
        WRITE(*,101) '<g> weighting EY + bootstrap with errors EY'
        WRITE(*,101) '<h> weighting EY + bootstrap with errors EX+EY'
        LANYPLOT=.FALSE.
        CFIT=' '
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        DO WHILE(CFIT.NE.'0')
          WRITE(*,100) 'Fit.....(a/b/c/d/e/f/g/h, 0=none, '//
     >     '?=show options) '
          CFIT=READC_B('0','abcdefgh0?')
          WRITE(77,101) CFIT
          WRITE(*,102)
C..............................................................................
          IF(CFIT.EQ.'?')THEN
            WRITE(*,*)
            WRITE(*,101) '<a> original data (no errors)'
            WRITE(*,101) '<b> bootstrap with original data (no errors)'
            WRITE(*,101) '<c> jackknife with original data (no errors)'
            WRITE(*,101) '<d> bootstrap+EY+EX with original data'
            WRITE(*,101) '<e> weighting EY with original data '//
     >       '(Bevington)'
            WRITE(*,101) '<f> weighting EY with original data '//
     >       '(modified)'
            WRITE(*,101) '<g> weighting EY + bootstrap with errors EY'
            WRITE(*,101) '<h> weighting EY + bootstrap with errors'//
     >       ' EX+EY'
C..............................................................................
C ajustamos el polinomio a los datos por el metodo ordinario OLS(Y|X) sin pesos
          ELSEIF(CFIT.EQ.'a')THEN
            DO I=1,NF
              XF(I)=DBLE(XDATA(I,NB0))
              YF(I)=DBLE(YDATA(I,NB0))
            END DO
            CALL FITPOL(NF,XF,YF,NDEG,IFCOEF,A,VARA,CHISQR,SR2,COVAR)
            CALL SHOW_FITPOL(A,VARA,IFCOEF,NDEGMAX,NDEG,NF)
            LFITOK=.TRUE.
C..............................................................................
C Bootstrap: resampling procedure to evaluate regression coefficients & errors
          ELSEIF(CFIT.EQ.'b')THEN
            DO NSIMUL=1,NSIMULMAX
              DO I=1,NF
                IRAN=INT(REAL(NF)*RANRED(NSEED))+1
                XF(I)=DBLE(XDATA(IRAN,NB0))
                YF(I)=DBLE(YDATA(IRAN,NB0))
              END DO
              CALL FITPOL(NF,XF,YF,NDEG,IFCOEF,A,VARA,CHISQR,SR2,COVAR)
              DO K=1,NDEG+1
                A_SIMU(K,NSIMUL)=A(K)
              END DO
            END DO
            DO K=1,NDEG+1
              A(K)=0.D0
              DO NSIMUL=1,NSIMULMAX
                A(K)=A(K)+A_SIMU(K,NSIMUL)
              END DO
              A(K)=A(K)/DBLE(NSIMULMAX)
              VARA(K)=0.D0
              DO NSIMUL=1,NSIMULMAX
                VARA(K)=VARA(K)+
     >           (A_SIMU(K,NSIMUL)-A(K))*
     >           (A_SIMU(K,NSIMUL)-A(K))
              END DO
              VARA(K)=VARA(K)/DBLE(NSIMULMAX-1)
            END DO
            CALL SHOW_FITPOL(A,VARA,IFCOEF,NDEGMAX,NDEG,NF)
C recalculamos CHISQR y SR2 para los coeficientes promedio
            SR2=0.D0
            DO I=1,NF
              X0=XF(I)
              Y0=A(NDEG+1)
              DO K=NDEG,1,-1
                Y0=Y0*X0+A(K)
              END DO
              SR2=SR2+(YF(I)-Y0)*(YF(I)-Y0)
            END DO
            CHISQR=SR2
            SR2=SR2/DBLE(NF-(NDEG+1))
            LFITOK=.TRUE.
C..............................................................................
C Jackknife: resampling procedure to evaluate regression coefficients & errors
          ELSEIF(CFIT.EQ.'c')THEN
            IF(NCOEF.GT.NF-1)THEN
              DO K=1,NDEG+1
                A(K)=0.D0
                VARA(K)=0.D0
              END DO
              LFITOK=.FALSE.
            ELSE
              DO NSIMUL=1,NF
                DO I=1,NF
                  IF(I.LT.NSIMUL)THEN
                    XF(I)=DBLE(XDATA(I,NB0))
                    YF(I)=DBLE(YDATA(I,NB0))
                  ELSEIF(I.GT.NSIMUL)THEN
                    XF(I-1)=DBLE(XDATA(I,NB0))
                    YF(I-1)=DBLE(YDATA(I,NB0))
                  END IF
                END DO
                CALL FITPOL(NF-1,XF,YF,NDEG,IFCOEF,A,VARA,CHISQR,SR2,
     >           COVAR)
                DO K=1,NDEG+1
                  A_SIMU(K,NSIMUL)=A(K)
                END DO
              END DO
              DO K=1,NDEG+1
                A(K)=0.D0
                DO NSIMUL=1,NF
                  A(K)=A(K)+A_SIMU(K,NSIMUL)
                END DO
                A(K)=A(K)/DBLE(NF)
                VARA(K)=0.D0
                DO NSIMUL=1,NF
                  VARA(K)=VARA(K)+
     >             (A_SIMU(K,NSIMUL)-A(K))*
     >             (A_SIMU(K,NSIMUL)-A(K))
                END DO
                VARA(K)=VARA(K)/DBLE(NF-1)
              END DO
              LFITOK=.TRUE.
            END IF
            CALL SHOW_FITPOL(A,VARA,IFCOEF,NDEGMAX,NDEG,NF)
C recalculamos CHISQR y SR2 para los coeficientes promedio
            IF(LFITOK)THEN
              SR2=0.D0
              DO I=1,NF
                X0=XF(I)
                Y0=A(NDEG+1)
                DO K=NDEG,1,-1
                  Y0=Y0*X0+A(K)
                END DO
                SR2=SR2+(YF(I)-Y0)*(YF(I)-Y0)
              END DO
              CHISQR=SR2
              SR2=SR2/DBLE(NF-(NDEG+1))
            END IF
C..............................................................................
C Bootstrap+errors: simulamos cada punto con su error en ambos ejes
          ELSEIF(CFIT.EQ.'d')THEN
            IF((.NOT.LXERR(NB0)).AND.(.NOT.LYERR(NB0)))THEN
              DO K=1,NDEG+1
                A(K)=0.D0
                VARA(K)=0.D0
              END DO
              LFITOK=.FALSE.
            ELSE
              DO NSIMUL=1,NSIMULMAX
                DO I=1,NF
                  XF(I)=DBLE(XDATA(I,NB0))
                  IF(LXERR(NB0))THEN
                    RAN1=DBLE(RANRED(NSEED))
                    RAN2=DBLE(RANRED(NSEED))
                    XF(I)=XF(I)+SQR2*DBLE(EXDATA(I,NB0))*
     >               DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
                  END IF
                  YF(I)=DBLE(YDATA(I,NB0))
                  IF(LYERR(NB0))THEN
                    RAN1=DBLE(RANRED(NSEED))
                    RAN2=DBLE(RANRED(NSEED))
                    YF(I)=YF(I)+SQR2*DBLE(EYDATA(I,NB0))*
     >               DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
                  END IF
                END DO
                CALL FITPOL(NF,XF,YF,NDEG,IFCOEF,A,VARA,CHISQR,SR2,
     >           COVAR)
                DO K=1,NDEG+1
                  A_SIMU(K,NSIMUL)=A(K)
                END DO
              END DO
              DO K=1,NDEG+1
                A(K)=0.D0
                DO NSIMUL=1,NSIMULMAX
                  A(K)=A(K)+A_SIMU(K,NSIMUL)
                END DO
                A(K)=A(K)/DBLE(NSIMULMAX)
                VARA(K)=0.D0
                DO NSIMUL=1,NSIMULMAX
                  VARA(K)=VARA(K)+
     >             (A_SIMU(K,NSIMUL)-A(K))*
     >             (A_SIMU(K,NSIMUL)-A(K))
                END DO
                VARA(K)=VARA(K)/DBLE(NSIMULMAX-1)
              END DO
              LFITOK=.TRUE.
            END IF
            CALL SHOW_FITPOL(A,VARA,IFCOEF,NDEGMAX,NDEG,NF)
C recalculamos CHISQR y SR2 para los coeficientes promedio
            IF(LFITOK)THEN
              SR2=0.D0
              DO I=1,NF
                X0=XF(I)
                Y0=A(NDEG+1)
                DO K=NDEG,1,-1
                  Y0=Y0*X0+A(K)
                END DO
                SR2=SR2+(YF(I)-Y0)*(YF(I)-Y0)
              END DO
              CHISQR=SR2
              SR2=SR2/DBLE(NF-(NDEG+1))
            END IF
C..............................................................................
C weighting EY
          ELSEIF(CFIT.EQ.'e')THEN
            IF(LYERR(NB0))THEN
              DO I=1,NF
                XF(I)=DBLE(XDATA(I,NB0))
                YF(I)=DBLE(YDATA(I,NB0))
                EYF(I)=DBLE(EYDATA(I,NB0))
              END DO
              CALL FITPOLEY(NF,XF,YF,EYF,NDEG,IFCOEF,A,VARA,CHISQR,SR2,
     >         COVAR)
              LFITOK=.TRUE.
            ELSE
              DO K=1,NDEG+1
                A(K)=0.D0
                VARA(K)=0.D0
              END DO
              LFITOK=.FALSE.
            END IF
            CALL SHOW_FITPOL(A,VARA,IFCOEF,NDEGMAX,NDEG,NF)
C..............................................................................
C weighting EY con formulas modificadas
          ELSEIF(CFIT.EQ.'f')THEN
            IF(LYERR(NB0))THEN
              DO I=1,NF
                XF(I)=DBLE(XDATA(I,NB0))
                YF(I)=DBLE(YDATA(I,NB0))
                EYF(I)=DBLE(EYDATA(I,NB0))
              END DO
              CALL FITPOLEYB(NF,XF,YF,EYF,NDEG,IFCOEF,A,VARA,CHISQR,SR2,
     >         COVAR)
              LFITOK=.TRUE.
            ELSE
              DO K=1,NDEG+1
                A(K)=0.D0
                VARA(K)=0.D0
              END DO
              LFITOK=.FALSE.
            END IF
            CALL SHOW_FITPOL(A,VARA,IFCOEF,NDEGMAX,NDEG,NF)
C..............................................................................
C weighting EY + bootstrap (con errores en Y solamente)
          ELSEIF(CFIT.EQ.'g')THEN
            IF(LYERR(NB0))THEN
              DO NSIMUL=1,NSIMULMAX
                DO I=1,NF
                  XF(I)=DBLE(XDATA(I,NB0))
                  YF(I)=DBLE(YDATA(I,NB0))
                  EYF(I)=DBLE(EYDATA(I,NB0))
                  RAN1=DBLE(RANRED(NSEED))
                  RAN2=DBLE(RANRED(NSEED))
                  YF(I)=YF(I)+SQR2*DBLE(EYF(I))*
     >             DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
                END DO
                CALL FITPOLEY(NF,XF,YF,EYF,NDEG,IFCOEF,A,VARA,
     >           CHISQR,SR2,COVAR)
                DO K=1,NDEG+1
                  A_SIMU(K,NSIMUL)=A(K)
                END DO
              END DO
              DO K=1,NDEG+1
                A(K)=0.D0
                DO NSIMUL=1,NSIMULMAX
                  A(K)=A(K)+A_SIMU(K,NSIMUL)
                END DO
                A(K)=A(K)/DBLE(NSIMULMAX)
                VARA(K)=0.D0
                DO NSIMUL=1,NSIMULMAX
                  VARA(K)=VARA(K)+
     >             (A_SIMU(K,NSIMUL)-A(K))*
     >             (A_SIMU(K,NSIMUL)-A(K))
                END DO
                VARA(K)=VARA(K)/DBLE(NSIMULMAX-1)
              END DO
              LFITOK=.TRUE.
            ELSE
              DO K=1,NDEG+1
                A(K)=0.D0
                VARA(K)=0.D0
              END DO
              LFITOK=.FALSE.
            END IF
            CALL SHOW_FITPOL(A,VARA,IFCOEF,NDEGMAX,NDEG,NF)
C recalculamos CHISQR y SR2 para los coeficientes promedio
            IF(LFITOK)THEN
              SR2=0.D0
              DO I=1,NF
                X0=XF(I)
                Y0=A(NDEG+1)
                DO K=NDEG,1,-1
                  Y0=Y0*X0+A(K)
                END DO
                SR2=SR2+(YF(I)-Y0)*(YF(I)-Y0)
              END DO
              CHISQR=SR2
              SR2=SR2/DBLE(NF-(NDEG+1))
            END IF
C..............................................................................
C weighting EY + bootstrap (con errores en ambos ejes)
          ELSEIF(CFIT.EQ.'h')THEN
            IF(LYERR(NB0).AND.LXERR(NB0))THEN
              DO NSIMUL=1,NSIMULMAX
                DO I=1,NF
                  XF(I)=DBLE(XDATA(I,NB0))
                  EXF(I)=DBLE(EXDATA(I,NB0))
                  RAN1=DBLE(RANRED(NSEED))
                  RAN2=DBLE(RANRED(NSEED))
                  XF(I)=XF(I)+SQR2*DBLE(EXF(I))*
     >             DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
                  YF(I)=DBLE(YDATA(I,NB0))
                  EYF(I)=DBLE(EYDATA(I,NB0))
                  RAN1=DBLE(RANRED(NSEED))
                  RAN2=DBLE(RANRED(NSEED))
                  YF(I)=YF(I)+SQR2*DBLE(EYF(I))*
     >             DSQRT(-DLOG(1.D0-RAN1))*DCOS(2.D0*PI*RAN2)
                END DO
                CALL FITPOLEY(NF,XF,YF,EYF,NDEG,IFCOEF,A,VARA,
     >           CHISQR,SR2,COVAR)
                DO K=1,NDEG+1
                  A_SIMU(K,NSIMUL)=A(K)
                END DO
              END DO
              DO K=1,NDEG+1
                A(K)=0.D0
                DO NSIMUL=1,NSIMULMAX
                  A(K)=A(K)+A_SIMU(K,NSIMUL)
                END DO
                A(K)=A(K)/DBLE(NSIMULMAX)
                VARA(K)=0.D0
                DO NSIMUL=1,NSIMULMAX
                  VARA(K)=VARA(K)+
     >             (A_SIMU(K,NSIMUL)-A(K))*
     >             (A_SIMU(K,NSIMUL)-A(K))
                END DO
                VARA(K)=VARA(K)/DBLE(NSIMULMAX-1)
              END DO
              LFITOK=.TRUE.
            ELSE
              DO K=1,NDEG+1
                A(K)=0.D0
                VARA(K)=0.D0
              END DO
              LFITOK=.FALSE.
            END IF
            CALL SHOW_FITPOL(A,VARA,IFCOEF,NDEGMAX,NDEG,NF)
C recalculamos CHISQR y SR2 para los coeficientes promedio
            IF(LFITOK)THEN
              SR2=0.D0
              DO I=1,NF
                X0=XF(I)
                Y0=A(NDEG+1)
                DO K=NDEG,1,-1
                  Y0=Y0*X0+A(K)
                END DO
                SR2=SR2+(YF(I)-Y0)*(YF(I)-Y0)
              END DO
              CHISQR=SR2
              SR2=SR2/DBLE(NF-(NDEG+1))
            END IF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          END IF
C determinamos, de haber errores en Y, el error tipico esperado
          IF((CFIT.NE.'0').AND.(CFIT.NE.'?').AND.LFITOK)THEN
            IF(INDEX('abcd',CFIT).NE.0)THEN                       !no hay pesos
              DO I=1,NF
                WEIGHT(I)=1.D0
              END DO
              VAR_EXPECTED=0.D0
            ELSE                                                     !hay pesos
              DO I=1,NF
                WEIGHT(I)=DBLE(EYDATA(I,NB0))
              END DO
              VAR_EXPECTED=0.D0
              DO I=1,NF
                VAR_EXPECTED=VAR_EXPECTED+1.D0/(WEIGHT(I)*WEIGHT(I))
              END DO
              VAR_EXPECTED=DBLE(NF)/VAR_EXPECTED
            END IF
C calculamos coeficiente de determinacion
            YMED=0.D0
            SUMERR=0.D0
            DO I=1,NF
              YMED=YMED+YF(I)/(WEIGHT(I)*WEIGHT(I))
              SUMERR=SUMERR+1.D0/(WEIGHT(I)*WEIGHT(I))
            END DO
            YMED=YMED/SUMERR
            VAR_EXP=0.D0
            VAR_TOT=0.D0
            DO I=1,NF
              X0=XF(I)
              Y0=A(NDEG+1)
              DO K=NDEG,1,-1
                Y0=Y0*X0+A(K)
              END DO
              VAR_EXP=VAR_EXP+
     >         (Y0-YMED)*(Y0-YMED)/(WEIGHT(I)*WEIGHT(I))
              VAR_TOT=VAR_TOT+
     >         (YF(I)-YMED)*(YF(I)-YMED)/(WEIGHT(I)*WEIGHT(I))
            END DO
            R2=VAR_EXP/VAR_TOT
C mostramos estadistica adicional sobre el ajuste representado
            WRITE(*,102)
            WRITE(*,100) 'Number of points used in fit..: '
            WRITE(*,*) NF
            WRITE(*,100) 'Degrees of freedom............: '
            WRITE(*,*) NF-(NDEG+1)+NCOEF_FORCED
            WRITE(*,100) 'Determination coefficient r**2: '
            WRITE(*,*) R2
            IF(VAR_EXPECTED.GT.0.D0)THEN
              WRITE(*,101) '* Goodness-of-fit probability Q:'
              WRITE(*,100) 'Chi-square, Q.................: '
              WRITE(*,*) CHISQR,FCHISQR(NF-(NDEG+1),REAL(CHISQR))
              WRITE(*,101) '(Q is the prob. that a value of '//
     >         'chi-square "as poor" should occur by chance)'
            END IF
            WRITE(*,102)
            WRITE(*,100) 'Residual standard deviation...: '
            WRITE(*,*) DSQRT(SR2)
            WRITE(*,100) 'Expected standard deviation...: '
            WRITE(*,*) DSQRT(VAR_EXPECTED)
            IF(VAR_EXPECTED.GT.0.D0)THEN
              F=REAL(SR2/VAR_EXPECTED)
              WRITE(*,101) '* H0: residual variance .le. '//
     >         'expected variance:'
              WRITE(*,100) 'F, alpha......................: '
              N1=NF-(NDEG+1)+NCOEF_FORCED
              N2=NF
              WRITE(*,*) F,FFFISHER(N1-1,N2-1,F)
            END IF
            WRITE(*,102)
          END IF
C..............................................................................
C dibujamos el ajuste
          IF((CFIT.NE.'0').AND.(CFIT.NE.'?').AND.LFITOK)THEN
            IF(LANYPLOT)THEN
              CALL PGSLS(2)
              CALL PGSCI(1)
              CALL PGLINE(NPLOTMAX,XP,YP)
            END IF
            DO I=1,NPLOTMAX
              X0=DBLE(XMINF+(XMAXF-XMINF)*REAL(I-1)/REAL(NPLOTMAX-1))
              Y0=A(NDEG+1)
              DO K=NDEG,1,-1
                Y0=Y0*X0+A(K)
              END DO
              XP(I)=REAL(X0)
              YP(I)=REAL(Y0)
            END DO
            CALL PGSLS(1)
            CALL PGSCI(0)
            CALL PGLINE(NPLOTMAX,XP,YP)
            LANYPLOT=.TRUE.
          END IF
C..............................................................................
C Un test Kolmogorov-Smirnov: para evitar el problema de que no podemos
C determinar parametros de la distribucion que queremos testear con la misma
C muestra, ordenamos los datos, calculamos la media y desviacion tipica con
C la mitad de la muestra, y realizamos el test KS con la otra mitad.
          IF((CFIT.NE.'0').AND.(CFIT.NE.'?').AND.LFITOK)THEN
            DO I=1,NDATABUFF(NB0)
              X0=DBLE(XDATA(I,NB0))
              Y0=A(NDEG+1)
              DO K=NDEG,1,-1
                Y0=Y0*X0+A(K)
              END DO
              RESI(I)=YDATA(I,NB0)-REAL(Y0)
            END DO
            CALL ORDENA1F(NDATABUFF(NB0),RESI)
            IF(NDATABUFF(NB0).GE.6)THEN
              K=0
              DO I=1,NDATABUFF(NB0),2   !calculamos media y std con los impares
                K=K+1
                RESI_(K)=RESI(I)
              END DO
              RESI_MEAN=FMEAN0(K,RESI_,RESI_STDV)
              WRITE(*,100) 'Residuals mean and stdv. (half sample): '
              WRITE(*,*) RESI_MEAN,RESI_STDV
              K=0
              DO I=2,NDATABUFF(NB0),2 !pasamos residuos a distri. normal tipif.
                K=K+1
                RESI_(K)=(RESI(I)-RESI_MEAN)/RESI_STDV
              END DO
              CALL KSONE(RESI_,K,KS_FUNC,KS_D,KS_PROB)
              WRITE(*,101) '* H0: residuals follow '//
     >         'a normal distribution:'
              WRITE(*,100) 'Kolmogorov-Smirnov test, D, PROB '//
     >         '(half sample): '
              WRITE(*,*) KS_D,KS_PROB
              WRITE(*,101) '(small values of PROB show that the '//
     >         'cumulative distribution function of'
              WRITE(*,101) 'residuals is significantly different from'//
     >         ' the normal distribution)'
              WRITE(*,102)
            END IF
          END IF
C..............................................................................
C dibujamos limites correspondientes a un nivel de confianza dado para la
C prediccion del ajuste
          IF((CFIT.NE.'0').AND.(CFIT.NE.'?').AND.LFITOK)THEN
            FALPHA=-1.
            DO WHILE((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))
              WRITE(*,100) '* NOTE: alfa(1sigma)=0.1587'
              WRITE(*,100) ', alfa(2sigma)=0.0228'
              WRITE(*,101) ', alfa(3sigma)=0.00135'
              WRITE(*,100) 'Error on prediction........: '
              WRITE(*,100) 'significance level alpha/2 (0=none) '
              FALPHA=READF_B('0.0')
              IF((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))THEN
                WRITE(*,101) 'ERROR: alpha/2 must be in [0.0,0.5]'
                WRITE(*,100) 'Press <CR> to continue...'
                IF(LBATCH)THEN
                  WRITE(*,*)
                ELSE
                  READ(*,*)
                END IF
              END IF
            END DO
            WRITE(77,*) FALPHA
            IF(FALPHA.GT.0.0)THEN
              FACTOR_TSTUDENT=FTSTUDENTI(NF-NCOEF,FALPHA)
              WRITE(CDUMMY,*) NF-NCOEF
              L1=TRUEBEG(CDUMMY)
              L2=TRUELEN(CDUMMY)
              WRITE(*,100) 'T(alpha/2) for '
              WRITE(*,100) CDUMMY(L1:L2)
              WRITE(*,100) ' degrees of freedom: '
              WRITE(*,*) FACTOR_TSTUDENT
              WRITE(*,100) 'X error to be added quadratically '
              ERRX0=READF_B('0.0')
              WRITE(77,*) ERRX0
              DO N=1,NPLOTMAX
                X0=DBLE(XMINF+(XMAXF-XMINF)*REAL(N-1)/REAL(NPLOTMAX-1))
                Y0=A(NDEG+1)                                   !polinomio en X0
                IF(NDEG.GT.0)THEN
                  DO K=NDEG,1,-1
                    Y0=Y0*X0+A(K)
                  END DO
                END IF
                YY0=DBLE(NDEG)*A(NDEG+1)                !derivada primera en X0
                IF(NDEG.GT.1)THEN
                  DO K=NDEG,2,-1
                    YY0=YY0*X0+DBLE(K-1)*A(K)
                  END DO
                END IF
                KK=0
                DO K=1,NDEG+1
                  IF(IFCOEF(K))THEN
                    KK=KK+1
                    IF(K.EQ.1)THEN
                      XX1(KK)=1.D0
                      XX2(KK)=1.D0
                    ELSE
                      XX1(KK)=X0**K
                      XX2(KK)=X0**K
                    END IF
                  END IF
                END DO
                DO I=1,NCOEF
                  XX3(I)=0.D0
                  DO J=1,NCOEF
                    XX3(I)=XX3(I)+XX1(J)*COVAR(J,I)
                  END DO
                END DO
                ERRY0=0.D0
                DO I=1,NCOEF
                  ERRY0=ERRY0+XX3(I)*XX2(I)
                END DO
                ERRY0=DSQRT(ERRY0*SR2+YY0*YY0*ERRX0*ERRX0)
                ERRY0=ERRY0*FACTOR_TSTUDENT
                XP(N)=X0
                YP1(N)=Y0-ERRY0
                YP2(N)=Y0+ERRY0
              END DO
              CALL PGLINE(NPLOTMAX,XP,YP1)
              CALL PGLINE(NPLOTMAX,XP,YP2)
            END IF
          END IF
C..............................................................................
C dibujamos limites correspondientes a un nivel de confianza dado para un
C futuro dato medido
          IF((CFIT.NE.'0').AND.(CFIT.NE.'?').AND.LFITOK)THEN
            FALPHA=-1.
            DO WHILE((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))
              WRITE(*,100) 'Error on future measurement: '
              WRITE(*,100) 'significance level alpha/2 (0=none) '
              FALPHA=READF_B('0.0')
              IF((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))THEN
                WRITE(*,101) 'ERROR: alpha/2 must be in [0.0,0.5]'
                WRITE(*,100) 'Press <CR> to continue...'
                IF(LBATCH)THEN
                  WRITE(*,*)
                ELSE
                  READ(*,*)
                END IF
              END IF
            END DO
            WRITE(77,*) FALPHA
            IF(FALPHA.GT.0.0)THEN
              FACTOR_TSTUDENT=FTSTUDENTI(NF-NCOEF,FALPHA)
              WRITE(CDUMMY,*) NF-NCOEF
              L1=TRUEBEG(CDUMMY)
              L2=TRUELEN(CDUMMY)
              WRITE(*,100) 'T(alpha/2) for '
              WRITE(*,100) CDUMMY(L1:L2)
              WRITE(*,100) ' degrees of freedom: '
              WRITE(*,*) FACTOR_TSTUDENT
              WRITE(*,100) 'X error to be added quadratically '
              ERRX0=READF_B('0.0')
              WRITE(77,*) ERRX0
              DO N=1,NPLOTMAX
                X0=DBLE(XMINF+(XMAXF-XMINF)*REAL(N-1)/REAL(NPLOTMAX-1))
                Y0=A(NDEG+1)                                   !polinomio en X0
                IF(NDEG.GT.0)THEN
                  DO K=NDEG,1,-1
                    Y0=Y0*X0+A(K)
                  END DO
                END IF
                YY0=DBLE(NDEG)*A(NDEG+1)                !derivada primera en X0
                IF(NDEG.GT.1)THEN
                  DO K=NDEG,2,-1
                    YY0=YY0*X0+DBLE(K-1)*A(K)
                  END DO
                END IF
                KK=0
                DO K=1,NDEG+1
                  IF(IFCOEF(K))THEN
                    KK=KK+1
                    IF(K.EQ.1)THEN
                      XX1(KK)=1.D0
                      XX2(KK)=1.D0
                    ELSE
                      XX1(KK)=X0**K
                      XX2(KK)=X0**K
                    END IF
                  END IF
                END DO
                DO I=1,NCOEF
                  XX3(I)=0.D0
                  DO J=1,NCOEF
                    XX3(I)=XX3(I)+XX1(J)*COVAR(J,I)
                  END DO
                END DO
                ERRY0=0.D0
                DO I=1,NCOEF
                  ERRY0=ERRY0+XX3(I)*XX2(I)
                END DO
                ERRY0=DSQRT((1.D0+ERRY0)*SR2+YY0*YY0*ERRX0*ERRX0)
                ERRY0=ERRY0*FACTOR_TSTUDENT
                XP(N)=X0
                YP1(N)=Y0-ERRY0
                YP2(N)=Y0+ERRY0
              END DO
              CALL PGLINE(NPLOTMAX,XP,YP1)
              CALL PGLINE(NPLOTMAX,XP,YP2)
            END IF
          END IF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        END DO
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C Si hemos hecho algun ajuste, podemos salvar esta informacion en otros
C buffers
        IF(LANYPLOT)THEN
          WRITE(*,100) 'Are you saving fit results (y/n) '
          CSAVE=READC_B('n','yn')
          WRITE(77,101) CSAVE
        ELSE
          CSAVE='n'
        END IF
C
        IF(CSAVE.EQ.'y')THEN
          ICSAVE=1
          DO WHILE(ICSAVE.NE.0)
            WRITE(*,101) '(1) Save last fit into buffer'
            WRITE(*,101) '(2) Save fit predictions into buffer'
            WRITE(*,101) '(3) Save fit residuals into buffer'
            WRITE(*,101) '(4) Save confidence level for predictions'
            WRITE(*,101) '(5) Save confidence level for new observation'
            WRITE(*,100) '(6) Generate a FORTRAN program to compute '
            WRITE(*,101) 'polynomial and errors'
            WRITE(*,100) '(7) Remove buffer data beyond a fixed '
            WRITE(*,101) 'confidence level for new observation'
            WRITE(*,101) '(0) EXIT'
            WRITE(*,100) 'Option '
            ICSAVE=READILIM_B('0',0,7)
            WRITE(77,*) ICSAVE
C
            IF((ICSAVE.EQ.1).OR.(ICSAVE.EQ.2).OR.(ICSAVE.EQ.3).OR.
     +       (ICSAVE.EQ.7))THEN
              WRITE(*,100) 'Buffer # to store new data.............'
              NB=READILIM_B('@',1,NBUFFMAX)
              WRITE(77,*) NB
            ELSEIF((ICSAVE.EQ.4).OR.(ICSAVE.EQ.5))THEN
              WRITE(*,100) '1st Buffer # to store lower limit......'
              NB1=READILIM_B('@',1,NBUFFMAX)
              WRITE(77,*) NB1
              WRITE(*,100) '2nd Buffer # to store upper limit......'
              NB2=READILIM_B('@',1,NBUFFMAX)
              WRITE(77,*) NB2
            END IF
C
            IF((ICSAVE.EQ.1).OR.(ICSAVE.EQ.4).OR.(ICSAVE.EQ.5))THEN
              WRITE(*,100) 'Xmin '
              XMINF=READF_B(CXMINF)
              WRITE(77,*) XMINF
              WRITE(*,100) 'Xmax '
              XMAXF=READF_B(CXMAXF)
              WRITE(77,*) XMAXF
              WRITE(*,100) 'No. of points '
              NDATA=READILIM_B('1000',2,NDATAMAX)
              WRITE(77,*) NDATA
            END IF
C..............................................................................
            IF(ICSAVE.EQ.1)THEN                                       !last fit
              DO I=1,NDATA
                X0=DBLE(XMINF+(XMAXF-XMINF)*REAL(I-1)/REAL(NDATA-1))
                Y0=A(NDEG+1)
                DO K=NDEG,1,-1
                  Y0=Y0*X0+A(K)
                END DO
                XDATA(I,NB)=REAL(X0)
                EXDATA(I,NB)=0.
                YDATA(I,NB)=REAL(Y0)
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
              DATAKEY(NB)=READC_B('polynomial fit','@')
              L1=TRUEBEG(DATAKEY(NB))
              L2=TRUELEN(DATAKEY(NB))
              WRITE(77,101) DATAKEY(NB)(L1:L2)
              CALL SHOW_BUFFERS
C..............................................................................
            ELSEIF(ICSAVE.EQ.2)THEN                                !predictions
              NDATABUFF(NB)=NDATABUFF(NB0)
              DO I=1,NDATABUFF(NB)
                X0=DBLE(XDATA(I,NB0))
                Y0=A(NDEG+1)
                DO K=NDEG,1,-1
                  Y0=Y0*X0+A(K)
                END DO
                XDATA(I,NB)=REAL(X0)
                EXDATA(I,NB)=0.
                YDATA(I,NB)=REAL(Y0)
                EYDATA(I,NB)=0.
              END DO
              LXERR(NB)=.FALSE.
              LYERR(NB)=.FALSE.
              NSYMBBUFF(NB)=NSYMBBUFF(NB0)
              CALL UPDATELIMITS(NB)
              LDEFBUFF(NB)=.TRUE.
              LUSEBUFF(NB)=.TRUE.
              WRITE(*,100) 'Key label '
              DATAKEY(NB)=READC_B('polynomial predictions','@')
              L1=TRUEBEG(DATAKEY(NB))
              L2=TRUELEN(DATAKEY(NB))
              WRITE(77,101) DATAKEY(NB)(L1:L2)
              CALL SHOW_BUFFERS
C..............................................................................
            ELSEIF(ICSAVE.EQ.3)THEN                                  !residuals
              NDATABUFF(NB)=NDATABUFF(NB0)
              DO I=1,NDATABUFF(NB)
                X0=DBLE(XDATA(I,NB0))
                Y0=A(NDEG+1)
                DO K=NDEG,1,-1
                  Y0=Y0*X0+A(K)
                END DO
                XDATA(I,NB)=REAL(X0)
                EXDATA(I,NB)=0.
                YDATA(I,NB)=YDATA(I,NB0)-REAL(Y0)
                EYDATA(I,NB)=0.
              END DO
              LXERR(NB)=.FALSE.
              LYERR(NB)=.FALSE.
              NSYMBBUFF(NB)=NSYMBBUFF(NB0)
              CALL UPDATELIMITS(NB)
              LDEFBUFF(NB)=.TRUE.
              LUSEBUFF(NB)=.TRUE.
              WRITE(*,100) 'Key label '
              DATAKEY(NB)=READC_B('residuals','@')
              L1=TRUEBEG(DATAKEY(NB))
              L2=TRUELEN(DATAKEY(NB))
              WRITE(77,101) DATAKEY(NB)(L1:L2)
              CALL SHOW_BUFFERS
C..............................................................................
            ELSEIF(ICSAVE.EQ.4)THEN              !confid. level for predictions
              FALPHA=-1.
              DO WHILE((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))
                WRITE(*,100) '* NOTE: alfa(1sigma)=0.1587'
                WRITE(*,100) ', alfa(2sigma)=0.0228'
                WRITE(*,101) ', alfa(3sigma)=0.00135'
                WRITE(*,100) 'Significance level (alpha/2) '
                FALPHA=READF_B('0.1587')
                IF((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))THEN
                  WRITE(*,101) 'ERROR: alpha/2 must be in [0.0,0.5]'
                  WRITE(*,100) 'Press <CR> to continue...'
                  IF(LBATCH)THEN
                    WRITE(*,*)
                  ELSE
                    READ(*,*)
                  END IF
                END IF
              END DO
              WRITE(77,*) FALPHA
              FACTOR_TSTUDENT=FTSTUDENTI(NF-NCOEF,FALPHA)
              WRITE(*,100) 'X error to be added quadratically '
              ERRX0=READF_B('0.0')
              WRITE(77,*) ERRX0
              DO N=1,NDATA
                X0=DBLE(XMINF+(XMAXF-XMINF)*REAL(N-1)/REAL(NDATA-1))
                Y0=A(NDEG+1)                                   !polinomio en X0
                IF(NDEG.GT.0)THEN
                  DO K=NDEG,1,-1
                    Y0=Y0*X0+A(K)
                  END DO
                END IF
                YY0=DBLE(NDEG)*A(NDEG+1)                !derivada primera en X0
                IF(NDEG.GT.1)THEN
                  DO K=NDEG,2,-1
                    YY0=YY0*X0+DBLE(K-1)*A(K)
                  END DO
                END IF
                KK=0
                DO K=1,NDEG+1
                  IF(IFCOEF(K))THEN
                    KK=KK+1
                    IF(K.EQ.1)THEN
                      XX1(KK)=1.D0
                      XX2(KK)=1.D0
                    ELSE
                      XX1(KK)=X0**K
                      XX2(KK)=X0**K
                    END IF
                  END IF
                END DO
                DO I=1,NCOEF
                  XX3(I)=0.D0
                  DO J=1,NCOEF
                    XX3(I)=XX3(I)+XX1(J)*COVAR(J,I)
                  END DO
                END DO
                ERRY0=0.D0
                DO I=1,NCOEF
                  ERRY0=ERRY0+XX3(I)*XX2(I)
                END DO
                ERRY0=DSQRT(ERRY0*SR2+YY0*YY0*ERRX0*ERRX0)
                ERRY0=ERRY0*FACTOR_TSTUDENT
                XP(N)=X0
                YP1(N)=Y0-ERRY0
                YP2(N)=Y0+ERRY0
              END DO
              CALL PGLINE(NPLOTMAX,XP,YP1)
              CALL PGLINE(NPLOTMAX,XP,YP2)
              DO N=1,NDATA
                XDATA(N,NB1)=XP(N)
                EXDATA(N,NB1)=0.
                YDATA(N,NB1)=YP1(N)
                EYDATA(N,NB1)=0.
                XDATA(N,NB2)=XP(N)
                EXDATA(N,NB2)=0.
                YDATA(N,NB2)=YP2(N)
                EYDATA(N,NB2)=0.
              END DO
              NDATABUFF(NB1)=NDATA
              NDATABUFF(NB2)=NDATA
              LXERR(NB1)=.FALSE.
              LYERR(NB1)=.FALSE.
              LXERR(NB2)=.FALSE.
              LYERR(NB2)=.FALSE.
              CALL UPDATELIMITS(NB1)
              CALL UPDATELIMITS(NB2)
              NSYMBBUFF(NB1)=1001 !linea continua
              NSYMBBUFF(NB2)=1001 !linea continua
              LDEFBUFF(NB1)=.TRUE.
              LUSEBUFF(NB1)=.TRUE.
              LDEFBUFF(NB2)=.TRUE.
              LUSEBUFF(NB2)=.TRUE.
              WRITE(*,100) 'Key label for  first buffer '
              DATAKEY(NB1)=READC_B('c.l. for prediction','@')
              L1=TRUEBEG(DATAKEY(NB1))
              L2=TRUELEN(DATAKEY(NB1))
              WRITE(77,101) DATAKEY(NB1)(L1:L2)
              WRITE(*,100) 'Key label for second buffer '
              DATAKEY(NB2)=READC_B('c.l. for prediction','@')
              L1=TRUEBEG(DATAKEY(NB2))
              L2=TRUELEN(DATAKEY(NB2))
              WRITE(77,101) DATAKEY(NB2)(L1:L2)
              CALL SHOW_BUFFERS
C..............................................................................
            ELSEIF(ICSAVE.EQ.5)THEN          !confid. level for new observation
              FALPHA=-1.
              DO WHILE((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))
                WRITE(*,100) '* NOTE: alfa(1sigma)=0.1587'
                WRITE(*,100) ', alfa(2sigma)=0.0228'
                WRITE(*,101) ', alfa(3sigma)=0.00135'
                WRITE(*,100) 'Significance level (alpha/2) '
                FALPHA=READF_B('0.1587')
                IF((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))THEN
                  WRITE(*,101) 'ERROR: alpha/2 must be in [0.0,0.5]'
                  WRITE(*,100) 'Press <CR> to continue...'
                  IF(LBATCH)THEN
                    WRITE(*,*)
                  ELSE
                    READ(*,*)
                  END IF
                END IF
              END DO
              WRITE(77,*) FALPHA
              FACTOR_TSTUDENT=FTSTUDENTI(NF-NCOEF,FALPHA)
              WRITE(*,100) 'X error to be added quadratically '
              ERRX0=READF_B('0.0')
              WRITE(77,*) ERRX0
              DO N=1,NDATA
                X0=DBLE(XMINF+(XMAXF-XMINF)*REAL(N-1)/REAL(NDATA-1))
                Y0=A(NDEG+1)                                   !polinomio en X0
                IF(NDEG.GT.0)THEN
                  DO K=NDEG,1,-1
                    Y0=Y0*X0+A(K)
                  END DO
                END IF
                YY0=DBLE(NDEG)*A(NDEG+1)                !derivada primera en X0
                IF(NDEG.GT.1)THEN
                  DO K=NDEG,2,-1
                    YY0=YY0*X0+DBLE(K-1)*A(K)
                  END DO
                END IF
                KK=0
                DO K=1,NDEG+1
                  IF(IFCOEF(K))THEN
                    KK=KK+1
                    IF(K.EQ.1)THEN
                      XX1(KK)=1.D0
                      XX2(KK)=1.D0
                    ELSE
                      XX1(KK)=X0**K
                      XX2(KK)=X0**K
                    END IF
                  END IF
                END DO
                DO I=1,NCOEF
                  XX3(I)=0.D0
                  DO J=1,NCOEF
                    XX3(I)=XX3(I)+XX1(J)*COVAR(J,I)
                  END DO
                END DO
                ERRY0=0.D0
                DO I=1,NCOEF
                  ERRY0=ERRY0+XX3(I)*XX2(I)
                END DO
                ERRY0=DSQRT((1.D0+ERRY0)*SR2+YY0*YY0*ERRX0*ERRX0)
                ERRY0=ERRY0*FACTOR_TSTUDENT
                XP(N)=X0
                YP1(N)=Y0-ERRY0
                YP2(N)=Y0+ERRY0
              END DO
              CALL PGLINE(NPLOTMAX,XP,YP1)
              CALL PGLINE(NPLOTMAX,XP,YP2)
              DO N=1,NDATA
                XDATA(N,NB1)=XP(N)
                EXDATA(N,NB1)=0.
                YDATA(N,NB1)=YP1(N)
                EYDATA(N,NB1)=0.
                XDATA(N,NB2)=XP(N)
                EXDATA(N,NB2)=0.
                YDATA(N,NB2)=YP2(N)
                EYDATA(N,NB2)=0.
              END DO
              NDATABUFF(NB1)=NDATA
              NDATABUFF(NB2)=NDATA
              LXERR(NB1)=.FALSE.
              LYERR(NB1)=.FALSE.
              LXERR(NB2)=.FALSE.
              LYERR(NB2)=.FALSE.
              CALL UPDATELIMITS(NB1)
              CALL UPDATELIMITS(NB2)
              NSYMBBUFF(NB1)=1001 !linea continua
              NSYMBBUFF(NB2)=1001 !linea continua
              LDEFBUFF(NB1)=.TRUE.
              LUSEBUFF(NB1)=.TRUE.
              LDEFBUFF(NB2)=.TRUE.
              LUSEBUFF(NB2)=.TRUE.
              WRITE(*,100) 'Key label for  first buffer '
              DATAKEY(NB1)=READC_B('c.l. for new data','@')
              L1=TRUEBEG(DATAKEY(NB1))
              L2=TRUELEN(DATAKEY(NB1))
              WRITE(77,101) DATAKEY(NB1)(L1:L2)
              WRITE(*,100) 'Key label for second buffer '
              DATAKEY(NB2)=READC_B('c.l. for new data','@')
              L1=TRUEBEG(DATAKEY(NB2))
              L2=TRUELEN(DATAKEY(NB2))
              WRITE(77,101) DATAKEY(NB2)(L1:L2)
              CALL SHOW_BUFFERS
C..............................................................................
            ELSEIF(ICSAVE.EQ.6)THEN                 !generate a FORTRAN program
C pedimos nombre de fichero FORTRAN; si ya existe, lo borramos
              WRITE(*,100) 'Output FORTRAN file name'
              FORTRAN_FILE=READC_B('@','@')
              L1=TRUEBEG(FORTRAN_FILE)
              L2=TRUELEN(FORTRAN_FILE)
              WRITE(77,101) FORTRAN_FILE(L1:L2)
              INQUIRE(FILE=FORTRAN_FILE,EXIST=LOGFILE)
              IF(LOGFILE)THEN
                COMANDO='rm -f '
                COMANDO(7:)=FORTRAN_FILE(L1:L2)
                ISYSTEM=SYSTEMFUNCTION(COMANDO)
                IF((ISYSTEM.EQ.127).OR.(ISYSTEM.EQ.-1))THEN
                  WRITE(*,101) 'ERROR: while calling system function.'
                  WRITE(*,100) 'Press <CR> to continue...'
                  IF(LBATCH)THEN
                    WRITE(*,*)
                  ELSE
                    READ(*,*)
                  END IF
                  LOK=.FALSE.
                ELSE
                  LOK=.TRUE.
                END IF
              ELSE
                LOK=.TRUE.
              END IF
c si existe algun fichero compute_polynomial.0, lo borramos
              IF(LOK)THEN
                INQUIRE(FILE='./compute_polynomial.0',EXIST=LOGFILE)
                IF(LOGFILE)THEN
                  ISYSTEM=SYSTEMFUNCTION('rm -f compute_polynomial.0')
                  IF((ISYSTEM.EQ.127).OR.(ISYSTEM.EQ.-1))THEN
                    WRITE(*,101) 'ERROR: while calling system function.'
                    WRITE(*,100) 'Press <CR> to continue...'
                    IF(LBATCH)THEN
                      WRITE(*,*)
                    ELSE
                      READ(*,*)
                    END IF
                    LOK=.FALSE.
                  ELSE
                    LOK=.TRUE.
                  END IF
                ELSE
                  LOK=.TRUE.
                END IF
              END IF
C generamos fichero  compute_polynomial.0
              IF(LOK)THEN
                OPEN(10,FILE='compute_polynomial.0',STATUS='NEW',
     >           FORM='FORMATTED')
                L1=TRUEBEG(FORTRAN_FILE)
                L2=TRUELEN(FORTRAN_FILE)
                WRITE(10,100) '        PROGRAM '
                WRITE(10,101) FORTRAN_FILE(L1:L2-2)
                WRITE(10,101) '        IMPLICIT NONE'
                WRITE(10,101) 'C'
                WRITE(10,101) '        INTEGER NDEG_FREEDOM'
                WRITE(10,100) '        PARAMETER (NDEG_FREEDOM='
                WRITE(CDUMMY,*) NF-NCOEF
                L1=TRUEBEG(CDUMMY)
                L2=TRUELEN(CDUMMY)
                WRITE(10,100) CDUMMY(L1:L2)
                WRITE(10,101) ')'
                WRITE(10,101) 'C'
                CLOSE(10)
                WRITE(*,100) 'INFO: file compute_polynomial.0'
                WRITE(*,101) ' (main program head)'
              END IF
C copiamos cuerpo del programa principal desde el directorio de xpgp
              IF(LOK)THEN
                COMANDO(1:6)='cp -f '
                XPGPDIR_=XPGPDIR
                L1=TRUEBEG(XPGPDIR_)
                L2=TRUELEN(XPGPDIR_)
                COMANDO(7:L2-L1+7)=XPGPDIR_(L1:L2)
                COMANDO(L2-L1+8:)='/compute_polynomial.1'
                ISYSTEM=SYSTEMFUNCTION(COMANDO)
                IF((ISYSTEM.EQ.127.OR.ISYSTEM.EQ.-1))THEN
                  WRITE(*,101) 'ERROR: while calling system function.'
                  WRITE(*,100) 'Press <CR> to continue...'
                  IF(LBATCH)THEN
                    WRITE(*,*)
                  ELSE
                    READ(*,*)
                  END IF
                  LOK=.FALSE.
                ELSE
                  WRITE(*,100) 'INFO: file compute_polynomial.1'
                  WRITE(*,101) ' (main program body)'
                  LOK=.TRUE.
                END IF
              END IF
c si existe algun fichero compute_polynomial.2, lo borramos
              IF(LOK)THEN
                INQUIRE(FILE='./compute_polynomial.2',EXIST=LOGFILE)
                IF(LOGFILE)THEN
                  ISYSTEM=SYSTEMFUNCTION('rm -f compute_polynomial.2')
                  IF((ISYSTEM.EQ.127).OR.(ISYSTEM.EQ.-1))THEN
                    WRITE(*,101) 'ERROR: while calling system function.'
                    WRITE(*,100) 'Press <CR> to continue...'
                    IF(LBATCH)THEN
                      WRITE(*,*)
                    ELSE
                      READ(*,*)
                    END IF
                    LOK=.FALSE.
                  ELSE
                    LOK=.TRUE.
                  END IF
                ELSE
                  LOK=.TRUE.
                END IF
              END IF
c generamos subrutina en compute_polynomial.2
              IF(LOK)THEN
                OPEN(10,FILE='compute_polynomial.2',STATUS='NEW',
     >           FORM='FORMATTED')
                WRITE(10,100) '        SUBROUTINE SCOMPUTE_POLYNOMIAL'
                WRITE(10,101) '(X0,Y0,ERR_X0,ERR_Y0,ERR_EXPY0)'
                WRITE(10,101) '        IMPLICIT NONE'
                WRITE(10,101) '        DOUBLE PRECISION X0'
                WRITE(10,100) '        DOUBLE PRECISION'
                WRITE(10,101) ' Y0,ERR_X0,ERR_Y0,ERR_EXPY0'
                WRITE(10,101) 'C'
                WRITE(10,101) '        INTEGER NDEG'
                WRITE(10,100) '        PARAMETER (NDEG='
                WRITE(CDUMMY,*) NDEG
                L1=TRUEBEG(CDUMMY)
                L2=TRUELEN(CDUMMY)
                WRITE(10,100) CDUMMY(L1:L2)
                WRITE(10,101) ')'
                WRITE(10,101) 'C'
                WRITE(10,101) '        INTEGER I,J'
                WRITE(10,101) '        INTEGER K,KK'
                WRITE(10,101) '        INTEGER NCOEF'
                WRITE(10,101) '        DOUBLE PRECISION A(NDEG+1)'
                WRITE(10,100) '        DOUBLE PRECISION'
                WRITE(10,101) ' COVAR(NDEG+1,NDEG+1)'
                WRITE(10,101) '        DOUBLE PRECISION SR2'
                WRITE(10,101) '        DOUBLE PRECISION XX1(NDEG+1)'
                WRITE(10,101) '        DOUBLE PRECISION XX2(NDEG+1)'
                WRITE(10,101) '        DOUBLE PRECISION XX3(NDEG+1)'
                WRITE(10,101) '        DOUBLE PRECISION YY0'
                WRITE(10,101) '        DOUBLE PRECISION DUMMY'
                WRITE(10,101) '        LOGICAL IFCOEF(NDEG+1)'
                WRITE(10,101) 'C'
                DO K=1,NDEG+1
                  WRITE(10,'(A,I2.2,A,$)') '        A(',K,')='
                  WRITE(10,*) A(K)
                END DO
                DO K=1,NDEG+1
                  WRITE(10,'(A,I2.2,A,$)') '        IFCOEF(',K,')='
                  IF(IFCOEF(K))THEN
                    WRITE(10,101) '.TRUE.'
                  ELSE
                    WRITE(10,101) '.FALSE.'
                  END IF
                END DO
                II=0
                DO I=1,NDEG+1
                  IF(IFCOEF(I))THEN
                    II=II+1
                    JJ=0
                    DO J=1,NDEG+1
                      IF(IFCOEF(J))THEN
                        JJ=JJ+1
                        WRITE(10,'(A,I2.2,A,I2.2,A,$)') 
     >                   '        COVAR(',JJ,',',II,')='
                        WRITE(10,*) COVAR(JJ,II) 
                      END IF
                    END DO
                  END IF
                END DO
                WRITE(10,100) '        SR2='
                WRITE(10,*) SR2
                WRITE(10,101) 'C'
                WRITE(10,101) '        Y0=A(NDEG+1)'
                WRITE(10,101) '        IF(NDEG.GT.0)THEN'
                WRITE(10,101) '          DO K=NDEG,1,-1'
                WRITE(10,101) '            Y0=Y0*X0+A(K)'
                WRITE(10,101) '          END DO'
                WRITE(10,101) '        END IF'
                WRITE(10,101) 'C'
                WRITE(10,101) '        YY0=DBLE(NDEG)*A(NDEG+1)'
                WRITE(10,101) '        IF(NDEG.GT.1)THEN'
                WRITE(10,101) '          DO K=NDEG,2,-1'
                WRITE(10,101) '            YY0=YY0*X0+DBLE(K-1)*A(K)'
                WRITE(10,101) '           END DO'
                WRITE(10,101) '        END IF'
                WRITE(10,101) 'C'
                WRITE(10,101) '        KK=0'
                WRITE(10,101) '        DO K=1,NDEG+1'
                WRITE(10,101) '          IF(IFCOEF(K))THEN'
                WRITE(10,101) '            KK=KK+1'
                WRITE(10,101) '            IF(K.EQ.1)THEN'
                WRITE(10,101) '              XX1(KK)=1.D0'
                WRITE(10,101) '              XX2(KK)=1.D0'
                WRITE(10,101) '            ELSE'
                WRITE(10,101) '              XX1(KK)=X0**K'
                WRITE(10,101) '              XX2(KK)=X0**K'
                WRITE(10,101) '            END IF'
                WRITE(10,101) '          END IF'
                WRITE(10,101) '        END DO'
                WRITE(10,101) '        NCOEF=KK'
                WRITE(10,101) 'C'
                WRITE(10,101) '        DO I=1,NCOEF'
                WRITE(10,101) '          XX3(I)=0.D0'
                WRITE(10,101) '          DO J=1,NCOEF'
                WRITE(10,100) '            XX3(I)='
                WRITE(10,101) 'XX3(I)+XX1(J)*COVAR(J,I)'
                WRITE(10,101) '          END DO'
                WRITE(10,101) '        END DO'
                WRITE(10,101) '        DUMMY=0.D0'
                WRITE(10,101) '        DO I=1,NCOEF'
                WRITE(10,101) '          DUMMY=DUMMY+XX3(I)*XX2(I)'
                WRITE(10,101) '        END DO'
                WRITE(10,100) '        ERR_Y0=DSQRT(DUMMY*SR2'
                WRITE(10,101) '+YY0*YY0*ERR_X0*ERR_X0)'
                WRITE(10,100) '        ERR_EXPY0=DSQRT((1.D0+DUMMY)*SR2'
                WRITE(10,101) '+YY0*YY0*ERR_X0*ERR_X0)'
                WRITE(10,101) 'C'
                WRITE(10,101) '        END'
                CLOSE(10)
                WRITE(*,100) 'INFO: file compute_polynomial.2'
                WRITE(*,101) ' (subroutine)'
              END IF
C unimos todos los subficheros en un fichero unico
              IF(LOK)THEN
                COMANDO='cat compute_polynomial.? > '
                L2=TRUELEN(COMANDO)
                COMANDO(L2+1:)=FORTRAN_FILE
                ISYSTEM=SYSTEMFUNCTION(COMANDO)
                IF((ISYSTEM.EQ.127.OR.ISYSTEM.EQ.-1))THEN
                  WRITE(*,101) 'ERROR: while calling system function.'
                  WRITE(*,100) 'Press <CR> to continue...'
                  IF(LBATCH)THEN
                    WRITE(*,*)
                  ELSE
                    READ(*,*)
                  END IF
                  LOK=.FALSE.
                ELSE
                  L1=TRUEBEG(FORTRAN_FILE)
                  L2=TRUELEN(FORTRAN_FILE)
                  WRITE(*,100) 'INFO: file '
                  WRITE(*,100) FORTRAN_FILE(L1:L2)
                  WRITE(*,101) ' (main program + subroutine)'
                  WRITE(*,100) '      ...now compiling (please wait)...'
                  LOK=.TRUE.
                END IF
              END IF
C compilamos el programa
              IF(LOK)THEN
                COMANDO='f77 -o '
                LL1=TRUEBEG(FORTRAN_FILE)
                LL2=TRUELEN(FORTRAN_FILE)
                COMANDO(8:)=FORTRAN_FILE(LL1:LL2-2)
                L2=TRUELEN(COMANDO)
                COMANDO(L2+1:L2+1)=' '
                COMANDO(L2+2:)=FORTRAN_FILE(LL1:LL2)
                L2=TRUELEN(COMANDO)
                COMANDO(L2+1:L2+1)=' '
                LL1=TRUEBEG(XPGPDIR_)
                LL2=TRUELEN(XPGPDIR_)
                COMANDO(L2+2:)=XPGPDIR_(LL1:LL2)
                L2=TRUELEN(COMANDO)
                COMANDO(L2+1:)='/iofunctions.f '
                L2=TRUELEN(COMANDO)
                COMANDO(L2+2:)=XPGPDIR_(LL1:LL2)
                L2=TRUELEN(COMANDO)
                COMANDO(L2+1:)='/statistic.f '
                L2=TRUELEN(COMANDO)
                COMANDO(L2+2:)=XPGPDIR_(LL1:LL2)
                L2=TRUELEN(COMANDO)
                COMANDO(L2+1:)='/truebeg.f '
                L2=TRUELEN(COMANDO)
                COMANDO(L2+2:)=XPGPDIR_(LL1:LL2)
                L2=TRUELEN(COMANDO)
                COMANDO(L2+1:)='/truelen.f '
                L2=TRUELEN(COMANDO)
                COMANDO(L2+2:)=XPGPDIR_(LL1:LL2)
                L2=TRUELEN(COMANDO)
                COMANDO(L2+1:)='/fextrae.f '
                L2=TRUELEN(COMANDO)
                COMANDO(L2+2:)=XPGPDIR_(LL1:LL2)
                L2=TRUELEN(COMANDO)
                COMANDO(L2+1:)='/downhill.f'
                ISYSTEM=SYSTEMFUNCTION(COMANDO)
                WRITE(*,*)
                IF((ISYSTEM.EQ.127.OR.ISYSTEM.EQ.-1))THEN
                  WRITE(*,101) 'ERROR: while calling system function.'
                  WRITE(*,100) 'Press <CR> to continue...'
                  IF(LBATCH)THEN
                    WRITE(*,*)
                  ELSE
                    READ(*,*)
                  END IF
                  LOK=.FALSE.
                ELSE
                  L1=TRUEBEG(FORTRAN_FILE)
                  L2=TRUELEN(FORTRAN_FILE)
                  WRITE(*,100) 'INFO: file '
                  WRITE(*,100) FORTRAN_FILE(L1:L2-2)
                  WRITE(*,101) ' (compiled program)'
                  LOK=.TRUE.
                END IF
              END IF
C borramos ficheros intermedios
              IF(LOK)THEN
                ISYSTEM=SYSTEMFUNCTION('rm -f compute_polynomial.?')
              END IF
              WRITE(*,*)
C..............................................................................
            ELSEIF(ICSAVE.EQ.7)THEN      !remove buffer data beyond conf. level
              FALPHA=-1.
              DO WHILE((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))
                WRITE(*,100) '* NOTE: alfa(1sigma)=0.1587'
                WRITE(*,100) ', alfa(2sigma)=0.0228'
                WRITE(*,101) ', alfa(3sigma)=0.00135'
                WRITE(*,100) 'Significance level (alpha/2) '
                WRITE(*,100) 'to remove buffer data '
                FALPHA=READF_B('0.00135')
                IF((FALPHA.LT.0.0).OR.(FALPHA.GT.0.5))THEN
                  WRITE(*,101) 'ERROR: alpha/2 must be in [0.0,0.5]'
                  WRITE(*,100) 'Press <CR> to continue...'
                  IF(LBATCH)THEN
                    WRITE(*,*)
                  ELSE
                    READ(*,*)
                  END IF
                END IF
              END DO
              WRITE(77,*) FALPHA
              FACTOR_TSTUDENT=FTSTUDENTI(NF-NCOEF,FALPHA)
              NDATA=0
              NREMOVED=0
              DO N=1,NDATABUFF(NB0)
                X0=DBLE(XDATA(N,NB0))
                ERRX0=DBLE(EXDATA(N,NB0))
                Y0=A(NDEG+1)                                   !polinomio en X0
                IF(NDEG.GT.0)THEN
                  DO K=NDEG,1,-1
                    Y0=Y0*X0+A(K)
                  END DO
                END IF
                YY0=DBLE(NDEG)*A(NDEG+1)                !derivada primera en X0
                IF(NDEG.GT.1)THEN
                  DO K=NDEG,2,-1
                    YY0=YY0*X0+DBLE(K-1)*A(K)
                  END DO
                END IF
                KK=0
                DO K=1,NDEG+1
                  IF(IFCOEF(K))THEN
                    KK=KK+1
                    IF(K.EQ.1)THEN
                      XX1(KK)=1.D0
                      XX2(KK)=1.D0
                    ELSE
                      XX1(KK)=X0**K
                      XX2(KK)=X0**K
                    END IF
                  END IF
                END DO
                DO I=1,NCOEF
                  XX3(I)=0.D0
                  DO J=1,NCOEF
                    XX3(I)=XX3(I)+XX1(J)*COVAR(J,I)
                  END DO
                END DO
                ERRY0=0.D0
                DO I=1,NCOEF
                  ERRY0=ERRY0+XX3(I)*XX2(I)
                END DO
                ERRY0=DSQRT((1.D0+ERRY0)*SR2+YY0*YY0*ERRX0*ERRX0)
                ERRY0=ERRY0*FACTOR_TSTUDENT
                IF(ABS(REAL(Y0)-YDATA(N,NB0)).LE.ERRY0)THEN
                  NDATA=NDATA+1
                  XDATA(NDATA,NB)=X0
                  EXDATA(NDATA,NB)=EXDATA(N,NB0)
                  YDATA(NDATA,NB)=YDATA(N,NB0)
                  EYDATA(NDATA,NB)=EYDATA(N,NB0)
                ELSE
                  NREMOVED=NREMOVED+1
                  CALL PGPOINT(1,XDATA(N,NB0),YDATA(N,NB0),5)
                END IF
              END DO
              WRITE(*,100) 'No. of data points removed: '
              WRITE(*,*) NREMOVED
              NDATABUFF(NB)=NDATA
              LXERR(NB)=LXERR(NB0)
              LYERR(NB)=LYERR(NB0)
              CALL UPDATELIMITS(NB)
              NSYMBBUFF(NB)=NSYMBBUFF(NB0)
              LDEFBUFF(NB)=.TRUE.
              LUSEBUFF(NB)=.TRUE.
              WRITE(*,100) 'Key label for new buffer '
              DATAKEY(NB)=READC_B('stripped data','@')
              L1=TRUEBEG(DATAKEY(NB))
              L2=TRUELEN(DATAKEY(NB))
              WRITE(77,101) DATAKEY(NB)(L1:L2)
              CALL SHOW_BUFFERS
C..............................................................................
            END IF
          END DO
        END IF
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C borramos botones
        DO K=1,NDEGMAX+1
          WRITE(CDUMMY,*) K-1
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(NBDEG(K),'a\\d'//CDUMMY(L1:L2),-1)
        END DO
        CALL BUTTON(38,'go >>>',-1)
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
102     FORMAT(79('-'))
        END
