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
        SUBROUTINE OTHERFIT(NB0)
        IMPLICIT NONE
        INTEGER NB0
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
        INCLUDE 'nfixedmax.inc'
        INCLUDE 'nknotsmax.inc'
        INCLUDE 'ndegmax.inc'
C
        INTEGER NPLOTMAX
        PARAMETER (NPLOTMAX=1000)
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER READI_B
        INTEGER READILIM_B
        REAL READF_B
        REAL FPOLY
        REAL RANRED
        CHARACTER*255 READC_B
C
        INTEGER I
        INTEGER IOPC
        INTEGER NTERMS
        INTEGER NFIXED
        INTEGER IKNOT,NKNOTS
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER NDATA
        INTEGER IEXPAND
        INTEGER NF
        INTEGER NB
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        INTEGER L1,L2
        INTEGER ILUP
        INTEGER NEVALMAX
        INTEGER NSEED
        INTEGER I0SPL
        INTEGER MODE
        REAL YRMSTOL
        REAL WEIGHT
        REAL POWER
        REAL EPOWER
        REAL A(NDEGMAX+1)
        REAL XKNOT(NKNOTSMAX),YKNOT(NKNOTSMAX)
        REAL ASPL(NKNOTSMAX),BSPL(NKNOTSMAX),CSPL(NKNOTSMAX)
        REAL XDATA(NDATAMAX,NBUFFMAX),YDATA(NDATAMAX,NBUFFMAX)
        REAL EXDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XF(NDATAMAX),YF(NDATAMAX),EYF(NDATAMAX)
        REAL XFIXED(NFIXEDMAX),YFIXED(NFIXEDMAX)
        REAL FIXEDWEIGHT
        REAL XFMIN,XFMAX
        REAL XP(NPLOTMAX),YP(NPLOTMAX)
        REAL XMIN,XMAX,YMIN,YMAX
        REAL XMIN0,XMAX0,XMINF,XMAXF
        REAL TSIGMA
        REAL RDUMMY
        REAL CHISQR
        REAL XMINFIT,XMAXFIT,YMINFIT,YMAXFIT
        CHARACTER*1 CSAVE,COPC,CERR,CNOR
        CHARACTER*50 CDUMMY
        CHARACTER*50 DATAKEY(NBUFFMAX),DATAKEY_
        CHARACTER*50 CXMINF,CXMAXF
        LOGICAL LUP
        LOGICAL LNOR
        LOGICAL LOOP
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
C
        COMMON/BLKSETTINGS1/XMIN,XMAX,YMIN,YMAX
        COMMON/BLKSETTINGS7/IEXPAND
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKBUFFERS3/NSYMBBUFF,LWBUFF
        COMMON/BLKXYERR/LXERR,LYERR
        COMMON/BLKSETTINGS8B/DATAKEY
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKFIXED1/NFIXED
        COMMON/BLKFIXED2/XFIXED,YFIXED
        COMMON/BLKFIXED3/FIXEDWEIGHT
C------------------------------------------------------------------------------
C tipos de ajuste
        WRITE(*,*)
        WRITE(*,101) '(1) upper/lower boundary (pseudofit)'
        WRITE(*,101) '(2) adaptive splines'
        WRITE(*,101) '(3) normal polynomials with POLFIT '//
     +   '(Bevington 1969)'
        WRITE(*,101) '(0) EXIT'
        WRITE(*,100) 'Option '
        IOPC=READILIM_B('0',0,3)
        WRITE(77,111) IOPC,
     +   '# 1=pseudofit, 2=adaptive splines, 0=EXIT'
        IF(IOPC.EQ.0) RETURN
C------------------------------------------------------------------------------
C datos a ajustar
        NF=NDATABUFF(NB0)
        DO I=1,NF
          XF(I)=XDATA(I,NB0)
          YF(I)=YDATA(I,NB0)
          EYF(I)=EYDATA(I,NB0)
        END DO
C------------------------------------------------------------------------------
        IF((IOPC.EQ.1).OR.(IOPC.EQ.2))THEN
          WRITE(*,100) 'Number of fixed points '
          NFIXED=READI_B('0')
          IF(NFIXED.GT.NFIXEDMAX)THEN
            WRITE(*,100) 'NFIXEDMAX: '
            WRITE(*,*) NFIXEDMAX
            WRITE(*,100) 'NFIXED...: '
            WRITE(*,*) NFIXED
            WRITE(*,101) 'FATAL ERROR: NFIXED.GT.NFIXEDMAX'
            STOP
          END IF
          WRITE(77,111) NFIXED,'# Number of fixed points'
          IF(NFIXED.LT.0)THEN
            WRITE(*,101) 'WARNING: invalid number. NFIXED set to 0.'
            NFIXED=0
          ELSEIF(NFIXED.GT.0)THEN
            WRITE(*,100) 'WEIGHT for fixed points '
            FIXEDWEIGHT=READF_B('1.E6')
            WRITE(77,*) FIXEDWEIGHT,'# WEIGHT for fixed points'
            DO I=1,NFIXED
              WRITE(*,'(A,I2,$)') 'X-coordinate of point #',I
              XFIXED(I)=READF_B('@')
              WRITE(77,*) XFIXED(I),'# X-coordinate of point #',I
              WRITE(*,'(A,I2,$)') 'Y-coordinate of point #',I
              YFIXED(I)=READF_B('@')
              WRITE(77,*) YFIXED(I),'# Y-coordinate of point #',I
            END DO
          END IF
        END IF
C------------------------------------------------------------------------------
C ajustes
        IF(IOPC.EQ.1)THEN !...................................simple polynomial
          !parametros para el ajuste
          WRITE(*,100) 'Polynomial degree'
          WRITE(CDUMMY,*) NFIXED
          NTERMS=READILIM_B(CDUMMY,0,10)
          WRITE(77,111) NTERMS,'# Polynomial degree'
          NTERMS=NTERMS+1
          WRITE(*,101) '(Note: WEIGHT=1.0 is equivalent to a '//
     +     'normal fit to a simple polynomial)'
          WRITE(*,100) 'WEIGHT for pseudofit '
          WEIGHT=READF_B('1000.0')
          WRITE(77,*) WEIGHT,'# WEIGHT for pseudofit (1.0=no pseudofit)'
          WRITE(*,100) 'POWER for pseudofit '
          POWER=READF_B('2.0')
          WRITE(77,*) POWER,'# POWER for pseudofit'
          WRITE(*,100) 'EPOWER for pseudofit '
          EPOWER=READF_B('2.0')
          WRITE(77,*) EPOWER,'# EPOWER for pseudofit'
          WRITE(*,100) 'Which side: 1=upper, 2=lower '
          ILUP=READILIM_B('@',1,2)
          WRITE(77,111) ILUP,'# side: 1=upper, 2=lower'
          LUP=(ILUP.EQ.1)
          WRITE(*,100) 'Are you considering error bars (y/n) '
          CERR(1:1)=READC_B('n','yn')
          WRITE(77,112) CERR,'# Using error bars (y/n)?'
          IF(CERR.EQ.'y')THEN
            LOOP=.TRUE.
            DO WHILE(LOOP)
              WRITE(*,100) 'Times sigma to fit data (0.0=none) '
              TSIGMA=READF_B('1.0')
              IF(TSIGMA.LT.0.0)THEN
                WRITE(*,100) 'WARNING: this number must be >= 0.0.'
                WRITE(*,101) ' Try again!'
              ELSE
                LOOP=.FALSE.
              END IF
            END DO
            WRITE(77,*) TSIGMA,'# Times sigma to fit data (0=none)'
          ELSE
            TSIGMA=0.0
          END IF
          !parametros para DOWNHILL
          WRITE(*,100) 'YRMSTOL for DOWNHILL '
          YRMSTOL=READF_B('1E-5')
          WRITE(77,*) YRMSTOL,'# YRMSTOL for DOWNHILL'
          WRITE(*,100) 'NEVALMAX for DOWNHILL '
          NEVALMAX=READILIM_B('1000',1,1000000)
          WRITE(77,111) NEVALMAX,'# NEVALMAX for DOWNHILL'
          !realizamos el ajuste
          CALL PSEUDOFIT(XF,YF,EYF,NF,NTERMS,YRMSTOL,NEVALMAX,
     +     WEIGHT,POWER,EPOWER,LUP,TSIGMA,A)
C..............................................................................
        ELSEIF(IOPC.EQ.2)THEN !................................adaptive splines
          !parametros para el ajuste
          WRITE(*,100) 'Number of knots '
          NKNOTS=READILIM_B('@',2,20)
          WRITE(77,111) NKNOTS,'# Number of knots'
          !como los datos no tienen por que venir ordenados, buscamos
          !los extremos en el eje X para fijar ahi al menos dos knots
          CALL FINDMML(NF,1,NF,XF,XFMIN,XFMAX)
          XKNOT(1)=XFMIN
          DO IKNOT=1,NKNOTS-1
            XKNOT(IKNOT+1)=XFMIN+
     +       (XFMAX-XFMIN)*REAL(IKNOT)/REAL(NKNOTS-1)
          END DO
          WRITE(*,101) '(Note: WEIGHT=1.0 is equivalent to a '//
     +     'normal fit by splines)'
          WRITE(*,100) 'WEIGHT for pseudofit '
          WEIGHT=READF_B('1000.0')
          WRITE(77,*) WEIGHT,'# WEIGHT for pseudofit (1.0=no pseudofit)'
          WRITE(*,100) 'POWER for pseudofit '
          POWER=READF_B('2.0')
          WRITE(77,*) POWER,'# POWER for pseudofit'
          WRITE(*,100) 'EPOWER for pseudofit '
          EPOWER=READF_B('2.0')
          WRITE(77,*) EPOWER,'# EPOWER for pseudofit'
          WRITE(*,100) 'Which side: 1=upper, 2=lower '
          ILUP=READILIM_B('@',1,2)
          WRITE(77,111) ILUP,'# side: 1=upper, 2=lower'
          LUP=(ILUP.EQ.1)
          WRITE(*,100) 'Are you considering error bars (y/n) '
          CERR(1:1)=READC_B('n','yn')
          WRITE(77,112) CERR,'# Using error bars (y/n)?'
          IF(CERR.EQ.'y')THEN
            LOOP=.TRUE.
            DO WHILE(LOOP)
              WRITE(*,100) 'Times sigma to fit data (0.0=none) '
              TSIGMA=READF_B('1.0')
              IF(TSIGMA.LT.0.0)THEN
                WRITE(*,100) 'WARNING: this number must be >= 0.0.'
                WRITE(*,101) ' Try again!'
              ELSE
                LOOP=.FALSE.
              END IF
            END DO
            WRITE(77,*) TSIGMA,'# Times sigma to fit data (0=none)'
          ELSE
            TSIGMA=0.0
          END IF
          !parametros para DOWNHILL
          WRITE(*,100) 'YRMSTOL for DOWNHILL '
          YRMSTOL=READF_B('1E-5')
          WRITE(77,*) YRMSTOL,'# YRMSTOL for DOWNHILL'
          WRITE(*,100) 'NEVALMAX for DOWNHILL '
          NEVALMAX=READILIM_B('1000',1,1000000)
          WRITE(77,111) NEVALMAX,'# NEVALMAX for DOWNHILL'
          !semilla para numeros aleatorios
          WRITE(*,101) '(Note: NSEED must be > 0 to make the '//
     +     'merging-knots process repeatable)'
          WRITE(*,100) 'NSEED, negative to call srand(time()) '
          NSEED=READI_B('-1')
          WRITE(77,111) NSEED,'# NSEED for random numbers'
          IF(NSEED.LT.0) RDUMMY=RANRED(NSEED)
          !realizamos el ajuste
          CALL SPLFIT(NF,XF,YF,EYF,NKNOTS,XKNOT,YRMSTOL,NEVALMAX,NSEED,
     +     WEIGHT,POWER,EPOWER,LUP,TSIGMA,
     +     NPLOTMAX,XP,YP,XKNOT(1),XKNOT(NKNOTS),YKNOT,ASPL,BSPL,CSPL)
C..............................................................................
        ELSEIF(IOPC.EQ.3)THEN !................polinomios "normales" con POLFIT
          !parametros para el ajuste
          WRITE(*,100) 'Polynomial degree'
          NTERMS=READILIM_B('@',0,19)
          WRITE(77,111) NTERMS,'# Polynomial degree'
          NTERMS=NTERMS+1
          WRITE(*,100) 'MODE for POLFIT (0=no weighting) '
          MODE=READILIM_B('0',-1,1)
          WRITE(77,111) MODE,'# MODE for POLFIT (0=no weighting)'
          IF(MODE.EQ.0)THEN
            WRITE(*,100) 'Re-normalize X & Y ranges (y/n) '
            CNOR(1:1)=READC_B('y','yn')
            WRITE(77,112) CNOR,'# Re-normalize X & Y ranges?'
            LNOR=(CNOR.EQ.'y')
          ELSE
            LNOR=.FALSE.
          END IF
          IF(LNOR)THEN
            WRITE(*,100) 'Xmin for fit '
            XMINFIT=READF_B('-1.0')
            WRITE(77,*) XMINFIT,'# Xmin for fit'
            WRITE(*,100) 'Xmax for fit '
            XMAXFIT=READF_B('+1.0')
            WRITE(77,*) XMAXFIT,'# Xmax for fit'
            WRITE(*,100) 'Ymin for fit '
            YMINFIT=READF_B('-1.0')
            WRITE(77,*) YMINFIT,'# Ymin for fit'
            WRITE(*,100) 'Ymax for fit '
            YMAXFIT=READF_B('+1.0')
            WRITE(77,*) YMAXFIT,'# Ymax for fit'
          ELSE
            XMINFIT=0.0
            XMAXFIT=1.0
            YMINFIT=0.0
            YMAXFIT=1.0
          END IF
          !realizamos el ajuste
          CALL POLFIT(XF,YF,EYF,NF,NTERMS,MODE,A,CHISQR,
     +     LNOR,XMINFIT,XMAXFIT,YMINFIT,YMAXFIT)
          DO I=1,NTERMS
            WRITE(*,'(A2,I2.2,A2,$)') 'a(',I,')='
            WRITE(*,*) A(I)
          END DO
          WRITE(*,100) 'POLFIT> CHISQR: '
          WRITE(*,*) CHISQR
C..............................................................................
        ELSE !...................................................invalid option
          WRITE(*,100) 'IOPC='
          WRITE(*,*) IOPC
          STOP 'FATAL ERROR in subroutine otherfit.f'
        END IF
C------------------------------------------------------------------------------
C dibujamos el ajuste y preguntamos si queremos salvarlo en algun buffer
        !determinamos los limites para dibujar el ajuste
        CALL FINDMML(NF,1,NF,XF,XMIN0,XMAX0)
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
        !calculamos el ajuste
        DO I=1,NPLOTMAX
          XP(I)=XMINF+(XMAXF-XMINF)*REAL(I-1)/REAL(NPLOTMAX-1)
        END DO
        IF(IOPC.EQ.1)THEN !...........................................pseudofit
          DO I=1,NPLOTMAX
            YP(I)=FPOLY(NTERMS-1,A,XP(I))
          END DO
        ELSEIF(IOPC.EQ.2)THEN !................................adaptive splines
          I0SPL=1 !hace falta para comenzar buscando en el inicio de la tabla
          DO I=1,NPLOTMAX
            CALL CUBSPLX(XKNOT,YKNOT,ASPL,BSPL,CSPL,NKNOTS,I0SPL,
     +       XP(I),YP(I))
          END DO
        ELSEIF(IOPC.EQ.3)THEN !...................normal polynomial with POLFIT
          DO I=1,NPLOTMAX
            YP(I)=FPOLY(NTERMS-1,A,XP(I))
          END DO
        ELSE
          WRITE(*,100) 'IOPC='
          WRITE(*,*) IOPC
          STOP 'FATAL ERROR in subroutine otherfit.f'
        END IF
        CALL PGSCI(0)
        CALL PGLINE(NPLOTMAX,XP,YP)
        !preguntamos si queremos salvar el ajuste
        WRITE(*,100) 'Are you saving fit results (y/n) '
        CSAVE(1:1)=READC_B('n','yn')
        WRITE(77,112) CSAVE,'# save fit results'
        IF(CSAVE.EQ.'y')THEN
          COPC=' '
          DO WHILE(COPC.NE.'0')
            WRITE(*,101) '(1) Save last fit into buffer'
            WRITE(*,101) '(2) Save fit predictions into buffer'
            IF(IOPC.EQ.2)THEN
              WRITE(*,101) '(K) Save knots'
            END IF
            WRITE(*,101) '(0) EXIT'
            WRITE(*,100) 'Option '
            COPC(1:1)=READC_B('0','012kK')
            IF(COPC.EQ.'k') COPC='K'
            WRITE(77,112) COPC,
     +       '# save: 1=last fit, 2=fit predictions, k=knots, 0=EXIT'
            IF(COPC.NE.'0')THEN
              !pedimos nuevo buffer (puede ser el mismo NB0)
              WRITE(*,100) 'Buffer # to store new data'
              NB=READILIM_B('@',1,NBUFFMAX)
              WRITE(77,111) NB,'# Selected buffer number'
              IF(COPC.EQ.'1')THEN
                WRITE(*,100) 'Xmin '
                IF(IOPC.EQ.2)THEN
                  WRITE(CDUMMY,*) XFMIN
                ELSE
                  WRITE(CDUMMY,*) XMINF
                END IF
                XMINF=READF_B(CDUMMY)
                WRITE(77,*) XMINF,'# Xmin'
                WRITE(*,100) 'Xmax '
                IF(IOPC.EQ.2)THEN
                  WRITE(CDUMMY,*) XFMAX
                ELSE
                  WRITE(CDUMMY,*) XMAXF
                END IF
                XMAXF=READF_B(CDUMMY)
                WRITE(77,*) XMAXF,'# Xmax'
                WRITE(*,100) 'Number of points '
                NDATA=READILIM_B('1000',2,NDATAMAX)
                WRITE(77,111) NDATA,'# Number of points'
                NDATABUFF(NB)=NDATA
                DO I=1,NDATABUFF(NB)
                  XDATA(I,NB)=XMINF+(XMAXF-XMINF)*REAL(I-1)/
     +             REAL(NDATABUFF(NB)-1)
                END DO
              ELSEIF(COPC.EQ.'2')THEN
                NDATABUFF(NB)=NF
                DO I=1,NDATABUFF(NB)
                  XDATA(I,NB)=XF(I)
                END DO
              ELSEIF(COPC.EQ.'K')THEN
                NDATABUFF(NB)=NKNOTS
                DO I=1,NDATABUFF(NB)
                  XDATA(I,NB)=XKNOT(I)
                  YDATA(I,NB)=YKNOT(I)
                END DO
              ELSE
                WRITE(*,101) 'FATAL ERROR in subrotine OTHERFIT'
                WRITE(*,101) 'COPC='//COPC
                STOP
              END IF
              IF(COPC.NE.'K')THEN
                IF((IOPC.EQ.1).OR.(IOPC.EQ.3))THEN !..................pseudofit
                  DO I=1,NDATABUFF(NB)
                    YDATA(I,NB)=FPOLY(NTERMS-1,A,XDATA(I,NB))
                  END DO
                ELSEIF(IOPC.EQ.2)THEN !........................adaptive splines
                  I0SPL=1 !comenzar buscando en el inicio de la tabla
                  DO I=1,NDATABUFF(NB)
                    CALL CUBSPLX(XKNOT,YKNOT,ASPL,BSPL,CSPL,
     +               NKNOTS,I0SPL,XDATA(I,NB),YDATA(I,NB))
                  END DO
                ELSEIF(IOPC.EQ.3)THEN !..........normal polynomials with POLFIT
                  DO I=1,NDATABUFF(NB)
                    YDATA(I,NB)=FPOLY(NTERMS-1,A,XDATA(I,NB))
                  END DO
                ELSE
                  WRITE(*,100) 'IOPC='
                  WRITE(*,*) IOPC
                  STOP 'FATAL ERROR in subroutine otherfit.f'
                END IF
              END IF
              DO I=1,NDATABUFF(NB)
                EXDATA(I,NB)=0.
                EYDATA(I,NB)=0.
              END DO
              LXERR(NB)=.FALSE.
              LYERR(NB)=.FALSE.
              CALL UPDATELIMITS(NB)
              IF(COPC.EQ.'K')THEN
                NSYMBBUFF(NB)=17   !circulo solido
              ELSE
                NSYMBBUFF(NB)=1001 !linea continua
              END IF
              LDEFBUFF(NB)=.TRUE.
              LUSEBUFF(NB)=.TRUE.
              WRITE(*,100) 'Key label '
              IF(IOPC.EQ.1)THEN
                DATAKEY_(1:50)=READC_B('polynomial pseudofit','@')
              ELSEIF(IOPC.EQ.2)THEN
                DATAKEY_(1:50)=READC_B('adaptive splines','@')
              ELSEIF(IOPC.EQ.3)THEN
                DATAKEY_(1:50)=READC_B('polynomial (POLFIT)','@')
              END IF
              DATAKEY(NB)=DATAKEY_
              L1=TRUEBEG(DATAKEY(NB))
              L2=TRUELEN(DATAKEY(NB))
              CALL TOLOG77_STRING(DATAKEY(NB)(L1:L2),'Key label')
              CALL SHOW_BUFFERS
            END IF
          END DO
        END IF
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
112     FORMAT(11X,A1,1X,A)
        END
