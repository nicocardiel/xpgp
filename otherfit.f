        SUBROUTINE OTHERFIT(NB0)
        IMPLICIT NONE
        INTEGER NB0
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER NDEGMAX
        PARAMETER (NDEGMAX=16)
        INTEGER NKNOTSMAX
        PARAMETER (NKNOTSMAX=20)
        INTEGER NPLOTMAX
        PARAMETER (NPLOTMAX=1000)
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER READI_B
        INTEGER READILIM_B
        REAL READF_B
        REAL FPOLY
        CHARACTER*255 READC_B
C
        INTEGER I
        INTEGER IOPC
        INTEGER NTERMS
        INTEGER IKNOT,NKNOTS
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER NDATA
        INTEGER ICSAVE
        INTEGER IEXPAND
        INTEGER NF
        INTEGER NB
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        INTEGER L1,L2
        INTEGER ILUP
        INTEGER NEVALMAX
        INTEGER NSEED
        INTEGER I0SPL
        REAL YRMSTOL
        REAL WEIGHT
        REAL POWER
        REAL A(NDEGMAX+1)
        REAL XKNOT(NKNOTSMAX),YKNOT(NKNOTSMAX)
        REAL ASPL(NKNOTSMAX),BSPL(NKNOTSMAX),CSPL(NKNOTSMAX)
        REAL XDATA(NDATAMAX,NBUFFMAX),YDATA(NDATAMAX,NBUFFMAX)
        REAL EXDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XF(NDATAMAX),YF(NDATAMAX),EYF(NDATAMAX)
        REAL XP(NPLOTMAX),YP(NPLOTMAX)
        REAL XMIN,XMAX,YMIN,YMAX
        REAL XMIN0,XMAX0,XMINF,XMAXF
        REAL TSIGMA
        CHARACTER*1 CSAVE,CERR
        CHARACTER*50 DATAKEY(NBUFFMAX),DATAKEY_
        CHARACTER*50 CXMINF,CXMAXF
        LOGICAL LUP
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
C------------------------------------------------------------------------------
C tipos de ajuste
        WRITE(*,*)
        WRITE(*,101) '(1) upper/lower boundary (pseudofit)'
        WRITE(*,101) '(2) adaptive splines'
        WRITE(*,101) '(0) EXIT'
        WRITE(*,100) 'Option '
        IOPC=READILIM_B('0',0,2)
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
C ajustes
        IF(IOPC.EQ.1)THEN !...........................................pseudofit
          !parametros para el ajuste
          WRITE(*,100) 'Polynomial degree'
          NTERMS=READILIM_B('@',0,10)
          WRITE(77,111) NTERMS,'# Polynomial degree'
          NTERMS=NTERMS+1
          WRITE(*,100) 'WEIGHT for pseudofit (1.0=no pseudofit='//
     +     'normal polynomial fit) '
          WEIGHT=READF_B('100')
          WRITE(77,*) WEIGHT,'# WEIGHT for pseudofit (1.0=no pseudofit)'
          WRITE(*,100) 'POWER for pseudofit '
          POWER=READF_B('2')
          WRITE(77,*) POWER,'# POWER for pseudofit'
          WRITE(*,100) 'Which side: 1=upper, 2=lower '
          ILUP=READILIM_B('1',1,2)
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
          NEVALMAX=READILIM_B('5000',10,1000000)
          WRITE(77,111) NEVALMAX,'# NEVALMAX for DOWNHILL'
          !realizamos el ajuste
          CALL PSEUDOFIT(XF,YF,EYF,NF,NTERMS,YRMSTOL,NEVALMAX,
     +     WEIGHT,POWER,LUP,TSIGMA,A)
C..............................................................................
        ELSEIF(IOPC.EQ.2)THEN !................................adaptive splines
          !parametros para el ajuste
          WRITE(*,100) 'Number of knots '
          NKNOTS=READILIM_B('5',2,20)
          WRITE(77,111) NKNOTS,'# Number of knots'
          XKNOT(1)=XF(1)
          DO IKNOT=1,NKNOTS-1
            XKNOT(IKNOT+1)=XF(1)+
     +       (XF(NF)-XF(1))*REAL(IKNOT)/REAL(NKNOTS-1)
          END DO
          WRITE(*,100) 'WEIGHT for pseudofit (1.0=no pseudofit='//
     +     'normal polynomial fit) '
          WEIGHT=READF_B('100')
          WRITE(77,*) WEIGHT,'# WEIGHT for pseudofit (1.0=no pseudofit)'
          WRITE(*,100) 'POWER for pseudofit '
          POWER=READF_B('2')
          WRITE(77,*) POWER,'# POWER for pseudofit'
          WRITE(*,100) 'Which side: 1=upper, 2=lower '
          ILUP=READILIM_B('1',1,2)
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
          NEVALMAX=READILIM_B('5000',10,1000000)
          WRITE(77,111) NEVALMAX,'# NEVALMAX for DOWNHILL'
          !semilla para numeros aleatorios
          WRITE(*,100) 'NSEED, negative to call srand(time()) '
          NSEED=READI_B('-1')
          WRITE(77,111) NSEED,'# NSEED for random numbers'
          !realizamos el ajuste
          CALL SPLFIT(NF,XF,YF,EYF,NKNOTS,XKNOT,YRMSTOL,NEVALMAX,NSEED,
     +     WEIGHT,POWER,LUP,TSIGMA,
     +     NPLOTMAX,XP,YP,XF(1),XF(NF),YKNOT,ASPL,BSPL,CSPL)
C..............................................................................
        ELSE !...................................................invalid option
          WRITE(*,100) 'IOPC='
          WRITE(*,*) IOPC
          STOP 'FATAL ERROR in subroutine otherfit.f'
        END IF
C------------------------------------------------------------------------------
C dibujamos el ajuste y preguntamos si queremos salvarlo en algun buffer
        !determinamos los limites para dibujar el ajuste
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
          ICSAVE=1
          DO WHILE(ICSAVE.NE.0)
            WRITE(*,101) '(1) Save last fit into buffer'
            WRITE(*,101) '(2) Save fit predictions into buffer'
            WRITE(*,101) '(0) EXIT'
            WRITE(*,100) 'Option '
            ICSAVE=READILIM_B('0',0,2)
            WRITE(77,111) ICSAVE,
     +       '# 1=save last fit, 2=save fit predictions, 0=EXIT'
            IF(ICSAVE.NE.0)THEN
              !pedimos nuevo buffer (puede ser el mismo NB0)
              WRITE(*,100) 'Buffer # to store new data'
              NB=READILIM_B('@',1,NBUFFMAX)
              WRITE(77,111) NB,'# Selected buffer number'
              IF(ICSAVE.EQ.1)THEN
                WRITE(*,100) 'Xmin '
                XMINF=READF_B(CXMINF)
                WRITE(77,*) XMINF,'# Xmin'
                WRITE(*,100) 'Xmax '
                XMAXF=READF_B(CXMAXF)
                WRITE(77,*) XMAXF,'# Xmax'
                WRITE(*,100) 'Number of points '
                NDATA=READILIM_B('1000',2,NDATAMAX)
                WRITE(77,111) NDATA,'# Number of points'
                NDATABUFF(NB)=NDATA
                DO I=1,NDATABUFF(NB)
                  XDATA(I,NB)=XMINF+(XMAXF-XMINF)*REAL(I-1)/
     +             REAL(NDATABUFF(NB)-1)
                END DO
              ELSE !ICSAVE=2
                NDATABUFF(NB)=NF
                DO I=1,NDATABUFF(NB)
                  XDATA(I,NB)=XF(I)
                END DO
              END IF
              IF(IOPC.EQ.1)THEN !...................................pseudofit
                DO I=1,NDATABUFF(NB)
                  YDATA(I,NB)=FPOLY(NTERMS-1,A,XDATA(I,NB))
                END DO
              ELSEIF(IOPC.EQ.2)THEN !........................adaptive splines
                I0SPL=1 !comenzar buscando en el inicio de la tabla
                DO I=1,NDATABUFF(NB)
                  CALL CUBSPLX(XKNOT,YKNOT,ASPL,BSPL,CSPL,NKNOTS,I0SPL,
     +             XDATA(I,NB),YDATA(I,NB))
                END DO
              ELSE
                WRITE(*,100) 'IOPC='
                WRITE(*,*) IOPC
                STOP 'FATAL ERROR in subroutine otherfit.f'
              END IF
              DO I=1,NDATABUFF(NB)
                EXDATA(I,NB)=0.
                EYDATA(I,NB)=0.
              END DO
              LXERR(NB)=.FALSE.
              LYERR(NB)=.FALSE.
              CALL UPDATELIMITS(NB)
              NSYMBBUFF(NB)=1001 !linea continua
              LDEFBUFF(NB)=.TRUE.
              LUSEBUFF(NB)=.TRUE.
              WRITE(*,100) 'Key label '
              IF(IOPC.EQ.1)THEN
                DATAKEY_(1:50)=READC_B('pseudofit','@')
              ELSEIF(IOPC.EQ.2)THEN
                DATAKEY_(1:50)=READC_B('adaptive splines','@')
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
