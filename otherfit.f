        SUBROUTINE OTHERFIT(NB0)
        IMPLICIT NONE
        INTEGER NB0
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER NDEGMAX
        PARAMETER (NDEGMAX=16)
        INTEGER NPLOTMAX
        PARAMETER (NPLOTMAX=1000)
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER READILIM_B
        REAL READF_B
        REAL FPOLY
        CHARACTER*255 READC_B
C
        INTEGER I
        INTEGER IOPC
        INTEGER NTERMS
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER NDATA
        INTEGER ICSAVE
        INTEGER IEXPAND
        INTEGER NF
        INTEGER NB
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        INTEGER L1,L2
        INTEGER ILUP
        REAL YRMSTOL
        REAL WEIGHT
        REAL POWER
        REAL A(NDEGMAX+1)
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
        WRITE(*,*)
        WRITE(*,101) '(1) upper/lower boundary'
        WRITE(*,101) '(0) EXIT'
        WRITE(*,100) 'Option '
        IOPC=READILIM_B('0',0,1)
        WRITE(77,111) IOPC,'# 1=upper/lower boundary, 0=EXIT'
        IF(IOPC.EQ.0) RETURN
C------------------------------------------------------------------------------
        NF=NDATABUFF(NB0)
C------------------------------------------------------------------------------
        IF(IOPC.EQ.1)THEN
          WRITE(*,100) 'Polynomial degree'
          NTERMS=READILIM_B('@',0,10)
          WRITE(77,111) NTERMS,'# Polynomial degree'
          NTERMS=NTERMS+1
          WRITE(*,100) 'YRMSTOL for DOWNHILL '
          YRMSTOL=READF_B('1E-5')
          WRITE(77,*) YRMSTOL,'# YRMSTOL for DOWNHILL'
          WRITE(*,100) 'WEIGHT for pseudofit '
          WEIGHT=READF_B('100')
          WRITE(77,*) WEIGHT,'# WEIGHT for pseudofit'
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
          DO I=1,NF
            XF(I)=XDATA(I,NB0)
            YF(I)=YDATA(I,NB0)
            EYF(I)=EYDATA(I,NB0)
          END DO
          !realizamos el ajuste
          CALL PSEUDOFIT(XF,YF,EYF,NF,NTERMS,YRMSTOL,WEIGHT,POWER,
     +     LUP,TSIGMA,A)
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
            YP(I)=FPOLY(NTERMS-1,A,XP(I))
          END DO
          CALL PGSCI(0)
          CALL PGLINE(NPLOTMAX,XP,YP)
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
     +         '# 1=save last fit, 2=save fit predictions, 0=EXIT'
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
     +               REAL(NDATABUFF(NB)-1)
                  END DO
                ELSE !ICSAVE=2
                  NDATABUFF(NB)=NF
                  DO I=1,NDATABUFF(NB)
                    XDATA(I,NB)=XF(I)
                  END DO
                END IF
                DO I=1,NDATABUFF(NB)
                  YDATA(I,NB)=FPOLY(NTERMS-1,A,XDATA(I,NB))
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
                DATAKEY_(1:50)=READC_B('pseudofit','@')
                DATAKEY(NB)=DATAKEY_
                L1=TRUEBEG(DATAKEY(NB))
                L2=TRUELEN(DATAKEY(NB))
                CALL TOLOG77_STRING(DATAKEY(NB)(L1:L2),'Key label')
                CALL SHOW_BUFFERS
              END IF
            END DO
          END IF
        END IF
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
112     FORMAT(11X,A1,1X,A)
        END
