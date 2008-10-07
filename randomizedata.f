C
C******************************************************************************
C Genera una version de los datos en NB1 aleatorizados (usando las
C incertidumbres correspondientes) y los guarda en NB2
        SUBROUTINE RANDOMIZEDATA(NB1,NB2)
        IMPLICIT NONE
        INTEGER NB1,NB2
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
        INTEGER READI_B
        REAL RANRED
        CHARACTER*255 READC_B
C
        REAL PI
        PARAMETER (  PI=3.14159265)
        REAL SQR2
        PARAMETER (SQR2=1.41421356)
C
        INTEGER I
        INTEGER L1,L2
        INTEGER NSEED
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        REAL XDATA(NDATAMAX,NBUFFMAX),EXDATA(NDATAMAX,NBUFFMAX)
        REAL YDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
        REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
        REAL RAN1,RAN2
        CHARACTER*1 CAXIS
        CHARACTER*20 XYNAME(NDATAMAX,NBUFFMAX)
        CHARACTER*50 DATAKEY(NBUFFMAX)
        LOGICAL LRANX,LRANY
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LXYNAME(NBUFFMAX)
C
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKLXYERR/LXERR,LYERR
        COMMON/BLKXYNAME/XYNAME
        COMMON/BLKLXYNAME/LXYNAME
        COMMON/BLKSETTINGS8B/DATAKEY
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKBUFFERS3/NSYMBBUFF,LWBUFF
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
C------------------------------------------------------------------------------
        NDATABUFF(NB2)=NDATABUFF(NB1)
        L1=TRUEBEG(DATAKEY(NB1))
        L2=TRUELEN(DATAKEY(NB2))
        IF(L2.GT.0)THEN
          DATAKEY(NB2)=DATAKEY(NB1)(L1:L2)//' [randomized]'
        ELSE
          DATAKEY(NB2)='[randomized]'
        END IF
C
        WRITE(*,100) 'NSEED, negative to call srand(time()) '
        NSEED=READI_B('-1')
        WRITE(77,111) NSEED,'# NSEED for random numbers'
C
        WRITE(*,100) 'Randomize x, y or both (x/y/b) '
        CAXIS(1:1)=READC_B('b','xyb')
        WRITE(77,112) CAXIS,'# Randomize x, y or both (x/y/b)'
        LRANX=(CAXIS.EQ.'x').OR.(CAXIS.EQ.'b')
        LRANY=(CAXIS.EQ.'y').OR.(CAXIS.EQ.'b')
C
        DO I=1,NDATABUFF(NB1)
          IF(LRANX)THEN
            RAN1=RANRED(NSEED)
            RAN2=RANRED(NSEED)
            XDATA(I,NB2)=XDATA(I,NB1)+
     +       SQR2*EXDATA(I,NB1)*SQRT(-LOG(1.-RAN1))*COS(2.*PI*RAN2)
          ELSE
            XDATA(I,NB2)=XDATA(I,NB1)
          END IF
          IF(LRANY)THEN
            RAN1=RANRED(NSEED)
            RAN2=RANRED(NSEED)
            YDATA(I,NB2)=YDATA(I,NB1)+
     +       SQR2*EYDATA(I,NB1)*SQRT(-LOG(1.-RAN1))*COS(2.*PI*RAN2)
          ELSE
            YDATA(I,NB2)=YDATA(I,NB1)
          END IF
          EXDATA(I,NB2)=EXDATA(I,NB1)
          EYDATA(I,NB2)=EYDATA(I,NB1)
          XYNAME(I,NB2)=XYNAME(I,NB1)
        END DO
C
        LXERR(NB2)=.FALSE.  !por defecto para tener plots mas claros........OJO
        LYERR(NB2)=.FALSE.  !por defecto para tener plots mas claros........OJO
        CALL UPDATELIMITS(NB2)
        LDEFBUFF(NB2)=LDEFBUFF(NB1)
        LUSEBUFF(NB2)=LUSEBUFF(NB1)
        LXYNAME(NB2)=LXYNAME(NB1)
        LWBUFF(NB2)=LWBUFF(NB1)
        NSYMBBUFF(NB2)=NSYMBBUFF(NB1)
C
100     FORMAT(A,$)
111     FORMAT(I12,1X,A)
112     FORMAT(11X,A1,1X,A)
        END
