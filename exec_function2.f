C
C******************************************************************************
C Ejecuta una funcion y=f(x), con x entre Xmin y Xmax, y calculada con un 
C numero de puntos definido. Coloca la nueva informacion en el buffer NB.
C ISTATUS retorna 0 si falla algo y 1 si funciona bien.
        SUBROUTINE EXEC_FUNCTION2(ISTATUS,NB)
        IMPLICIT NONE
        INTEGER ISTATUS
C
        INCLUDE 'fcompil.inc'
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER SYSTEMFUNCTION
        INTEGER TRUEBEG,TRUELEN
        INTEGER READILIM_B
        REAL READF_B
        CHARACTER*255 READC_B
C
        INTEGER I,NB
        INTEGER NF
        INTEGER L,L1,L2,NC
        INTEGER LF1,LF2
        INTEGER LND1,LND2
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER ISYSTEM
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        INTEGER IEXPAND
        REAL XDATA(NDATAMAX,NBUFFMAX),EXDATA(NDATAMAX,NBUFFMAX)
        REAL YDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
        REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
        REAL XMIN,XMAX,YMIN,YMAX
        REAL XMIN0,XMAX0
        REAL XMINF,XMAXF
        CHARACTER*1 CNEW
        CHARACTER*50 CNDATABUFF
        CHARACTER*50 CXMINF,CXMAXF
        CHARACTER*255 CFUNCTIONY
        CHARACTER*255 CFUNCTIONEX
        CHARACTER*255 CFUNCTIONEY
        CHARACTER*255 FCOMPIL_
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LBUFFER
        LOGICAL LBATCH
C
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKBUFFERS3/NSYMBBUFF,LWBUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKLXYERR/LXERR,LYERR
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
        COMMON/BLKSETTINGS1/XMIN,XMAX,YMIN,YMAX
        COMMON/BLKSETTINGS2/LBUFFER
        COMMON/BLKSETTINGS7/IEXPAND
        COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
        FCOMPIL_=FCOMPIL
        ISTATUS=0                          !salvo que se demuestre lo contrario
        WRITE(*,100) 'Function:  y='
        CFUNCTIONY=READC_B('@','@')
        CALL TOLOG77_STRING(CFUNCTIONY(1:TRUELEN(CFUNCTIONY)),
     +   'Function: y=?')
        WRITE(*,100) 'Function: ex='
        CFUNCTIONEX=READC_B('@','@')
        CALL TOLOG77_STRING(CFUNCTIONEX(1:TRUELEN(CFUNCTIONEX)),
     +   'Function: ex=?')
        WRITE(*,100) 'Function: ey='
        CFUNCTIONEY=READC_B('@','@')
        CALL TOLOG77_STRING(CFUNCTIONEY(1:TRUELEN(CFUNCTIONEY)),
     +   'Function: ey=?')
C------------------------------------------------------------------------------
        IF(LBUFFER)THEN
          XMIN0=(1.+REAL(IEXPAND)/100.)*XMIN+REAL(IEXPAND)/100.*XMAX
          XMIN0=XMIN0/(1.+2.*REAL(IEXPAND)/100.)
          XMAX0=(1.+REAL(IEXPAND)/100.)*XMAX+REAL(IEXPAND)/100.*XMIN
          XMAX0=XMAX0/(1.+2.*REAL(IEXPAND)/100.)
          IF(XMIN.LT.XMIN0)THEN
            WRITE(CXMINF,*) XMIN0
          ELSE
            WRITE(CXMINF,*) XMIN
          END IF
          WRITE(*,100) 'Xmin '
          XMINF=READF_B(CXMINF)
          WRITE(77,*) XMINF,'# Xmin'
          IF(XMAX.GT.XMAX0)THEN
            WRITE(CXMAXF,*) XMAX0
          ELSE
            WRITE(CXMAXF,*) XMAX
          END IF
          WRITE(*,100) 'Xmax '
          XMAXF=READF_B(CXMAXF)
          WRITE(77,*) XMAXF,'# Xmax'
        ELSE
          WRITE(*,100) 'Xmin'
          XMINF=READF_B('@')
          WRITE(77,*) XMINF,'# Xmin'
          WRITE(*,100) 'Xmax'
          XMAXF=READF_B('@')
          WRITE(77,*) XMAXF,'# Xmax'
        END IF
        WRITE(*,100) 'No. of points '
        NF=READILIM_B('1000',2,NDATAMAX)
        WRITE(77,111) NF,'# No. of points'
        WRITE(CNDATABUFF,*) NF
        LND1=TRUEBEG(CNDATABUFF)
        LND2=TRUELEN(CNDATABUFF)
        WRITE(CXMINF,*) XMINF
        WRITE(CXMAXF,*) XMAXF
C------------------------------------------------------------------------------
C creamos el fichero con la funcion
        OPEN(10,FILE='funct_xpgp.f',STATUS='UNKNOWN',
     +   FORM='FORMATTED')
        WRITE(10,101) '       PROGRAM FUNCT_NCLPLOT'
        WRITE(10,101) '       IMPLICIT NONE'
        WRITE(10,101) 'C'
        WRITE(10,101) '       INTEGER I,N'
        WRITE(10,101) '       REAL XMIN,XMAX'
        WRITE(10,101) '       REAL X,Y,EX,EY'
        WRITE(10,101) 'C'
        WRITE(10,101) '       N='//CNDATABUFF(LND1:LND2)
        WRITE(10,100) '       XMIN='
        WRITE(10,101) CXMINF(TRUEBEG(CXMINF):TRUELEN(CXMINF))
        WRITE(10,100) '       XMAX='
        WRITE(10,101) CXMAXF(TRUEBEG(CXMAXF):TRUELEN(CXMAXF))
        WRITE(10,101) 'C'
        WRITE(10,101) '       OPEN(20,FILE='//CHAR(39)//
     +   'onebuffer_xpgp.dat'//CHAR(39)//','
        WRITE(10,101) '     +  STATUS='//CHAR(39)//'UNKNOWN'//CHAR(39)
        WRITE(10,101) '     +  ,FORM='//CHAR(39)//'UNFORMATTED'//
     +   CHAR(39)//')'
        WRITE(10,101) '       DO I=1,N'
        WRITE(10,101) '         X=XMIN+(XMAX-XMIN)*REAL(I-1)/REAL(N-1)'
C..............................................................................
C insertamos funcion y
        L1=TRUEBEG(CFUNCTIONY)
        L2=TRUELEN(CFUNCTIONY)
        NC=2
        WRITE(10,100) '       y='
c
        L=L1
        DO WHILE(L.LE.L2)
          CNEW=CFUNCTIONY(L:L)
          WRITE(10,100) CNEW
          NC=NC+1
          IF((NC.EQ.65).AND.(L.NE.L2))THEN !nueva linea
            WRITE(10,*) !salto de linea
            NC=0
            WRITE(10,100) '     > ' 
          END IF
          L=L+1
        END DO
        WRITE(10,*)
C..............................................................................
C insertamos funcion ex
        L1=TRUEBEG(CFUNCTIONEX)
        L2=TRUELEN(CFUNCTIONEX)
        NC=3
        WRITE(10,100) '       ex='
c
        L=L1
        DO WHILE(L.LE.L2)
          CNEW=CFUNCTIONEX(L:L)
          WRITE(10,100) CNEW
          NC=NC+1
          IF((NC.EQ.65).AND.(L.NE.L2))THEN !nueva linea
            WRITE(10,*) !salto de linea
            NC=0
            WRITE(10,100) '     > ' 
          END IF
          L=L+1
        END DO
        WRITE(10,*)
C..............................................................................
C insertamos funcion ey
        L1=TRUEBEG(CFUNCTIONEY)
        L2=TRUELEN(CFUNCTIONEY)
        NC=3
        WRITE(10,100) '       ey='
c
        L=L1
        DO WHILE(L.LE.L2)
          CNEW=CFUNCTIONEY(L:L)
          WRITE(10,100) CNEW
          NC=NC+1
          IF((NC.EQ.65).AND.(L.NE.L2))THEN !nueva linea
            WRITE(10,*) !salto de linea
            NC=0
            WRITE(10,100) '     > ' 
          END IF
          L=L+1
        END DO
        WRITE(10,*)
C..............................................................................
        WRITE(10,101) '         WRITE(20) X,Y,EX,EY'
        WRITE(10,101) '       END DO'
        WRITE(10,101) '       CLOSE(20)'
        WRITE(10,101) 'C'
        WRITE(10,101) '       STOP'
        WRITE(10,101) '       END'
        CLOSE(10)
C------------------------------------------------------------------------------
C compilamos el fichero con la funcion
        LF1=TRUEBEG(FCOMPIL_)
        LF2=TRUELEN(FCOMPIL_)
        ISYSTEM=SYSTEMFUNCTION(FCOMPIL_(LF1:LF2)//
     +   ' -o funct_xpgp funct_xpgp.f')
        IF((ISYSTEM.EQ.127.OR.ISYSTEM.EQ.-1))THEN
          WRITE(*,101) 'ERROR: while calling system function.'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          GOTO 90
        ELSE
          WRITE(*,101) 'INFO: function program was compiled'
        END IF
C------------------------------------------------------------------------------
C ejecutamos el fichero con la funcion
        ISYSTEM=SYSTEMFUNCTION('./funct_xpgp')
        IF((ISYSTEM.EQ.127.OR.ISYSTEM.EQ.-1))THEN
          WRITE(*,101) 'ERROR: while calling executing function.'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          GOTO 90
        ELSE
          WRITE(*,101) 'INFO: function program was executed'
        END IF
C------------------------------------------------------------------------------
C leemos el fichero con todos los datos de los buffers
        OPEN(20,FILE='onebuffer_xpgp.dat',STATUS='OLD',
     +   FORM='UNFORMATTED')
        DO I=1,NF
          READ(20) XDATA(I,NB),YDATA(I,NB),EXDATA(I,NB),EYDATA(I,NB)
        END DO
        CLOSE(20)
        WRITE(*,101) 'INFO: buffer data were updated'
C------------------------------------------------------------------------------
C borramos el fichero con los datos del buffer
90      ISYSTEM=SYSTEMFUNCTION('rm -f onebuffer_xpgp.dat')
        IF((ISYSTEM.EQ.127.OR.ISYSTEM.EQ.-1))THEN
          WRITE(*,101) 'ERROR: while removing onebuffer_xpgp.dat.'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
        END IF
C------------------------------------------------------------------------------
C borramos el fichero fortran y su ejecutable
        ISYSTEM=SYSTEMFUNCTION('rm -f funct_xpgp.f funct_xpgp')
        IF((ISYSTEM.EQ.127.OR.ISYSTEM.EQ.-1))THEN
          WRITE(*,101) 'ERROR: while removing funct_xpgp*'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
        END IF
C------------------------------------------------------------------------------
        NDATABUFF(NB)=NF
        LXERR(NB)=.FALSE.
        LYERR(NB)=.FALSE.
        CALL UPDATELIMITS(NB)
        NSYMBBUFF(NB)=1001 !linea continua
        ISTATUS=1
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
        END
