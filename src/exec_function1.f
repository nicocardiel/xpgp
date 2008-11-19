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
C Ejecuta una funcion f(Xij,Yij).
C ISTATUS retorna 0 si falla algo, y 1 si funciona bien.
        SUBROUTINE EXEC_FUNCTION1(ISTATUS)
        IMPLICIT NONE
        INTEGER ISTATUS
C
        INCLUDE 'fcompil.inc'
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER SYSTEMFUNCTION
        INTEGER TRUEBEG,TRUELEN
        CHARACTER*255 READC_B
C
        INTEGER I,NB,NB_,NBNEW
        INTEGER L,L1,L2,NC
        INTEGER LF1,LF2
        INTEGER LND1,LND2
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER ISYSTEM
        REAL XDATA(NDATAMAX,NBUFFMAX),EXDATA(NDATAMAX,NBUFFMAX)
        REAL YDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
        REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
        CHARACTER*1 CNEW,CNEXT
        CHARACTER*50 CNDATABUFF
        CHARACTER*255 CFUNCTION
        CHARACTER*255 FCOMPIL_
        LOGICAL LOK
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LBATCH
C
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKLXYERR/LXERR,LYERR
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
        COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
        FCOMPIL_=FCOMPIL
        ISTATUS=0                          !salvo que se demuestre lo contrario
        WRITE(*,100) 'Function'
        CFUNCTION=READC_B('@','@')
        CALL TOLOG77_STRING(CFUNCTION(1:TRUELEN(CFUNCTION)),
     +   'Function')
C------------------------------------------------------------------------------
        L2=INDEX(CFUNCTION,'=')
        IF(L2.LE.2)THEN
          WRITE(*,101) 'ERROR: syntax error #1 in function'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          RETURN
        END IF
        L1=INDEX(CFUNCTION(1:L2-1),'x')
        IF(L1.EQ.0)THEN
          L1=INDEX(CFUNCTION(1:L2-1),'y')
          IF(L1.EQ.0)THEN
            WRITE(*,101) 'ERROR: syntax error #2 in function'
            WRITE(*,100) 'Press <CR> to continue...'
            IF(LBATCH)THEN
              WRITE(*,*)
            ELSE
              READ(*,*)
            END IF
            RETURN
          END IF
        END IF
        NBNEW=INDEX('12345678',CFUNCTION(L1+1:L1+1))
        IF(NBNEW.EQ.0)THEN
          WRITE(*,101) 'ERROR: syntax error #3 in function'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          RETURN
        END IF
        IF(.NOT.LDEFBUFF(NBNEW))THEN
          WRITE(*,101) 'ERROR: destination buffer is undefined'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          RETURN
        END IF
        WRITE(CNDATABUFF,*) NDATABUFF(NBNEW)
        LND1=TRUEBEG(CNDATABUFF)
        LND2=TRUELEN(CNDATABUFF)
C------------------------------------------------------------------------------
C creamos el fichero con todos los datos de los buffers
        OPEN(20,FILE='allbuffers_xpgp.dat',STATUS='UNKNOWN',
     +   FORM='UNFORMATTED')
        DO NB=1,NBUFFMAX
          WRITE(20) (XDATA(I,NB),I=1,NDATABUFF(NBNEW))
          WRITE(20) (EXDATA(I,NB),I=1,NDATABUFF(NBNEW))
          WRITE(20) (YDATA(I,NB),I=1,NDATABUFF(NBNEW))
          WRITE(20) (EYDATA(I,NB),I=1,NDATABUFF(NBNEW))
        END DO
        CLOSE(20)
        WRITE(*,101) 'INFO: initial buffer data were saved'
C------------------------------------------------------------------------------
C creamos el fichero con la funcion
        OPEN(10,FILE='funct_xpgp.f',STATUS='UNKNOWN',
     +   FORM='FORMATTED')
        WRITE(10,101) '       PROGRAM FUNCT_NCLPLOT'
        WRITE(10,101) '       IMPLICIT NONE'
        WRITE(10,101) 'C'
        WRITE(10,101) '       INTEGER NBUFFMAX'
        WRITE(10,100) '       PARAMETER (NBUFFMAX='
        WRITE(10,'(I12,$)') NBUFFMAX
        WRITE(10,101) ')'
        WRITE(10,101) '       INTEGER NDATAMAX'
        WRITE(10,100) '       PARAMETER (NDATAMAX='
        WRITE(10,'(I12,$)') NDATAMAX
        WRITE(10,101) ')'
        WRITE(10,101) 'C'
        WRITE(10,101) '       REAL XDATA(NDATAMAX,NBUFFMAX)'
        WRITE(10,101) '       REAL EXDATA(NDATAMAX,NBUFFMAX)'
        WRITE(10,101) '       REAL YDATA(NDATAMAX,NBUFFMAX)'
        WRITE(10,101) '       REAL EYDATA(NDATAMAX,NBUFFMAX)'
        WRITE(10,101) 'C'
        WRITE(10,101) '       INTEGER I,NB'
        WRITE(10,101) 'C'
        WRITE(10,101) '       OPEN(10,FILE='//CHAR(39)//
     +   'allbuffers_xpgp.dat'//CHAR(39)//','
        WRITE(10,101) '     +  STATUS='//CHAR(39)//'OLD'//CHAR(39)//','
        WRITE(10,101) '     +  FORM='//CHAR(39)//'UNFORMATTED'//
     +   CHAR(39)//')'
        WRITE(10,101) '       DO NB=1,NBUFFMAX'
        WRITE(10,100) '         READ(10) (XDATA(I,NB),I=1,'
        WRITE(10,100) CNDATABUFF(LND1:LND2)
        WRITE(10,101) ')'
        WRITE(10,100) '         READ(10) (EXDATA(I,NB),I=1,'
        WRITE(10,100) CNDATABUFF(LND1:LND2)
        WRITE(10,101) ')'
        WRITE(10,100) '         READ(10) (YDATA(I,NB),I=1,'
        WRITE(10,100) CNDATABUFF(LND1:LND2)
        WRITE(10,101) ')'
        WRITE(10,100) '         READ(10) (EYDATA(I,NB),I=1,'
        WRITE(10,100) CNDATABUFF(LND1:LND2)
        WRITE(10,101) ')'
        WRITE(10,101) '       END DO'
        WRITE(10,101) '       CLOSE(10)'
        WRITE(10,101) 'C'
        WRITE(10,100) '       DO I=1,'
        WRITE(10,101) CNDATABUFF(LND1:LND2)
C..............................................................................
C insertamos funcion
        L1=TRUEBEG(CFUNCTION)
        L2=TRUELEN(CFUNCTION)
        NC=0
        WRITE(10,100) '       '
c
        L=L1
        DO WHILE(L.LE.L2)
          CNEW=CFUNCTION(L:L)
          IF(CNEW.EQ.'e')THEN            !chequeamos si la variable es de error
            IF(L.GE.L2-2)THEN
              CNEXT=CFUNCTION(L+1:L+1)
              IF((CNEW.EQ.'x').OR.(CNEW.EQ.'y'))THEN
                CNEXT=CFUNCTION(L+2:L+2)
                NB_=INDEX('12345678',CNEXT)
                IF(NB_.NE.0)THEN
                  WRITE(10,100) CNEW
                  NC=NC+1
                  IF(NC.EQ.65)THEN !nueva linea
                    WRITE(10,*) !salto de linea
                    NC=0
                    WRITE(10,100) '     > ' 
                  END IF
                  L=L+1
                  CNEW=CFUNCTION(L:L)
                END IF
              END IF
            END IF
          END IF
          IF((CNEW.EQ.'x').OR.(CNEW.EQ.'y'))THEN
            IF(L.EQ.L2)THEN !si es el final de la cadena
              LOK=.TRUE.
            ELSE
              CNEXT=CFUNCTION(L+1:L+1)
              NB_=INDEX('12345678',CNEXT)
              IF(NB_.EQ.0)THEN
                LOK=.TRUE.
              ELSE
                LOK=.FALSE.
                WRITE(10,100) CNEW
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
                WRITE(10,100) 'd'
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
                WRITE(10,100) 'a'
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
                WRITE(10,100) 't'
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
                WRITE(10,100) 'a'
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
                WRITE(10,100) '('
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
                WRITE(10,100) 'i'
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
                WRITE(10,100) ','
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
                WRITE(10,100) CNEXT
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
                WRITE(10,100) ')'
                NC=NC+1
                IF(NC.EQ.65)THEN !nueva linea
                  WRITE(10,*) !salto de linea
                  NC=0
                  WRITE(10,100) '     > ' 
                END IF
              END IF
            END IF
          ELSE
            LOK=.TRUE.
          END IF
c
          IF(LOK)THEN
            WRITE(10,100) CNEW
            NC=NC+1
            IF((NC.EQ.65).AND.(L.NE.L2))THEN !nueva linea
              WRITE(10,*) !salto de linea
              NC=0
              WRITE(10,100) '     > ' 
            END IF
            L=L+1
          ELSE
            L=L+2
          END IF
c
        END DO
        WRITE(10,*)
C..............................................................................
        WRITE(10,101) '       END DO'
        WRITE(10,101) 'C'
        WRITE(10,101) '       OPEN(20,FILE='//CHAR(39)//
     +   'allbuffers_xpgp.dat'//CHAR(39)//','
        WRITE(10,101) '     +  STATUS='//CHAR(39)//'UNKNOWN'//CHAR(39)
        WRITE(10,101) '     +  ,FORM='//CHAR(39)//'UNFORMATTED'//
     +   CHAR(39)//')'
        WRITE(10,101) '       DO NB=1,NBUFFMAX'
        WRITE(10,100) '         WRITE(20) (XDATA(I,NB),I=1,'
        WRITE(10,100) CNDATABUFF(LND1:LND2)
        WRITE(10,101) ')'
        WRITE(10,100) '         WRITE(20) (EXDATA(I,NB),I=1,'
        WRITE(10,100) CNDATABUFF(LND1:LND2)
        WRITE(10,101) ')'
        WRITE(10,100) '         WRITE(20) (YDATA(I,NB),I=1,'
        WRITE(10,100) CNDATABUFF(LND1:LND2)
        WRITE(10,101) ')'
        WRITE(10,100) '         WRITE(20) (EYDATA(I,NB),I=1,'
        WRITE(10,100) CNDATABUFF(LND1:LND2)
        WRITE(10,101) ')'
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
        OPEN(20,FILE='allbuffers_xpgp.dat',STATUS='OLD',
     +   FORM='UNFORMATTED')
        DO NB=1,NBUFFMAX
          READ(20) (XDATA(I,NB),I=1,NDATABUFF(NBNEW))
          READ(20) (EXDATA(I,NB),I=1,NDATABUFF(NBNEW))
          READ(20) (YDATA(I,NB),I=1,NDATABUFF(NBNEW))
          READ(20) (EYDATA(I,NB),I=1,NDATABUFF(NBNEW))
        END DO
        CLOSE(20)
        WRITE(*,101) 'INFO: buffer data were updated'
C------------------------------------------------------------------------------
C borramos el fichero con los datos de los buffers
90      ISYSTEM=SYSTEMFUNCTION('rm -f allbuffers_xpgp.dat')
        IF((ISYSTEM.EQ.127.OR.ISYSTEM.EQ.-1))THEN
          WRITE(*,101) 'ERROR: while removing allbuffers_xpgp.dat.'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
        ELSE
          WRITE(*,101) 'INFO: file allbuffers_xpgp.dat was removed'
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
        CALL UPDATELIMITS(NBNEW)
        ISTATUS=1
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
