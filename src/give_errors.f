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
C Muestra una sencilla estadistica de los errores en el buffer NB.
C La rutina tambien comprueba si los errores en algun eje contienen ceros. Si
C es asi, las variables LERRXNULL y LERRYNULL retornan .TRUE.
        SUBROUTINE GIVE_ERRORS(NB,LERRXNULL,LERRYNULL)
        IMPLICIT NONE
        INTEGER NB
        LOGICAL LERRXNULL,LERRYNULL
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER I
        INTEGER NDATABUFF(NBUFFMAX)
        REAL EXDATA(NDATAMAX,NBUFFMAX)
        REAL EYDATA(NDATAMAX,NBUFFMAX)
        REAL EXMEAN,EYMEAN
        REAL EXMIN,EYMIN
        REAL EXMAX,EYMAX
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
C
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKLXYERR/LXERR,LYERR
C------------------------------------------------------------------------------
        LERRXNULL=.FALSE.
        LERRYNULL=.FALSE.
        DO I=1,NDATABUFF(NB)
          IF(EXDATA(I,NB).LE.0.0) LERRXNULL=.TRUE.
          IF(EYDATA(I,NB).LE.0.0) LERRYNULL=.TRUE.
        END DO
C..............................................................................
        EXMEAN=0.
        EYMEAN=0.
        DO I=1,NDATABUFF(NB)
          EXMEAN=EXMEAN+EXDATA(I,NB)
          EYMEAN=EYMEAN+EYDATA(I,NB)
        END DO
        EXMEAN=EXMEAN/REAL(NDATABUFF(NB))
        EYMEAN=EYMEAN/REAL(NDATABUFF(NB))
C..............................................................................
        EXMIN=EXDATA(1,NB)
        EXMAX=EXMIN
        EYMIN=EYDATA(1,NB)
        EYMAX=EYMIN
        IF(NDATABUFF(NB).GT.1)THEN
          DO I=2,NDATABUFF(NB)
            IF(EXMIN.GT.EXDATA(I,NB)) EXMIN=EXDATA(I,NB)
            IF(EXMAX.LT.EXDATA(I,NB)) EXMAX=EXDATA(I,NB)
            IF(EYMIN.GT.EYDATA(I,NB)) EYMIN=EYDATA(I,NB)
            IF(EYMAX.LT.EYDATA(I,NB)) EYMAX=EYDATA(I,NB)
          END DO
        END IF
C..............................................................................
        WRITE(*,'(A,I2,A)') '* Buffer #',NB,':'
C..............................................................................
C valores medios
        WRITE(*,100) '>>> EXmean...............: '
        WRITE(*,*) EXMEAN
        WRITE(*,100) '>>> EYmean...............: '
        WRITE(*,*) EYMEAN
C..............................................................................
C numero de puntos y extremos
        WRITE(*,100) '>>> No. of data in buffer: '
        WRITE(*,*) NDATABUFF(NB)
        WRITE(*,100) '>>> EXmin, EXmax.........: '
        WRITE(*,*) EXMIN,EXMAX
        WRITE(*,100) '>>> EYmin, EYmax.........: '
        WRITE(*,*) EYMIN,EYMAX
C..............................................................................
100     FORMAT(A,$)
        END
