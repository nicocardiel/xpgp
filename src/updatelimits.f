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
C Actualiza los limites minimo y maximo para un buffer concreto
        SUBROUTINE UPDATELIMITS(NB)
        IMPLICIT NONE
        INTEGER NB
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER I
        INTEGER NDATABUFF(NBUFFMAX)
        REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
        REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
        REAL XDATA(NDATAMAX,NBUFFMAX),YDATA(NDATAMAX,NBUFFMAX)
        REAL EXDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
C
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
C------------------------------------------------------------------------------
        XMINBUFF(NB)=XDATA(1,NB)-EXDATA(1,NB)
        XMAXBUFF(NB)=XDATA(1,NB)+EXDATA(1,NB)
        YMINBUFF(NB)=YDATA(1,NB)-EYDATA(1,NB)
        YMAXBUFF(NB)=YDATA(1,NB)+EYDATA(1,NB)
        IF(NDATABUFF(NB).GT.1)THEN
          DO I=2,NDATABUFF(NB)
            IF(XMINBUFF(NB).GT.XDATA(I,NB)-EXDATA(I,NB))
     +       XMINBUFF(NB)=XDATA(I,NB)-EXDATA(I,NB)
            IF(XMAXBUFF(NB).LT.XDATA(I,NB)+EXDATA(I,NB))
     +       XMAXBUFF(NB)=XDATA(I,NB)+EXDATA(I,NB)
            IF(YMINBUFF(NB).GT.YDATA(I,NB)-EYDATA(I,NB))
     +       YMINBUFF(NB)=YDATA(I,NB)-EYDATA(I,NB)
            IF(YMAXBUFF(NB).LT.YDATA(I,NB)+EYDATA(I,NB))
     +       YMAXBUFF(NB)=YDATA(I,NB)+EYDATA(I,NB)
          END DO
        END IF
C
        END
