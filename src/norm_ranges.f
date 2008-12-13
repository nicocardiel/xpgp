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
C Normaliza los recorridos de las variables al intervalo [-1,+1]
        SUBROUTINE NORM_RANGES(NB0)
        IMPLICIT NONE
        INTEGER NB0
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER I
        INTEGER NDATABUFF(NBUFFMAX)
        REAL XDATA(NDATAMAX,NBUFFMAX),YDATA(NDATAMAX,NBUFFMAX)
        REAL EXDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XMIN,XMAX,YMIN,YMAX
        REAL BX,CX,BY,CY
C
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
C------------------------------------------------------------------------------
C limites de los datos
        XMIN=XDATA(1,NB0)
        XMAX=XMIN
        YMIN=YDATA(1,NB0)
        YMAX=YMIN
C
        DO I=1,NDATABUFF(NB0)
          XMIN=AMIN1(XDATA(I,NB0),XMIN)
          XMAX=AMAX1(XDATA(I,NB0),XMAX)
          YMIN=AMIN1(YDATA(I,NB0),YMIN)
          YMAX=AMAX1(YDATA(I,NB0),YMAX)
        END DO
C coeficientes de la transformacion en X
        IF(XMIN.NE.XMAX)THEN
          BX=2./(XMAX-XMIN)
          CX=(XMIN+XMAX)/(XMAX-XMIN)
        ELSE
          BX=1.0
          CX=0.0
        END IF
C coeficientes de la transformacion en Y
        IF(YMIN.NE.YMAX)THEN
          BY=2./(YMAX-YMIN)
          CY=(YMIN+YMAX)/(YMAX-YMIN)
        ELSE
          BY=1.0
          CY=0.0
        END IF
C normalizamos recorridos
        DO I=1,NDATABUFF(NB0)
          XDATA(I,NB0)=BX*XDATA(I,NB0)-CX
          EXDATA(I,NB0)=BX*EXDATA(I,NB0)
          YDATA(I,NB0)=BY*YDATA(I,NB0)-CY
          EYDATA(I,NB0)=BY*EYDATA(I,NB0)
        END DO
C
        CALL UPDATELIMITS(NB0)
        CALL SHOW_BUFFERS
C------------------------------------------------------------------------------
        END
