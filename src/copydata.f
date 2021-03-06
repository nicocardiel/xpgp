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
C CAPEND='1': se sustituye todo el buffer
C CAPEND='2': se a\~{n}aden los nuevos datos a un buffer ya definido
C
        SUBROUTINE COPYDATA(NB1,NB2,CAPEND)
        IMPLICIT NONE
        INTEGER NB1,NB2
        CHARACTER*1 CAPEND
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER I,I0
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        REAL XDATA(NDATAMAX,NBUFFMAX),EXDATA(NDATAMAX,NBUFFMAX)
        REAL YDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
        REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
        CHARACTER*20 XYNAME(NDATAMAX,NBUFFMAX)
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
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKBUFFERS3/NSYMBBUFF,LWBUFF
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
C------------------------------------------------------------------------------
        IF(CAPEND.EQ.'1')THEN                           !se sustituye el buffer
          I0=0
          NDATABUFF(NB2)=NDATABUFF(NB1)
        ELSE                          !introduce el nuevo buffer a continuacion
          I0=NDATABUFF(NB2)
          NDATABUFF(NB2)=NDATABUFF(NB2)+NDATABUFF(NB1)
        END IF
C
        DO I=1,NDATABUFF(NB1)
          XDATA(I+I0,NB2)=XDATA(I,NB1)
          YDATA(I+I0,NB2)=YDATA(I,NB1)
          EXDATA(I+I0,NB2)=EXDATA(I,NB1)
          EYDATA(I+I0,NB2)=EYDATA(I,NB1)
          XYNAME(I+I0,NB2)=XYNAME(I,NB1)
        END DO
C
        IF(CAPEND.EQ.'1')THEN                           !se sustituye el buffer
          LXERR(NB2)=LXERR(NB1)
          LYERR(NB2)=LYERR(NB1)
          XMINBUFF(NB2)=XMINBUFF(NB1)
          XMAXBUFF(NB2)=XMAXBUFF(NB1)
          YMINBUFF(NB2)=YMINBUFF(NB1)
          YMAXBUFF(NB2)=YMAXBUFF(NB1)
          LDEFBUFF(NB2)=LDEFBUFF(NB1)
          LUSEBUFF(NB2)=LUSEBUFF(NB1)
          LXYNAME(NB2)=LXYNAME(NB1)
          LWBUFF(NB2)=LWBUFF(NB1)
          NSYMBBUFF(NB2)=NSYMBBUFF(NB1)
        ELSE                          !introduce el nuevo buffer a continuacion
          IF(XMINBUFF(NB1).LT.XMINBUFF(NB2)) XMINBUFF(NB2)=XMINBUFF(NB1)
          IF(XMAXBUFF(NB1).GT.XMAXBUFF(NB2)) XMAXBUFF(NB2)=XMAXBUFF(NB1)
          IF(YMINBUFF(NB1).LT.YMINBUFF(NB2)) YMINBUFF(NB2)=YMINBUFF(NB1)
          IF(YMAXBUFF(NB1).GT.YMAXBUFF(NB2)) YMAXBUFF(NB2)=YMAXBUFF(NB1)
        END IF
C
        END
