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
C Editor de datos
        SUBROUTINE EDITOR(NB0,IMODE)
        IMPLICIT NONE
        INTEGER NB0
        INTEGER IMODE
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER TRUEBEG,TRUELEN
        REAL READF_B
        CHARACTER*255 READC_B
C
        INTEGER I,II
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER IMIN
        REAL XDATA(NDATAMAX,NBUFFMAX),YDATA(NDATAMAX,NBUFFMAX)
        REAL EXDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL DMIN,DNEW
        REAL XC,YC,XC_,YC_,XC0,YC0
        CHARACTER*1 CH
        CHARACTER*20 XYNAME(NDATAMAX,NBUFFMAX),XYNAME_
        CHARACTER*50 CDUMMY
        CHARACTER*255 CLINEA
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LXYNAME(NBUFFMAX)
        LOGICAL LBATCH
        LOGICAL LOOP
C
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKXYNAME/XYNAME
        COMMON/BLKLXYNAME/LXYNAME
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
        IF((IMODE.EQ.22).OR.(IMODE.EQ.30))THEN           !change/remove 1 point
          WRITE(*,*)
          WRITE(*,100) 'Select point with the mouse...'
          IF(LBATCH)THEN
            LOOP=.TRUE.
            DO WHILE(LOOP)
              READ(78,101) CLINEA
              IF(CLINEA(1:1).NE.'#') LOOP=.FALSE.
            END DO
            READ(CLINEA,*) XC,YC
          ELSE
            CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
          END IF
          WRITE(77,*) XC,YC,'# point selected with the mouse'
          IMIN=1
          DMIN=(XDATA(1,NB0)-XC)**2+(YDATA(1,NB0)-YC)**2
          IF(NDATABUFF(NB0).GT.1)THEN
            DO I=2,NDATABUFF(NB0)
              DNEW=(XDATA(I,NB0)-XC)**2+(YDATA(I,NB0)-YC)**2
              IF(DNEW.LT.DMIN)THEN
                DMIN=DNEW
                IMIN=I
              END IF
            END DO
          END IF
          WRITE(*,*)
          WRITE(*,100) 'Nearest point is #'
          WRITE(CDUMMY,*) IMIN
          WRITE(*,100) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          WRITE(*,100) ' at ('
          WRITE(CDUMMY,*) XDATA(IMIN,NB0)
          WRITE(*,100) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          WRITE(*,100) ','
          WRITE(CDUMMY,*) YDATA(IMIN,NB0)
          WRITE(*,100) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          WRITE(*,101) ')'
        END IF
C------------------------------------------------------------------------------
        IF(IMODE.EQ.22)THEN                                     !change 1 point
          WRITE(CDUMMY,*) XDATA(IMIN,NB0)
          WRITE(*,100) 'New  X value '
          XDATA(IMIN,NB0)=READF_B(CDUMMY)
          WRITE(77,*) XDATA(IMIN,NB0),'# New X value'
          WRITE(CDUMMY,*) EXDATA(IMIN,NB0)
          WRITE(*,100) 'New EX value '
          EXDATA(IMIN,NB0)=READF_B(CDUMMY)
          WRITE(77,*) EXDATA(IMIN,NB0),'# New EX value'
          WRITE(CDUMMY,*) YDATA(IMIN,NB0)
          WRITE(*,100) 'New  Y value '
          YDATA(IMIN,NB0)=READF_B(CDUMMY)
          WRITE(77,*) YDATA(IMIN,NB0),'# New Y value'
          WRITE(CDUMMY,*) EYDATA(IMIN,NB0)
          WRITE(*,100) 'New EY value '
          EYDATA(IMIN,NB0)=READF_B(CDUMMY)
          WRITE(77,*) EYDATA(IMIN,NB0),'# New EY value'
          IF(LXYNAME(NB0))THEN
            WRITE(*,100) 'New NAME value '
            XYNAME_(1:20)=READC_B(XYNAME(IMIN,NB0),'@')
            XYNAME(IMIN,NB0)=XYNAME_
            CALL TOLOG77_STRING(XYNAME(IMIN,NB0),'New NAME value')
          END IF
C------------------------------------------------------------------------------
        ELSEIF(IMODE.EQ.30)THEN                                 !remove 1 point
          IF(IMIN.LT.NDATABUFF(NB0))THEN
            DO I=IMIN,NDATABUFF(NB0)-1
              XDATA(I,NB0)=XDATA(I+1,NB0)
              EXDATA(I,NB0)=EXDATA(I+1,NB0)
              YDATA(I,NB0)=YDATA(I+1,NB0)
              EYDATA(I,NB0)=EYDATA(I+1,NB0)
              IF(LXYNAME(NB0))THEN
                XYNAME(I,NB0)=XYNAME(I+1,NB0)
              END IF
            END DO
          END IF
          NDATABUFF(NB0)=NDATABUFF(NB0)-1
          IF(NDATABUFF(NB0).EQ.0)THEN
            LUSEBUFF(NB0)=.FALSE.
            LDEFBUFF(NB0)=.FALSE.
          END IF
C------------------------------------------------------------------------------
        ELSEIF(IMODE.EQ.38)THEN                                  !remove region
          WRITE(*,*)
          WRITE(*,100) 'Select rectangle with the mouse...'
          IF(LBATCH)THEN
            LOOP=.TRUE.
            DO WHILE(LOOP)
              READ(78,101) CLINEA
              IF(CLINEA(1:1).NE.'#') LOOP=.FALSE.
            END DO
            READ(CLINEA,*) XC,YC
          ELSE
            CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
          END IF
          WRITE(77,*) XC,YC,'# first corner'
          CALL PGSCI(4)
          WRITE(*,100) 'click again...'
          IF(LBATCH)THEN
            LOOP=.TRUE.
            DO WHILE(LOOP)
              READ(78,101) CLINEA
              IF(CLINEA(1:1).NE.'#') LOOP=.FALSE.
            END DO
            READ(CLINEA,*) XC0,YC0
          ELSE
            CALL RPGBAND(2,0,XC,YC,XC0,YC0,CH)
          END IF
          WRITE(77,*) XC0,YC0,'# second corner'
          WRITE(*,101) 'OK!'
          CALL PGSCI(0)
          IF(XC0.LT.XC)THEN
            XC_=XC0
            XC0=XC
            XC=XC_
          END IF
          IF(YC0.LT.YC)THEN
            YC_=YC0
            YC0=YC
            YC=YC_
          END IF
          II=0
          DO I=1,NDATABUFF(NB0)
            IF((XDATA(I,NB0).GT.XC).AND.(XDATA(I,NB0).LT.XC0).AND.
     +         (YDATA(I,NB0).GT.YC).AND.(YDATA(I,NB0).LT.YC0))THEN
            ELSE
              II=II+1
              XDATA(II,NB0)=XDATA(I,NB0)
              EXDATA(II,NB0)=EXDATA(I,NB0)
              YDATA(II,NB0)=YDATA(I,NB0)
              EYDATA(II,NB0)=EYDATA(I,NB0)
              IF(LXYNAME(NB0))THEN
                XYNAME(II,NB0)=XYNAME(I,NB0)
              END IF
            END IF
          END DO
          NDATABUFF(NB0)=II
          IF(NDATABUFF(NB0).EQ.0)THEN
            LUSEBUFF(NB0)=.FALSE.
            LDEFBUFF(NB0)=.FALSE.
          END IF
C------------------------------------------------------------------------------
        END IF
C------------------------------------------------------------------------------
        CALL UPDATELIMITS(NB0)
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
