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
C Realiza un histograma con los datos del eje CAXIS del buffer NB0
        SUBROUTINE HISTOGRAM(CAXIS,NB0)
        IMPLICIT NONE
        CHARACTER*1 CAXIS
        INTEGER NB0
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
        INTEGER NBINMAX
        PARAMETER (NBINMAX=1000) !numero maximo de bins en el histograma
C
        INTEGER READILIM_B
        REAL READF_B
C
        INTEGER I,K,NBIN
        INTEGER NBNEW
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER NDATA(1:NBINMAX)
        REAL XDATA(NDATAMAX,NBUFFMAX),YDATA(NDATAMAX,NBUFFMAX)
        REAL EXDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
        REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
        REAL XMINH,XMAXH,DX
        CHARACTER*50 CDUMMY
        LOGICAL LBATCH
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LXYNAME(NBUFFMAX)
C
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKLXYERR/LXERR,LYERR
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKLBATCH/LBATCH
        COMMON/BLKLXYNAME/LXYNAME
C------------------------------------------------------------------------------
C definimos parametros del histograma necesarios
        IF(CAXIS.EQ.'X')THEN
          WRITE(CDUMMY,*) XMINBUFF(NB0)
          WRITE(*,100) 'Xmin '
        ELSE
          WRITE(CDUMMY,*) YMINBUFF(NB0)
          WRITE(*,100) 'Ymin '
        END IF
        XMINH=READF_B(CDUMMY)
        WRITE(77,*) XMINH,'# minimum value'
        IF(CAXIS.EQ.'X')THEN
          WRITE(CDUMMY,*) XMAXBUFF(NB0)
          WRITE(*,100) 'Xmax '
        ELSE
          WRITE(CDUMMY,*) YMAXBUFF(NB0)
          WRITE(*,100) 'Ymax '
        END IF
        XMAXH=READF_B(CDUMMY)
        WRITE(77,*) XMAXH,'# maximum value'
        WRITE(*,100) 'No. of bins '
        NBIN=READILIM_B('100',2,NBINMAX)
        WRITE(77,111) NBIN,'# Number of bins'
C------------------------------------------------------------------------------
        DO K=1,NBIN
          NDATA(K)=0
        END DO
C
        DX=(XMAXH-XMINH)/REAL(NBIN)
C
        IF(CAXIS.EQ.'X')THEN
          DO I=1,NDATABUFF(NB0)
            K=INT((XDATA(I,NB0)-XMINH)/DX)+1
            IF((K.GE.1).AND.(K.LE.NBIN)) NDATA(K)=NDATA(K)+1
          END DO
        ELSE
          DO I=1,NDATABUFF(NB0)
            K=INT((YDATA(I,NB0)-XMINH)/DX)+1
            IF((K.GE.1).AND.(K.LE.NBIN)) NDATA(K)=NDATA(K)+1
          END DO
        END IF
C------------------------------------------------------------------------------
        WRITE(*,100) 'Buffer # to store histogram '
        NBNEW=READILIM_B('@',1,NBUFFMAX)
        WRITE(77,111) NBNEW,'# Selected buffer number'
        WRITE(*,100) 'Selected buffer is #'
        WRITE(*,*) NBNEW
        IF(LDEFBUFF(NBNEW))THEN
          WRITE(*,100) 'ERROR: this buffer has already '
          WRITE(*,101) 'been defined.'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          RETURN
        END IF
C------------------------------------------------------------------------------
        NDATABUFF(NBNEW)=REAL(NBIN)
        DO K=1,NBIN
          XDATA(K,NBNEW)=XMINH+(REAL(K)-0.5)*DX
          YDATA(K,NBNEW)=REAL(NDATA(K))
          EXDATA(K,NBNEW)=0.
          EYDATA(K,NBNEW)=0.
        END DO
C
        LXERR(NBNEW)=.FALSE.
        LYERR(NBNEW)=.FALSE.
        LXYNAME(NBNEW)=.FALSE.
        XMINBUFF(NBNEW)=1.0
        XMAXBUFF(NBNEW)=REAL(NBIN)
        LDEFBUFF(NBNEW)=.TRUE.
        LUSEBUFF(NBNEW)=.TRUE.
C
        CALL UPDATELIMITS(NBNEW)
        CALL SHOW_BUFFERS
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
        END
