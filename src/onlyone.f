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
C Chequea si solo hay un buffer activo. Si es asi, el numero de ese buffer es
C retornado en NB. En caso contrario, NB retorna 0. Si no existe ningun buffer
C activo, la rutina retorna NB=-1.
        SUBROUTINE ONLYONE(NB)
        IMPLICIT NONE
        INTEGER NB
C
        INCLUDE 'nbuffmax.inc'
C
        INTEGER N,NB_
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
C
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
C------------------------------------------------------------------------------
        N=0
        DO NB_=1,NBUFFMAX
          IF(LUSEBUFF(NB_))THEN
            N=N+1
            NB=NB_
          END IF
        END DO
        IF(N.EQ.0)THEN
          NB=-1
        ELSEIF(N.NE.1)THEN
          NB=0
        END IF
C
        END
