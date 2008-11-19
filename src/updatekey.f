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
C Determina el tipo de data KEY a representar
        SUBROUTINE UPDATEKEY
        IMPLICIT NONE
C
        INCLUDE 'nbuffmax.inc'
C
        INTEGER READILIM_B
        INTEGER TRUEBEG
        INTEGER TRUELEN
        REAL READF_B
        CHARACTER*255 READC_B
C
        INTEGER NB
        INTEGER L1,L2
        INTEGER IDATAKEY
        CHARACTER*50 CDUMMY
        CHARACTER*50 DATAKEY(NBUFFMAX),DATAKEY_
        REAL DATAKEYCH,DATAKEYCHSYMB
        LOGICAL LBATCH
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
C
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKLBATCH/LBATCH
        COMMON/BLKSETTINGS8A/IDATAKEY
        COMMON/BLKSETTINGS8B/DATAKEY
        COMMON/BLKSETTINGS8C/DATAKEYCH,DATAKEYCHSYMB
C------------------------------------------------------------------------------
        WRITE(*,*)
        WRITE(*,101) 'Choose KEY location: '
        WRITE(*,101) '--------------------'
        WRITE(*,101) '| <1>          <2> |'
        WRITE(*,101) '|                  |'
        WRITE(*,101) '|                  |'
        WRITE(*,101) '| <3>          <4> |'
        WRITE(*,101) '--------------------'
        WRITE(*,100) 'Option (0=none) '
        WRITE(CDUMMY,*) IDATAKEY
        IDATAKEY=READILIM_B(CDUMMY,0,4)
        WRITE(77,111) IDATAKEY,'# key location (0,...,4)'
C
        IF(IDATAKEY.EQ.0) RETURN
C------------------------------------------------------------------------------
        WRITE(CDUMMY,*) DATAKEYCH
        WRITE(*,100) 'Current height for text in key '
        DATAKEYCH=READF_B(CDUMMY)
        WRITE(77,*) DATAKEYCH,'# Current height for text in key'
C
        WRITE(CDUMMY,*) DATAKEYCHSYMB
        WRITE(*,100) 'Current height for symbol in key (0=actual size) '
        DATAKEYCHSYMB=READF_B(CDUMMY)
        WRITE(77,*) DATAKEYCHSYMB,
     +   '# Current height for symbol in key (0=actual size)'
C------------------------------------------------------------------------------
        NB=1
        DO WHILE(NB.NE.0)
          WRITE(*,100) 'Buffer # to change key label (0=exit)'
          NB=READILIM_B('0',0,NBUFFMAX)
          WRITE(77,111) NB,
     +     '# Buffer number to change key label (0=exit)'
          IF(NB.NE.0)THEN
            IF(LUSEBUFF(NB))THEN
              L1=TRUEBEG(DATAKEY(NB))
              L2=TRUELEN(DATAKEY(NB))
              WRITE(*,100) 'New key (000=do not display) '
              DATAKEY_(1:50)=READC_B(DATAKEY(NB)(L1:L2),'@')
              DATAKEY(NB)=DATAKEY_
              L1=TRUEBEG(DATAKEY(NB))
              L2=TRUELEN(DATAKEY(NB))
              CALL TOLOG77_STRING(DATAKEY(NB)(L1:L2),'Key label')
            ELSE
              WRITE(*,101) 'ERROR: this buffer is not activated'
              WRITE(*,100) 'Press <CR> to continue...'
              IF(LBATCH)THEN
                WRITE(*,*)
              ELSE
                READ(*,*)
              END IF
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
        END
