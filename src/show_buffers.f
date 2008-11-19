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
C Muestra los botones indicando el estado de cada buffer
        SUBROUTINE SHOW_BUFFERS
        IMPLICIT NONE
C
        INCLUDE 'nbuffmax.inc'
C
        INTEGER NB
        INTEGER NCOLORBUFF(NBUFFMAX)
        CHARACTER*50 CBUTTON
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
C
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKBUFFERS2/NCOLORBUFF
C------------------------------------------------------------------------------
        DO NB=1,NBUFFMAX
          WRITE(CBUTTON,'(A10,I1,A1)') 'buffer # [',NB,']'
          IF(LDEFBUFF(NB))THEN
            IF(LUSEBUFF(NB))THEN
              CALL BUTTON(NB,CBUTTON,5)
              IF(NCOLORBUFF(NB).NE.0)THEN
                CALL BUTTON(NB,CBUTTON,-NCOLORBUFF(NB)-1)
              ELSE
                CALL BUTTON(NB,CBUTTON,2)
              END IF
            ELSE
              CALL BUTTON(NB,CBUTTON,0)
              IF(NCOLORBUFF(NB).NE.0)THEN
                CALL BUTTON(NB,CBUTTON,-NCOLORBUFF(NB)-1)
              ELSE
                CALL BUTTON(NB,CBUTTON,2)
              END IF
            END IF
          ELSE
            CALL BUTTON(NB,CBUTTON,0)
            CALL BUTTON(NB,CBUTTON,-16)       !color falso de boton desactivado
          END IF
        END DO
C
        END
