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
Comment
C
C SUBROUTINE BUTTSEX(NBUT,LEXIST)
C
C Input: NBUT,LEXIST
C
C Set whether the asked button is active (currently available) or not.
C
C INTEGER NBUT -> button number
C LOGICAL LEXIST -> .TRUE. if the button is active, .FALSE. otherwise
C
Comment
C------------------------------------------------------------------------------
C
        SUBROUTINE BUTTSEX(NBUT,LEXIST)
        IMPLICIT NONE
        INTEGER NBUT
        LOGICAL LEXIST
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
        IF((NBUT.LT.1).OR.(NBUT.GT.MAX_NBUTT))THEN
          WRITE(*,101)'ERROR: button number out of limits in '//
     +     'subroutine BUTTSEX.'
          LEXIST=.FALSE.
          RETURN
        END IF
        EXIST_BUTT(NBUT)=LEXIST
101     FORMAT(A)
        END
