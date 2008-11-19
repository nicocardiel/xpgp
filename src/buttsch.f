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
C SUBROUTINE BUTTSCH(SIZE)
C
C Input: SIZE
C
C Set the character height in buttons.
C
C REAL SIZE -> the current font size (dimensionless multiple of the default
C      size)
C
Comment
C------------------------------------------------------------------------------
C
        SUBROUTINE BUTTSCH(SIZE)
        IMPLICIT NONE
        REAL SIZE
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
        PGSCH_BUTT=SIZE
        END
