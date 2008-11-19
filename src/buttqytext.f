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
C SUBROUTINE BUTTQYTEXT(YTEXT)
C
C Output: YTEXT
C
C Return the current relative y-position of the text baseline in buttons
C (from 0 to 1)
C
C REAL YTEXT -> = YTEXT_BUTT
C
Comment
C------------------------------------------------------------------------------
C
        SUBROUTINE BUTTQYTEXT(YTEXT)
        IMPLICIT NONE
        REAL YTEXT
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
        YTEXT=YTEXT_BUTT
        END
