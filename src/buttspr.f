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
C SUBROUTINE BUTTSPR(X1,X2,Y1,Y2)
C
C Input: X1,X2,Y1,Y2
C 
C Set the plot region limits.
C
C REAL X1 -> x-coordinate of the left hand edge of the plot region viewport,
C      in normalized device coordinates
C REAL X2 -> x-coordinate of the right hand edge of the plot region viewport,
C      in normalized device coordinates
C REAL Y1 -> y-coordinate of the bottom edge of the plot region viewport,
C      in normalized device coordinates
C REAL Y2 -> y-coordinate of the top edge of the plot region viewport,
C      in normalized device coordinates
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE BUTTSPR(X1,X2,Y1,Y2)
        IMPLICIT NONE
        REAL X1,X2,Y1,Y2
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
        X1VPORT=X1
        X2VPORT=X2
        Y1VPORT=Y1
        Y2VPORT=Y2
        END
