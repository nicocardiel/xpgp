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
C SUBROUTINE RPGERASW(X1,X2,Y1,Y2,NCOLOR)
C
C Input: X1,X2,Y1,Y2
C
C Clear any rectangle defined by (X1,Y1) lower left corner
C                                (X2,Y2) upper right corner
C
C REAL X1 -> x-coordinate of the left hand edge of the rectangle to be 
C            cleared,in normalized device coordinates
C REAL X2 -> x-coordinate of the right hand edge of the rectangle to be 
C            cleared,in normalized device coordinates
C REAL Y1 -> y-coordinate of the bottom edge of the rectangle to be 
C            cleared,in normalized device coordinates
C REAL Y2 -> y-coordinate of the top edge of the rectangle to be 
C            cleared,in normalized device coordinates
C INTEGER NCOLOR -> background color
C
C NOTE: this subroutine preserves the original viewport and window coordinate
C       systems
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE RPGERASW(X1,X2,Y1,Y2,NCOLOR)
        IMPLICIT NONE
        REAL X1,X2,Y1,Y2
        INTEGER NCOLOR
C
        INTEGER CI,FS
        REAL XW1,XW2,YW1,YW2
        REAL XV1,XV2,YV1,YV2
C------------------------------------------------------------------------------
        CALL PGBBUF
C
        CALL PGQWIN(XW1,XW2,YW1,YW2)
        CALL PGQVP(0,XV1,XV2,YV1,YV2)
C
        CALL PGVPORT(X1,X2,Y1,Y2)
        CALL PGWINDOW(0.0,1.0,0.0,1.0)
        CALL PGQCI(CI)
        CALL PGQFS(FS)
        CALL PGSCI(NCOLOR)
        CALL PGSFS(1)
        CALL PGRECT(0.0,1.0,0.0,1.0)
        CALL PGSCI(CI)
        CALL PGSFS(FS)
C
        CALL PGVPORT(XV1,XV2,YV1,YV2)
        CALL PGWINDOW(XW1,XW2,YW1,YW2)
C
        CALL PGEBUF
C
        END
