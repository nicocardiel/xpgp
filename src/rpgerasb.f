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
C SUBROUTINE RPGERASB
C
C Input (COMMON) : X3VPORT,X4VPORT,Y3VPORT,Y4VPORT
C
C Clear the button region (preserving the plot region which does not overlap
C with the plot region).
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE RPGERASB
        IMPLICIT NONE
C Borra la region de plots
        INCLUDE 'button.inc'
C
        INTEGER CI,FS
C------------------------------------------------------------------------------
        CALL PGBBUF
        CALL PGVPORT(X3VPORT,X4VPORT,Y3VPORT,Y4VPORT)
        CALL PGWINDOW(0.0,1.0,0.0,1.0)
        CALL PGQCI(CI)
        CALL PGQFS(FS)
        CALL PGSCI(0)
        CALL PGSFS(1)
        CALL PGRECT(0.0,1.0,0.0,1.0)
        CALL PGSCI(CI)
        CALL PGSFS(FS)
        CALL PGEBUF
C
        IF(MODOTEXT_BUTT) WRITE(*,101) '^[[1;1f^[[J'
C
101     FORMAT(A)
        END
