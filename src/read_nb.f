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
C Leemos el valor de NB del fichero numero 78, ignorando comentarios
        SUBROUTINE READ_NB(NB)
        IMPLICIT NONE
        INTEGER NB
C
        LOGICAL LOOP
        CHARACTER*255 CLINEA
C------------------------------------------------------------------------------
        LOOP=.TRUE.
        DO WHILE(LOOP)
          READ(78,101) CLINEA
          IF(CLINEA(1:1).NE.'#') LOOP=.FALSE.
        END DO
        READ(CLINEA,*) NB
C
101     FORMAT(A)
        END
