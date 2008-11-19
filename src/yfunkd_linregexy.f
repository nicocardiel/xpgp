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
C Funcion a minimizar para ajustar una recta por minimos cuadrados,
C considerando la existencia de errores en las dos variables.
        DOUBLE PRECISION FUNCTION YFUNKD_LINREGEXY(A)
        IMPLICIT NONE
        DOUBLE PRECISION A(2)
C parametros
        INCLUDE 'ndatamax.inc'
C variables globales a la funcion y transmitidas mediante COMMONS
        INTEGER N
        DOUBLE PRECISION X(NDATAMAX),Y(NDATAMAX)
        DOUBLE PRECISION EX(NDATAMAX),EY(NDATAMAX)
C variables locales
        INTEGER I
        DOUBLE PRECISION SUM
C COMMONs
        COMMON/BLKYFUNKDLINREGEXY1/N
        COMMON/BLKYFUNKDLINREGEXY2/X,Y,EX,EY
C------------------------------------------------------------------------------
        SUM=0.D0
        DO I=1,N
          SUM=SUM+(Y(I)-A(1)-A(2)*X(I))*(Y(I)-A(1)-A(2)*X(I))/
     >     (EY(I)*EY(I)+A(2)*A(2)*EX(I)*EX(I))
        END DO
        YFUNKD_LINREGEXY=SUM
C
        END
