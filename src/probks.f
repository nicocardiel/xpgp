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
      FUNCTION PROBKS(ALAM)
      PARAMETER (EPS1=0.001, EPS2=1.E-8)
      A2=-2.*ALAM**2
      FAC=2.
      PROBKS=0.
      TERMBF=0.
      DO 11 J=1,100
        TERM=FAC*EXP(A2*J**2)
        PROBKS=PROBKS+TERM
        IF(ABS(TERM).LT.EPS1*TERMBF.OR.ABS(TERM).LT.EPS2*PROBKS)RETURN
        FAC=-FAC
        TERMBF=ABS(TERM)
11    CONTINUE
      PROBKS=1.
      RETURN
      END
