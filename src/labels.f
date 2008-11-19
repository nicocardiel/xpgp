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
C IMODE=0 pide el nombre de fichero con etiquetas
C IMODE=1 abre el fichero con etiquetas
        SUBROUTINE LABELS(IMODE)
        IMPLICIT NONE
        INTEGER IMODE
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER INDEXR
C
        INTEGER L1,L2,LL2
        INTEGER NEXT
        INTEGER NCOLOR,NFONT,OLDCI,OLDCF
        INTEGER LINESTYLE,LINEWIDTH,OLDLS,OLDLW
        REAL X,Y,ANGLE,FJUST,SIZE,OLDCH
        REAL X1,Y1,X2,Y2
        CHARACTER*255 FILELABELS,CADENA,ETIQUETA
        LOGICAL LBATCH
        LOGICAL LOGFILE
        LOGICAL LLABELS
        LOGICAL LOOP
C
        COMMON/BLKLABELS1/LLABELS
        COMMON/BLKLABELS2/FILELABELS
        COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
        IF(IMODE.EQ.0)THEN
          WRITE(*,101) 'The file must contain any of the following: '
          WRITE(*,101)
          WRITE(*,101) '(1) Comment lines (starting by #)'
          WRITE(*,101) '# this is a comment'
          WRITE(*,101)
          WRITE(*,101) '(2) A connected segment'
          WRITE(*,101) 'Connect: X1 Y1 X2 Y2 LineType LineWidth COLOR'
          WRITE(*,101)
          WRITE(*,101) '(3) An arbitrary string'
          WRITE(*,101) 'X Y ANGLE FJUST SIZE FONT COLOR LABEL'
          WRITE(*,101)
          LOGFILE=.FALSE.
          DO WHILE(.NOT.LOGFILE)
            WRITE(*,100) 'File name (<CR>=none) ? '
            IF(LBATCH)THEN
              LOOP=.TRUE.
              DO WHILE(LOOP)
                READ(78,101) FILELABELS
                IF(FILELABELS(1:1).NE.'#')THEN
                  LL2=INDEXR(FILELABELS,'#')
                  IF(LL2.GT.1)THEN
                    FILELABELS=FILELABELS(1:LL2-1)
                    WRITE(*,101) FILELABELS(1:TRUELEN(FILELABELS))
                    LOOP=.FALSE.
                  END IF
                END IF
              END DO
            ELSE
              READ(*,101) FILELABELS
              IF(TRUELEN(FILELABELS).EQ.0)THEN
                RETURN
              END IF
            END IF
            INQUIRE(FILE=FILELABELS,EXIST=LOGFILE)
            IF(.NOT.LOGFILE)THEN
              WRITE(*,100) 'ERROR: this file does not exist. '
              WRITE(*,101) 'Try again.'
              WRITE(*,100) 'Press <CR> to continue...'
              IF(LBATCH)THEN
                WRITE(*,*)
              ELSE
                READ(*,*)
              END IF
            END IF
          END DO
          CALL TOLOG77_STRING(FILELABELS(1:TRUELEN(FILELABELS)),
     +     'File name with labels')
          LLABELS=.TRUE.
          RETURN
        END IF
C
        OPEN(15,FILE=FILELABELS,STATUS='OLD',FORM='FORMATTED',ERR=904)
C
10      READ(15,101,END=900) CADENA
        IF(TRUELEN(CADENA).EQ.0) GOTO 10 !linea en blanco
        IF(CADENA(1:1).EQ.'#') GOTO 10   !comentario
        IF(CADENA(1:1).EQ.'C')THEN !.............................connected line
          L1=TRUEBEG(CADENA(2:))+1 !ojo, el +1 es importante
          L2=TRUELEN(CADENA)
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) X1
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) Y1
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) X2
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) Y2
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) LINESTYLE
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) LINEWIDTH
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          READ(CADENA(L1:L2),*,ERR=903) NCOLOR
C
          CALL PGQCI(OLDCI)
          CALL PGQLS(OLDLS)
          CALL PGQLW(OLDLW)
          CALL PGSCI(NCOLOR)
          CALL PGSLS(LINESTYLE)
          CALL PGSLW(LINEWIDTH)
          CALL PGMOVE(X1,Y1)
          CALL PGDRAW(X2,Y2)
          CALL PGSCI(OLDCI)
          CALL PGSLS(OLDLS)
          CALL PGSLW(OLDLW)
        ELSE !............................................una cadena arbitraria
          L1=TRUEBEG(CADENA)
          L2=TRUELEN(CADENA)
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) X
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) Y
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) ANGLE
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) FJUST
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) SIZE
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) NFONT
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          NEXT=INDEX(CADENA(L1:L2),' ')
          IF(NEXT.EQ.0) GOTO 902
          READ(CADENA(L1:L1+NEXT-2),*,ERR=903) NCOLOR
          L1=L1+NEXT
          L1=L1+TRUEBEG(CADENA(L1:L2))-1
C
          ETIQUETA=CADENA(L1:L2)
C
          CALL PGQCF(OLDCF)
          CALL PGQCI(OLDCI)
          CALL PGQCH(OLDCH)
          CALL PGSCF(NFONT)
          CALL PGSCI(NCOLOR)
          CALL PGSCH(SIZE)
          CALL PGPTXT(X,Y,ANGLE,FJUST,ETIQUETA)
          CALL PGSCF(OLDCF)
          CALL PGSCI(OLDCI)
          CALL PGSCH(OLDCH)
        END IF
C
        GOTO 10
C------------------------------------------------------------------------------
900     CLOSE(15)
        RETURN
C..............................................................................
902     WRITE(*,101) 'ERROR: unexpected end of line.'
        CLOSE(15)
        RETURN
C..............................................................................
903     WRITE(*,101) 'ERROR: while reading number.'
        CLOSE(15)
        RETURN
C..............................................................................
904     WRITE(*,101) 'ERROR: while opening the file.'
        RETURN
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
