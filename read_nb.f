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
