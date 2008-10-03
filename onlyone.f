C
C******************************************************************************
C Chequea si solo hay un buffer activo. Si es asi, el numero de ese buffer es
C retornado en NB. En caso contrario, NB retorna 0. Si no existe ningun buffer
C activo, la rutina retorna NB=-1.
        SUBROUTINE ONLYONE(NB)
        IMPLICIT NONE
        INTEGER NB
C
        INCLUDE 'nbuffmax.inc'
C
        INTEGER N,NB_
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
C
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
C------------------------------------------------------------------------------
        N=0
        DO NB_=1,NBUFFMAX
          IF(LUSEBUFF(NB_))THEN
            N=N+1
            NB=NB_
          END IF
        END DO
        IF(N.EQ.0)THEN
          NB=-1
        ELSEIF(N.NE.1)THEN
          NB=0
        END IF
C
        END
