C Selecciona un buffer. Si no hay ninguno activo, retorna -1
C******************************************************************************
        SUBROUTINE SELBUFFER(NB_)
        IMPLICIT NONE
        INTEGER NB_
C
        INCLUDE 'nbuffmax.inc'
C
        INTEGER NBLOCAL
        REAL XC,YC
        CHARACTER*1 CH
        LOGICAL LBATCH
        LOGICAL LBEXIST
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
C
        COMMON/BLKLBATCH/LBATCH
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
C------------------------------------------------------------------------------
        CALL ONLYONE(NB_)
        IF(NB_.EQ.-1)THEN
          WRITE(*,101) 'ERROR: you must activate at least one '//
     +     'buffer.'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
        ELSEIF(NB_.EQ.0)THEN
          WRITE(*,100) 'Select buffer...'
          DO WHILE((NB_.LT.1).OR.(NB_.GT.NBUFFMAX))
            IF(LBATCH)THEN
              CALL READ_NB(NB_)
            ELSE
              CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
              CALL IFBUTTON(XC,YC,NB_)
              NBLOCAL=INDEX('12345678',CH)
              IF(NBLOCAL.NE.0)THEN
                CALL BUTTQEX(NBLOCAL,LBEXIST)
                IF(LBEXIST) NB_=NBLOCAL
              END IF
            END IF
            IF(.NOT.LDEFBUFF(NB_)) NB_=0
          END DO
          WRITE(77,111) NB_,'# Selected buffer number'
          WRITE(*,'(A,I1,A)') '   ...OK! Buffer #',NB_,' selected'
        ELSE
          WRITE(*,'(A,I1,A)') 'Buffer #',NB_,' selected'
        END IF
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
        END
