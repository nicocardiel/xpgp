C
C******************************************************************************
C Muestra los botones indicando el estado de cada buffer
        SUBROUTINE SHOW_BUFFERS
        IMPLICIT NONE
C
        INCLUDE 'nbuffmax.inc'
C
        INTEGER NB
        INTEGER NCOLORBUFF(NBUFFMAX)
        CHARACTER*50 CBUTTON
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
C
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKBUFFERS2/NCOLORBUFF
C------------------------------------------------------------------------------
        DO NB=1,NBUFFMAX
          WRITE(CBUTTON,'(A10,I1,A1)') 'buffer # [',NB,']'
          IF(LDEFBUFF(NB))THEN
            IF(LUSEBUFF(NB))THEN
              CALL BUTTON(NB,CBUTTON,5)
              IF(NCOLORBUFF(NB).NE.0)THEN
                CALL BUTTON(NB,CBUTTON,-NCOLORBUFF(NB)-1)
              ELSE
                CALL BUTTON(NB,CBUTTON,2)
              END IF
            ELSE
              CALL BUTTON(NB,CBUTTON,0)
              IF(NCOLORBUFF(NB).NE.0)THEN
                CALL BUTTON(NB,CBUTTON,-NCOLORBUFF(NB)-1)
              ELSE
                CALL BUTTON(NB,CBUTTON,2)
              END IF
            END IF
          ELSE
            CALL BUTTON(NB,CBUTTON,0)
            CALL BUTTON(NB,CBUTTON,-16)       !color falso de boton desactivado
          END IF
        END DO
C
        END
