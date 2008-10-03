C
C******************************************************************************
C Muestra los botones indicando el estado de cada buffer
	SUBROUTINE SHOW_BUFFERS
	IMPLICIT NONE
C
	INTEGER NBUFFMAX                 !numero de buffers de datos diferentes
	PARAMETER (NBUFFMAX=8)
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
	      CALL BUTTON(NB,CBUTTON,-NCOLORBUFF(NB)-1)
	    ELSE
	      CALL BUTTON(NB,CBUTTON,0)
	      CALL BUTTON(NB,CBUTTON,-NCOLORBUFF(NB)-1)
	    END IF
	  ELSE
	    CALL BUTTON(NB,CBUTTON,0)
	    CALL BUTTON(NB,CBUTTON,-16)       !color falso de boton desactivado
	  END IF
	END DO
C
	END
