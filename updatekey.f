C
C******************************************************************************
C Determina el tipo de data KEY a representar
	SUBROUTINE UPDATEKEY
	IMPLICIT NONE
C
        INTEGER NBUFFMAX                 !numero de buffers de datos diferentes
        PARAMETER (NBUFFMAX=8)
C
	INTEGER READILIM_B
	INTEGER TRUEBEG
	INTEGER TRUELEN
	REAL READF_B
	CHARACTER*255 READC_B
C
	INTEGER NB
	INTEGER L1,L2
	INTEGER IDATAKEY
	CHARACTER*50 CDUMMY
	CHARACTER*50 DATAKEY(NBUFFMAX)
	REAL DATAKEYCH,DATAKEYCHSYMB
	LOGICAL LBATCH
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
C
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKLBATCH/LBATCH
        COMMON/BLKSETTINGS8A/IDATAKEY
        COMMON/BLKSETTINGS8B/DATAKEY
        COMMON/BLKSETTINGS8C/DATAKEYCH,DATAKEYCHSYMB
C------------------------------------------------------------------------------
	WRITE(*,*)
	WRITE(*,101) 'Choose KEY location: '
	WRITE(*,101) '--------------------'
	WRITE(*,101) '| <1>          <2> |'
	WRITE(*,101) '|                  |'
	WRITE(*,101) '|                  |'
	WRITE(*,101) '| <3>          <4> |'
	WRITE(*,101) '--------------------'
	WRITE(*,100) 'Option (0=none) '
	WRITE(CDUMMY,*) IDATAKEY
	IDATAKEY=READILIM_B(CDUMMY,0,4)
	WRITE(77,*) IDATAKEY
C
	IF(IDATAKEY.EQ.0) RETURN
C------------------------------------------------------------------------------
	WRITE(CDUMMY,*) DATAKEYCH
	WRITE(*,100) 'Current height for text in key '
	DATAKEYCH=READF_B(CDUMMY)
	WRITE(77,*) DATAKEYCH
	WRITE(CDUMMY,*) DATAKEYCHSYMB
	WRITE(*,100) 'Current height for symbol in key (0=actual size) '
	DATAKEYCHSYMB=READF_B(CDUMMY)
	WRITE(77,*) DATAKEYCHSYMB
C------------------------------------------------------------------------------
	NB=1
	DO WHILE(NB.NE.0)
	  WRITE(*,100) 'Buffer # to change key label (0=exit)'
	  NB=READILIM_B('0',0,NBUFFMAX)
	  WRITE(77,*) NB
	  IF(NB.NE.0)THEN
	    IF(LUSEBUFF(NB))THEN
	      L1=TRUEBEG(DATAKEY(NB))
	      L2=TRUELEN(DATAKEY(NB))
	      WRITE(*,100) 'New key '
	      DATAKEY(NB)=READC_B(DATAKEY(NB)(L1:L2),'@')
	      L1=TRUEBEG(DATAKEY(NB))
	      L2=TRUELEN(DATAKEY(NB))
	      WRITE(77,101) DATAKEY(NB)(L1:L2)
	    ELSE
	      WRITE(*,101) 'ERROR: this buffer is not activated'
	      WRITE(*,100) 'Press <CR> to continue...'
	      IF(LBATCH)THEN
	        WRITE(*,*)
	      ELSE
	        READ(*,*)
	      END IF
	    END IF
	  END IF
	END DO
C------------------------------------------------------------------------------
100	FORMAT(A,$)
101	FORMAT(A)
	END
