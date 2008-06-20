C
C******************************************************************************
C Muestra los coeficientes del ajuste polinomico en pantalla, realizando
C un analisis estadistico de la significacion de dichos coeficientes.
	SUBROUTINE SHOW_FITPOL(A,VARA,IFCOEF,NDEGMAX,NDEG,NF)
	IMPLICIT NONE
C
	INTEGER NDEGMAX
	DOUBLE PRECISION A(NDEGMAX+1)
	DOUBLE PRECISION VARA(NDEGMAX+1)
	LOGICAL IFCOEF(NDEGMAX+1)
	INTEGER NDEG,NF
C
	INTEGER I
	REAL T,FTSTUDENT
C------------------------------------------------------------------------------
        DO I=1,NDEG+1
          IF(IFCOEF(I))THEN
            IF(I.LE.10)THEN
              WRITE(*,'(A4,I1,A3,$)') '  a(',I-1,'): '
            ELSE
              WRITE(*,'(A3,I2,A3,$)') ' a(',I-1,'): '
            END IF
	    IF(VARA(I).EQ.0.D0)THEN
              WRITE(*,*) A(I),DSQRT(VARA(I))
	    ELSE
	      IF(VARA(I).EQ.0.0)THEN
                WRITE(*,*) A(I),DSQRT(VARA(I))
	      ELSE
                T=ABS(REAL(A(I)))/SQRT(REAL(VARA(I)))
                WRITE(*,*) A(I),DSQRT(VARA(I)),
     +           T,2.0*FTSTUDENT(NF-(NDEG+1),T)
	      END IF
	    END IF
          ELSE
            IF(I.LE.10)THEN
              WRITE(*,'(A4,I1,A3,$)') ' *a(',I-1,'): '
            ELSE
              WRITE(*,'(A3,I2,A3,$)') '*a(',I-1,'): '
            END IF
            WRITE(*,*) A(I),DSQRT(VARA(I))
          END IF
        END DO
C
	END
