C------------------------------------------------------------------------------
C Version 20-September-1999                                   File: downhilld.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: ncl@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE DOWNHILLD(N,X0,DX0,YFUNKD,A,B,G,YRMSTOL,XF,DXF,NEVAL)
C
C Input N,X0,DX0,YFUNKD,A,B,G,YRMSTOL
C Output XF,DXF,NEVAL
C
C Minimization of the function YFUNKD of N variables using the downhill 
C simplex method, as explained by Nelder and Mead (1965, Computer Journal, 7,
C pags. 308-313). The routine returns when the stopping criterion is reached 
C (the r.m.s. of the YFUNKD values computed with all the vertices of the simplex
C is .LT. YRMSTOL), or when the number of function evaluations
C is too large (NEVAL.GT.NEVALMAX).
C
C INTEGER N -> number of variables
C DOUBLE PRECISION   X0(N) -> starting point (initial solution)
C DOUBLE PRECISION  DX0(N) -> N characteristic length scales, employed to 
C                             derive N aditional starting points which, 
C                             together with X0, form the (N+1) vertices of 
C                             the simplex
C DOUBLE PRECISION  YFUNKD -> function to be minimized
C DOUBLE PRECISION       A -> reflection coefficient (ALPHA); tipically, A=1.0
C DOUBLE PRECISION       B -> contraction coefficient (BETA); tipically, B=0.5
C DOUBLE PRECISION       G -> expansion coefficient (GAMMA); tipically, G=2.0
C DOUBLE PRECISION YRMSTOL -> stopping criterion
C DOUBLE PRECISION   XF(N) -> final solution
C DOUBLE PRECISION  DXF(N) -> rms of XF evaluated from the final 
C                             different points of the simplex
C INTEGER            NEVAL -> number of evaluations of YFUNKD employed by 
C                             DOWNHIL to reach the solution; the routine 
C                             returns NEVAL=-1 if something goes wrong
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE DOWNHILLD(N,X0,DX0,YFUNKD,A,B,G,YRMSTOL,XF,DXF,NEVAL)
        IMPLICIT NONE
C
        INTEGER NMAX
        PARAMETER (NMAX=20)                        !maximum number of variables
        INTEGER NEVALMAX
        PARAMETER (NEVALMAX=2000) !maximum number of YFUNKD evaluations allowed
C
        INTEGER N
        DOUBLE PRECISION X0(N),DX0(N)
        DOUBLE PRECISION YFUNKD
        EXTERNAL YFUNKD
        DOUBLE PRECISION A,B,G
        DOUBLE PRECISION YRMSTOL
        DOUBLE PRECISION XF(N)
        DOUBLE PRECISION DXF(N)
        INTEGER NEVAL
C local variables
        INTEGER I,J
        INTEGER I1                   !index of point with lowest function value
        INTEGER I2             !index of point with next-highest function value
        INTEGER I3                  !index of point with highest function value
        DOUBLE PRECISION P(NMAX,NMAX+1)                !vertices of the simplex
        DOUBLE PRECISION PCEN(NMAX)   !(simplex without highest point) centroid
        DOUBLE PRECISION PSTAR1(NMAX),YSTAR1      !reflected point and function
        DOUBLE PRECISION PSTAR2(NMAX),YSTAR2       !expanded point and function
        DOUBLE PRECISION PSTAR3(NMAX),YSTAR3     !contracted point and function
        DOUBLE PRECISION Y(NMAX+1)     !function values at the simplex vertices
        DOUBLE PRECISION X(NMAX)  !coordinates of a single point of the simplex
        DOUBLE PRECISION CYRMSTOL                              !current YRMSTOL
        DOUBLE PRECISION FN,FN1                       !float of 1/N and 1/(N+1)
        DOUBLE PRECISION YMEAN      !mean value of the N+1 function evaluations
        DOUBLE PRECISION XMEAN         !mean single X value of the N+1 vertices
C------------------------------------------------------------------------------
C establecemos algunas protecciones y si algo falla retornamos NEVAL=-1
        NEVAL=-1
C
        DO J=1,N
          XF(J)=X0(J)
          DXF(J)=0.D0
        END DO
C
        IF(N.GT.NMAX)THEN
          WRITE(*,101)'ERROR in subroutine DOWNHILL:'
          WRITE(*,100)'N, NMAX: '
          WRITE(*,*)N,NMAX
          WRITE(*,101)'>>> N.GT.NMAX'
          WRITE(*,100)'Press <CR> to continue...'
          READ(*,*)
          RETURN
        END IF
C
        DO J=1,N
          IF(DX0(J).EQ.0.0)THEN
            WRITE(*,101)'ERROR in subroutine DOWNHILL:'
            DO I=1,N
              WRITE(*,100)'Length scale (i,value): '
              WRITE(*,*)I,DX0(I)
            END DO
            WRITE(*,101)'>>> characteristic length scale.EQ.0.0'
            WRITE(*,100)'Press <CR> to continue...'
            READ(*,*)
            RETURN
          END IF
        END DO
C------------------------------------------------------------------------------
C calculamos los N+1 vertices del simplex
        DO I=1,N+1                                           !numero de vertice
          IF(I.EQ.N+1)THEN                                 !el valor inicial X0
            DO J=1,N
              P(J,I)=X0(J)
            END DO
          ELSE                !el valor inicial, modificando solo una dimension
            DO J=1,N
              IF(J.EQ.I)THEN
                P(J,I)=X0(J)+DX0(J)
              ELSE
                P(J,I)=X0(J)
              END IF
            END DO
          END IF
        END DO
C evaluamos la funcion en cada uno de los vertices
        DO I=1,N+1
          DO J=1,N
            X(J)=P(J,I)
          END DO
          Y(I)=YFUNKD(X)
        END DO
C numero de evaluaciones de la funcion (no contamos las realizadas para
C inicializar el simplex)
        NEVAL=0
C real de 1/N y de 1/(N+1)
        FN=1.D0/DBLE(N)
        FN1=1.D0/DBLE(N+1)
C------------------------------------------------------------------------------
C calculamos el numero de vertice con el valor minimo (I1), el maximo (I3) y 
C el siguiente al maximo (I2)
10      IF(Y(1).LE.Y(2))THEN
          I3=2
          I2=1
        ELSE
          I3=1
          I2=2
        END IF
        I1=I2
        IF(N.GT.1)THEN
          DO I=3,N+1
            IF(Y(I).LT.Y(I1))THEN
              I1=I
            ELSE
              IF(Y(I).GT.Y(I3))THEN
                I2=I3
                I3=I
              ELSE
                IF(Y(I).GT.Y(I2)) I2=I
              END IF
            END IF
          END DO
        END IF
C------------------------------------------------------------------------------
C comprobamos si se satisface la condicion de salida
        YMEAN=0.D0
        DO I=1,N+1
          YMEAN=YMEAN+Y(I)
        END DO
        YMEAN=YMEAN*FN1
        CYRMSTOL=0.D0
        DO I=1,N+1
          CYRMSTOL=CYRMSTOL+(Y(I)-YMEAN)*(Y(I)-YMEAN)
        END DO
        CYRMSTOL=DSQRT(CYRMSTOL*FN)
        IF((CYRMSTOL.LT.YRMSTOL).OR.(NEVAL.GE.NEVALMAX))THEN !.............EXIT
          DO J=1,N  !retornamos como solucion el vertice con el valor minimo I1
            XF(J)=P(J,I1)
            XMEAN=0.D0
            DO I=1,N+1
              XMEAN=XMEAN+P(J,I)
            END DO
            XMEAN=XMEAN*FN1
            DXF(J)=0.D0
            DO I=1,N+1
              DXF(J)=DXF(J)+(P(J,I)-XMEAN)*(P(J,I)-XMEAN)
            END DO
            DXF(J)=SQRT(DXF(J)*FN)
          END DO
          RETURN
        END IF
C------------------------------------------------------------------------------
C calculamos el centroide de los vertices del simplex sin el punto I3
        DO J=1,N
          PCEN(J)=0.D0
        END DO
C sumamos vertices
        DO I=1,N+1
          IF(I.NE.I3)THEN
            DO J=1,N
              PCEN(J)=PCEN(J)+P(J,I)
            END DO
          END IF
        END DO
C normalizamos
        DO J=1,N
          PCEN(J)=PCEN(J)*FN
        END DO
C------------------------------------------------------------------------------
C ponemos en marcha la "maquina"
        DO J=1,N
          PSTAR1(J)=(1.D0+A)*PCEN(J)-A*P(J,I3)                      !reflection
        END DO
        YSTAR1=YFUNKD(PSTAR1)
        NEVAL=NEVAL+1
        IF(YSTAR1.LT.Y(I1))THEN              !reflection produced a new minimum
          DO J=1,N
            PSTAR2(J)=G*PSTAR1(J)+(1.D0-G)*PCEN(J) !try an additional expansion
          END DO
          YSTAR2=YFUNKD(PSTAR2)
          NEVAL=NEVAL+1
          IF(YSTAR2.LT.Y(I1))THEN               !additional expansion succeeded
            DO J=1,N                           !replace highest point by PSTAR2
              P(J,I3)=PSTAR2(J)
            END DO
            Y(I3)=YSTAR2
          ELSE                            !additional expansion did not succeed
            DO J=1,N                           !replace highest point by PSTAR1
              P(J,I3)=PSTAR1(J)
            END DO
            Y(I3)=YSTAR1
          END IF
        ELSE                          !reflection did not produce a new minimum
          IF(YSTAR1.GT.Y(I2))THEN
            IF(YSTAR1.LT.Y(I3))THEN     !reflected point is better than highest
              DO J=1,N                         !replace highest point by PSTAR1
                P(J,I3)=PSTAR1(J)
              END DO
              Y(I3)=YSTAR1
            END IF
            DO J=1,N
              PSTAR3(J)=B*P(J,I3)+(1.D0-B)*PCEN(J)           !try a contraction
            END DO
            YSTAR3=YFUNKD(PSTAR3)
            NEVAL=NEVAL+1
            IF(YSTAR3.GT.Y(I3))THEN                !contraction did not succeed
              DO I=1,N+1         !multiple contraction towards the lowest point
                IF(I.NE.I1)THEN
                  DO J=1,N
                    X(J)=0.5D0*(P(J,I)+P(J,I1))
                    P(J,I)=X(J)
                  END DO
                  Y(I)=YFUNKD(X)
                  NEVAL=NEVAL+1
                END IF
              END DO
            ELSE                                     !the contraction succeeded
              DO J=1,N                         !replace highest point by PSTAR3
                P(J,I3)=PSTAR3(J)
              END DO
              Y(I3)=YSTAR3
            END IF
          ELSE    !initial reflected point lies between lowest and next-highest
            DO J=1,N                           !replace highest point by PSTAR1
              P(J,I3)=PSTAR1(J)
            END DO
            Y(I3)=YSTAR1
          END IF
        END IF
        GOTO 10
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
