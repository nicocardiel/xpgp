C Fusiona los knots, modificando las variables de entrada
        SUBROUTINE MERGE_KNOTS(N,X,Y,LMERGE)
        INTEGER N
        REAL X(N),Y(N)
        LOGICAL LMERGE(N)
C
        INCLUDE 'nknotsmax.inc'
C
        INTEGER I,II
        INTEGER IMAX
        INTEGER NN
        REAL XX(NKNOTSMAX),YY(NKNOTSMAX)
        LOGICAL LOOP
C------------------------------------------------------------------------------
        do i=1,n-1
          print*,i,x(i),y(i),lmerge(i)
        end do
c
        DO I=1,N-1
          LOOP=.TRUE.
          II=I
          IMAX=I
          IF(LMERGE(I))THEN
            DO WHILE(LOOP)
              II=II+1
              IF(II.GT.N)THEN
                LOOP=.FALSE.
              ELSE
                IF(LMERGE(II))THEN
                  IMAX=II
                ELSE
                  LOOP=.FALSE.
                END IF
              END IF
            END DO
          END IF
          print*,'merge_knots: ',i,imax
        END DO
C
        END
