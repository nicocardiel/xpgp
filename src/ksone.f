      SUBROUTINE KSONE(DATA,N,FUNC,D,PROB)
      DIMENSION DATA(N)
      CALL ordena1f(N,DATA)
      EN=N
      D=0.
      FO=0.
      DO 11 J=1,N
        FN=J/EN !Data's c.d.f. after this step
        FF=FUNC(DATA(J)) !compare to the user-supplied function
        DT=AMAX1(ABS(FO-FF),ABS(FN-FF))
        IF(DT.GT.D)D=DT
        FO=FN
11    CONTINUE
      en=sqrt(en)
      PROB=PROBKS((en+0.12+0.11/en)*D)
      RETURN
      END
