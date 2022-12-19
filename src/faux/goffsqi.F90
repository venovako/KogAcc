  ! This is the generic part of the (off-)norm-squared routines for real matrices.
  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN
  SELECT CASE (O)
  CASE('A')
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(N,G) PRIVATE(I,Y) REDUCTION(+:S)
     DO J = 1, N
        DO I = 1, N
           Y = G(I,J)
           S = IEEE_FMA(Y, Y, S)
        END DO
     END DO
     !$OMP END PARALLEL DO
  CASE('O')
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(N,G) PRIVATE(I,Y) REDUCTION(+:S)
     DO J = 1, N
        DO I = 1, J-1
           Y = G(I,J)
           S = IEEE_FMA(Y, Y, S)
        END DO
        DO I = J+1, N
           Y = G(I,J)
           S = IEEE_FMA(Y, Y, S)
        END DO
     END DO
     !$OMP END PARALLEL DO
  CASE('a')
     DO J = 1, N
        DO I = 1, N
           Y = G(I,J)
           S = IEEE_FMA(Y, Y, S)
        END DO
     END DO
  CASE('o')
     DO J = 1, N
        DO I = 1, J-1
           Y = G(I,J)
           S = IEEE_FMA(Y, Y, S)
        END DO
        DO I = J+1, N
           Y = G(I,J)
           S = IEEE_FMA(Y, Y, S)
        END DO
     END DO
  CASE DEFAULT
     INFO = -1
  END SELECT
