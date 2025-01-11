  DO J = 1, N
     DO I = 1, M
        !DIR$ VECTOR ALWAYS
        B(I,J) = A(I,J)
     END DO
  END DO
