  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (K .LT. 0) INFO = -1
  L = N / 2 ! TODO
  IF (K .GT. L) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  IF (K .GT. 0) L = K
  IF (L .EQ. 0) THEN
     W(1) = ZERO
     RETURN
  END IF

  M = N * (N - 1)
  M_2 = M / 2
  J = 2 * M

  ! W = | G |
  CALL ABSOG(N, N, G, LDG, W, N, O(J))
  IF (O(J) .NE. 0) THEN
     INFO = -3
     RETURN
  END IF
  ! (W,P,Q) = (W(1:M_2),O(1:M_2),O(M_2+1:M))
  CALL MKWPQ(N, G, LDG, W, O, O(J))
  IF (O(J) .EQ. 0) THEN
     INFO = 0
     W(M_2 + 1) = ZERO
     RETURN
  END IF
  IF (O(J) .LT. 0) THEN
     INFO = -5
     RETURN
  END IF
  I = O(J)
  L = MIN(L, I)
  ! sort (W,P,Q)
  CALL PQSORT(PQCMP, M_2, W, O, O(M_2+1), W(M_2+1), O(M+1), O(M+M_2+1), INFO)
  O(J) = INFO
  IF (INFO .LT. 0) THEN
     INFO = -6
     RETURN
  END IF

  ! W(J) = || off(G) ||_F
  J = M_2 + 1
  W(J) = ZERO
  DO WHILE (I .GE. 1)
     IF (W(I) .GT. ZERO) THEN
        W(J) = W(I)
        I = I - 1
        EXIT
     END IF
     I = I - 1
  END DO
  DO WHILE (I .GE. 1)
     W(J) = CR_HYPOT(W(J), W(I))
     I = I - 1
  END DO

#ifndef NDEBUG
  !$OMP PARALLEL DO DEFAULT(SHARED)
  DO I = J+1, M
     W(I) = ZERO
  END DO
  !$OMP END PARALLEL DO
#endif
  !$OMP PARALLEL DO DEFAULT(SHARED)
  DO I = M+1, N*N
     W(I) = MONE
  END DO
  !$OMP END PARALLEL DO
#ifndef NDEBUG
  !$OMP PARALLEL DO DEFAULT(SHARED)
  DO I = M+1, 2*M-1
     O(I) = 0
  END DO
  !$OMP END PARALLEL DO
#endif

  I = 1
  INFO = 0
  DO WHILE ((INFO .LT. L) .AND. (W(I) .GE. ZERO))
     J = M_2 + I
     IF ((W(M+O(I)) .EQ. MONE) .AND. (W(M+O(J)) .EQ. MONE)) THEN
        INFO = INFO + 1
        W(M+O(I)) = W(I)
        W(M+O(J)) = W(I)
        O(M+INFO) = I
     END IF
     I = I + 1
     IF (I .GT. M_2) EXIT
  END DO
