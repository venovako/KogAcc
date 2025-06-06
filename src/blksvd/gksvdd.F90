  IF (INFO .LT. 0) THEN
     MRQSTP = -(INFO + 1)
     LACC = .TRUE.
  ELSE ! INFO .GE. 0
     MRQSTP = INFO
     LACC = .FALSE.
  END IF
  INFO = 0
  LOMP = .FALSE.
  J = IAND(JOB, 7)
  !$ IF (J .EQ. 3) LOMP = .TRUE.

  W(1) = ONE
  W(2) = ONE
  W(3) = ONE
  W(4) = ZERO
  W(5) = ZERO
  W(6) = ZERO

  IF (LDV .LT. N) INFO = -8
  IF (LDU .LT. N) INFO = -6
  IF (LDG .LT. N) INFO = -4
  IF ((N .LT. 0) .OR. (MOD(N, 2) .NE. 0)) INFO = -2
  IF (JOB .LT. 0) INFO = -1
  IF (JOB .GT. 1023) INFO = -1
  IF ((J .NE. 3) .AND. (J .NE. 6)) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  LUSID = (IAND(JOB, USID) .NE. 0)
  LUACC = (IAND(JOB, UACC) .NE. 0)
  LVSID = (IAND(JOB, VSID) .NE. 0)
  LVACC = (IAND(JOB, VACC) .NE. 0)

  M = N * (N - 1)

#ifndef NDEBUG
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(SV,N) PRIVATE(J) IF(LOMP)
  DO J = 1, N
     SV(J) = ZERO
  END DO
  !$OMP END PARALLEL DO
#endif

  IF (N .EQ. 1) THEN
     GN = ABS(G(1,1))
     IF (.NOT. (GN .LE. HUGE(GN))) THEN
        INFO = -3
     ELSE ! finite G
        IF (LUSID) U(1,1) = SIGN(ONE, G(1,1))
        IF (LVSID) V(1,1) = ONE
        G(1,1) = GN
        SV(1) = GN
        W(1) = GN
     END IF
     RETURN
  END IF

  ! optionally set U and V to I
  IF (LUSID) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(U,N) IF(LOMP)
     DO J = 1, N
        DO I = 1, J-1
           U(I,J) = ZERO
        END DO
        U(J,J) = ONE
        DO I = J+1, N
           U(I,J) = ZERO
        END DO
     END DO
     !$OMP END PARALLEL DO
  END IF
  IF (LVSID) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(V,N) IF(LOMP)
     DO J = 1, N
        DO I = 1, J-1
           V(I,J) = ZERO
        END DO
        V(J,J) = ONE
        DO I = J+1, N
           V(I,J) = ZERO
        END DO
     END DO
     !$OMP END PARALLEL DO
  END IF

  ! scale G
  L = -1
  !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
  CALL LANGO(N, G, LDG, GN, L)
  IF (L .NE. 0) THEN
     INFO = -3
     RETURN
  END IF
  GS = EXPONENT(HUGE(GN)) - EXPONENT(GN) - 3
  IF (GS .NE. 0) THEN
     L = 0
     !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
     CALL SCALG(N, N, G, LDG, GS, L)
     IF (L .NE. 0) THEN
        INFO = -3
        RETURN
     END IF
     GN = SCALE(GN, GS)
  END IF

  ! optionally scale U
  IF (LUACC) THEN
     IF (LUSID) THEN
        UN = ONE
        US = 0
     ELSE ! scaling of U might be required
        L = -1
        !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
        CALL LANGO(N, U, LDU, UN, L)
        IF (L .NE. 0) THEN
           INFO = -5
           RETURN
        END IF
        US = EXPONENT(HUGE(UN)) - EXPONENT(UN) - 2
     END IF
     IF (US .NE. 0) THEN
        L = 0
        !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
        CALL SCALG(N, N, U, LDU, US, L)
        IF (L .NE. 0) THEN
           INFO = -5
           RETURN
        END IF
        UN = SCALE(UN, US)
     END IF
  ELSE ! .NOT. LUACC
     UN = ONE
     US = 0
  END IF

  ! optionally scale V
  IF (LVACC) THEN
     IF (LVSID) THEN
        VN = ONE
        VS = 0
     ELSE ! scaling of V might be required
        L = -1
        !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
        CALL LANGO(N, V, LDV, VN, L)
        IF (L .NE. 0) THEN
           INFO = -7
           RETURN
        END IF
        VS = EXPONENT(HUGE(VN)) - EXPONENT(VN) - 2
     END IF
     IF (VS .NE. 0) THEN
        L = 0
        !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
        CALL SCALG(N, N, V, LDV, VS, L)
        IF (L .NE. 0) THEN
           INFO = -7
           RETURN
        END IF
        VN = SCALE(VN, VS)
     END IF
  ELSE ! .NOT. LVACC
     VN = ONE
     VS = 0
  END IF

  DO STP = 0, MRQSTP-1
     T = STP + 1

     ! build the current step's pairs
     L = 0
     !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
     IF (LACC) L = -L - 1
     CALL MKD(N, G, LDG, D, O, L)
     IF (L .LT. 0) THEN
        INFO = -10
        RETURN
     END IF
     I = L
     L = 0
     !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
     IF (LACC) L = -L - 1
     CALL MKDPQ(N, I, D, OD, L)
     IF (L .LT. 0) THEN
        INFO = -11
        RETURN
     END IF
     I = L
     ! convergence
     IF (I .EQ. 0) EXIT
     IF (I .LT. 0) THEN
        INFO = -12
        RETURN
     END IF

     ! compute and apply the transformations
     M = 0
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,U,W,OD,N,LDG,LDU,I,LUACC) PRIVATE(J,G2,U2,P,Q,WV,WS,T,L,ES) REDUCTION(+:M) IF(LOMP)
     DO J = 1, I
        L = (N * (N - 1)) / 2
        P = OD(1,J)
        Q = OD(2,J)
#ifndef NDEBUG
        IF ((P .LE. 0) .OR. (Q .LE. P) .OR. (P .GE. N) .OR. (Q .GT. N)) THEN
           M = M + 1
           CYCLE
        END IF
#endif
        G2(1,1) = G(P,P)
        G2(2,1) = G(Q,P)
        G2(1,2) = G(P,Q)
        G2(2,2) = G(Q,Q)
        WV = (J - 1) * 6 + 1
        WS = WV + 4
        ES(1) = 0
        CALL KSVD2(G2, U2, W(WV), W(WS), ES)
        OD(2,I+J) = ES(1)
        CALL CVGPP(G2, U2, W(WV), W(WS), ES)
        OD(1,I+J) = ES(1)
        T = ES(1)
        IF (T .LT. 0) THEN
           M = M + 1
           CYCLE
        END IF
        ! transform U from the right, transpose U2, and transform G from the left
        IF (IAND(T, 2) .NE. 0) THEN
           IF (LUACC) THEN
              L = 0
              CALL ROTCA(N, N, U, LDU, P, Q, U2, L)
              IF (L .LT. 0) THEN
                 M = M + 1
                 CYCLE
              END IF
           END IF
           G2(1,1) = U2(1,1)
           G2(2,1) = U2(1,2)
           G2(1,2) = U2(2,1)
           G2(2,2) = U2(2,2)
           L = 0
           CALL ROTRA(N, N, G, LDG, P, Q, G2, L)
           IF (L .LT. 0) THEN
              M = M + 1
              CYCLE
           END IF
        END IF
     END DO
!$OMP END PARALLEL DO
     IF (M .NE. 0) THEN
        INFO = -19
        RETURN
     END IF
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,V,W,OD,N,LDG,LDV,I,LVACC) PRIVATE(J,P,Q,WV,WS,T,L) REDUCTION(+:M) IF(LOMP)
     DO J = 1, I
        L = (N * (N - 1)) / 2
        P = OD(1,J)
        Q = OD(2,J)
        WV = (J - 1) * 6 + 1
        WS = WV + 4
        T = OD(1,I+J)
        ! transform V and G from the right
        IF (IAND(T, 4) .NE. 0) THEN
           IF (LVACC) THEN
              L = 0
              CALL ROTCA(N, N, V, LDV, P, Q, W(WV), L)
              IF (L .LT. 0) THEN
                 M = M + (I + 1)
                 CYCLE
              END IF
           END IF
           L = 0
           CALL ROTCA(N, N, G, LDG, P, Q, W(WV), L)
           IF (L .LT. 0) THEN
              M = M + (I + 1)
              CYCLE
           END IF
        END IF
        ! set the new values
        G(P,P) = W(WS)
        G(Q,P) = ZERO
        G(P,Q) = ZERO
        G(Q,Q) = W(WS+1)
        IF (IAND(T, 8) .NE. 0) M = M + 1
     END DO
!$OMP END PARALLEL DO
     IF ((M .LT. 0) .OR. (M .GT. I)) THEN
        INFO = -20
        RETURN
     END IF

     IF (M .GT. 0) THEN
        ! optionally scale G
#ifdef NDEBUG
        L = 0
        !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
#else
        L = -1
        !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
#endif
        CALL LANGO(N, G, LDG, GN, L)
        IF (L .NE. 0) THEN
           INFO = -3
           RETURN
        END IF
        T = EXPONENT(HUGE(GN)) - EXPONENT(GN) - 3
        IF (T .LT. 0) THEN
           L = 0
           !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
           CALL SCALG(N, N, G, LDG, T, L)
           IF (L .NE. 0) THEN
              INFO = -3
              RETURN
           END IF
           GN = SCALE(GN, T)
           GS = GS + T
        END IF

        ! optionally scale U
        IF (LUACC .AND. (.NOT. LUSID)) THEN
#ifdef NDEBUG
           L = 0
           !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
#else
           L = -1
           !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
#endif
           CALL LANGO(N, U, LDU, UN, L)
           IF (L .NE. 0) THEN
              INFO = -5
              RETURN
           END IF
           T = EXPONENT(HUGE(UN)) - EXPONENT(UN) - 2
           IF (T .LT. 0) THEN
              L = 0
              !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
              CALL SCALG(N, N, U, LDU, T, L)
              IF (L .NE. 0) THEN
                 INFO = -5
                 RETURN
              END IF
              UN = SCALE(UN, T)
              US = US + T
           END IF
        END IF

        ! optionally scale V
        IF (LVACC .AND. (.NOT. LVSID)) THEN
#ifdef NDEBUG
           L = 0
           !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
#else
           L = -1
           !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
#endif
           CALL LANGO(N, V, LDV, VN, L)
           IF (L .NE. 0) THEN
              INFO = -7
              RETURN
           END IF
           T = EXPONENT(HUGE(VN)) - EXPONENT(VN) - 2
           IF (T .LT. 0) THEN
              L = 0
              !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
              CALL SCALG(N, N, V, LDV, T, L)
              IF (L .NE. 0) THEN
                 INFO = -7
                 RETURN
              END IF
              VN = SCALE(VN, T)
              VS = VS + T
           END IF
        END IF
     END IF
  END DO

  ! no convergence if INFO = MRQSTP
  INFO = STP

  ! extract SV from G
#ifndef NDEBUG
  I = 0
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,N,GS) PRIVATE(J) REDUCTION(MAX:I) IF(LOMP)
#else
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,N,GS) PRIVATE(J) IF(LOMP)
#endif
  DO J = 1, N
     SV(J) = G(J,J)
#ifndef NDEBUG
     IF (.NOT. (SV(J) .LE. HUGE(SV(J)))) I = MAX(I, J)
#endif
  END DO
  !$OMP END PARALLEL DO
#ifndef NDEBUG
  IF (I .NE. 0) THEN
     INFO = -9
     RETURN
  END IF
#endif
  ! backscale G, U, V
  IF (GS .NE. 0) THEN
     L = 0
     !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
     CALL SCALG(N, N, G, LDG, -GS, L)
     IF (L .NE. 0) THEN
        INFO = -3
        RETURN
     END IF
     GN = SCALE(GN, -GS)
  END IF
  IF (US .NE. 0) THEN
     L = 0
     !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
     CALL SCALG(N, N, U, LDU, -US, L)
     IF (L .NE. 0) THEN
        INFO = -5
        RETURN
     END IF
     UN = SCALE(UN, -US)
  END IF
  IF (VS .NE. 0) THEN
     L = 0
     !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
     CALL SCALG(N, N, V, LDV, -VS, L)
     IF (L .NE. 0) THEN
        INFO = -7
        RETURN
     END IF
     VN = SCALE(VN, -VS)
  END IF

  ! the first three elements of W indicate if and where an overflow occured in the backscaling above
  W(1) = GN
  W(2) = UN
  W(3) = VN
  W(4) = REAL(GS, K)
  W(5) = REAL(US, K)
  W(6) = REAL(VS, K)
