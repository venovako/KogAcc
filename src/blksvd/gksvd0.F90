  MRQSTP = INFO
  INFO = 0
  LOMP = .FALSE.
  JS = IAND(JOB, 7)
  !$ IF ((JS .GE. 2) .AND. (JS .LE. 4) .AND. (N .GE. 4)) LOMP = .TRUE.

  W(1) = ONE
  W(2) = ONE
  W(3) = ONE
  W(4) = ZERO
  W(5) = ZERO
  W(6) = ZERO

  IF (MRQSTP .LT. 0) INFO = -13
  IF (LDV .LT. N) INFO = -8
  IF (LDU .LT. N) INFO = -6
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF ((JOB .LT. 0) .OR. (JOB .GT. 1023)) INFO = -1
  M = N * (N - 1)
  M_2 = M / 2
  SELECT CASE (JS)
  CASE (0,1) ! row/column cyclic
     NP = 1
     NS = M_2
  CASE (2,5) ! generalized Mantharam-Eberlein
     IF (MOD(N, 2) .EQ. 0) THEN
        NP = N / 2
        NS = N - 1
     ELSE ! N odd
        INFO = -2
     END IF
  CASE (4,7) ! modified modulus
     IF (MOD(N, 2) .EQ. 0) THEN
        NP = N / 2
        NS = N
     ELSE ! N odd
        INFO = -2
     END IF
  CASE DEFAULT
     INFO = -1
  END SELECT
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  LUSID = (IAND(JOB, USID) .NE. 0)
  LUACC = (IAND(JOB, UACC) .NE. 0)
  LVSID = (IAND(JOB, VSID) .NE. 0)
  LVACC = (IAND(JOB, VACC) .NE. 0)

#ifdef ANIMATE
  CALL C_F_POINTER(C_LOC(SV), CP)
  CTX = CP
  CP => NULL()
  LDF = INT(LDG, c_size_t)
  IF (C_ASSOCIATED(CTX)) L = INT(PVN_RVIS_FRAME(CTX, G, LDF))
#endif

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
#ifdef ANIMATE
     IF (C_ASSOCIATED(CTX)) L = INT(PVN_RVIS_FRAME(CTX, G, LDF))
#endif
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

  ! initialize the counters
  TM = 0_INT64
  TT = 0_INT64
#ifndef NDEBUG
  WRITE (OUTPUT_UNIT,*) '"STEP","GN","UN","VN","PAIRS","OFF_G_F","BIG_TRANS"'
  FLUSH(OUTPUT_UNIT)
#endif

  DO STP = 0, MRQSTP-1
     T = STP + 1
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,'(I10)',ADVANCE='NO') T
     WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', GN
     WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', UN
     WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', VN
     FLUSH(OUTPUT_UNIT)
#endif
#ifdef ANIMATE
     IF (C_ASSOCIATED(CTX)) L = INT(PVN_RVIS_FRAME(CTX, G, LDF))
#endif

     ! build the current step's pairs
     I = NP
     L = 0
     !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
     CALL JSTEP(JS, N, NS, T, I, O, R, L)
     IF (L .NE. 0) THEN
        INFO = -11
        RETURN
     END IF
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,'(A,I5)',ADVANCE='NO') ',', I
     WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', -ONE
     FLUSH(OUTPUT_UNIT)
#endif

     ! compute and apply the transformations
     M = 0
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,U,W,R,N,LDG,LDU,I,LUACC) PRIVATE(J,G2,U2,P,Q,WV,WS,L,ES) REDUCTION(MAX:M) IF(LOMP)
     DO J = 1, I
        P = R(1,J)
        Q = R(2,J)
        IF ((P .LE. 0) .OR. (Q .LE. P) .OR. (P .GE. N) .OR. (Q .GT. N)) THEN
           M = MAX(M, J * 10)
           CYCLE
        END IF
        G2(1,1) = G(P,P)
        G2(2,1) = G(Q,P)
        G2(1,2) = G(P,Q)
        G2(2,2) = G(Q,Q)
        WV = (J - 1) * 6 + 1
        WS = WV + 4
        IF ((G2(1,1) .GE. ZERO) .AND. (G2(2,1) .EQ. ZERO) .AND. (G2(1,2) .EQ. ZERO) .AND. (G2(2,2) .GE. ZERO) .AND. &
             (G2(1,1) .GE. G2(2,2))) THEN
           R(1,I+J) = 0
           R(2,I+J) = 0
           W(WS) = G2(1,1)
           W(WS+1) = G2(2,2)
           CYCLE
        END IF
        ES(1) = 0
        CALL KSVD2(G2, U2, W(WV), W(WS), ES)
        R(2,I+J) = ES(1)
        CALL CVGPP(G2, U2, W(WV), W(WS), ES)
        R(1,I+J) = ES(1)
        IF (ES(1) .LT. 0) THEN
           M = MAX(M, J * 10 + 1)
           CYCLE
        END IF
        R(2,I+J) = 1
        ! transform U from the right, transpose U2, and transform G from the left
        IF (IAND(ES(1), 2) .NE. 0) THEN
           IF (LUACC) THEN
              L = 0
              CALL ROTC(N, N, U, LDU, P, Q, U2, L)
              IF (L .LT. 0) THEN
                 M = MAX(M, J * 10 + 2)
                 CYCLE
              END IF
           END IF
           G2(1,1) = U2(1,1)
           G2(2,1) = U2(1,2)
           G2(1,2) = U2(2,1)
           G2(2,2) = U2(2,2)
           L = 0
           CALL ROTR(N, N, G, LDG, P, Q, G2, L)
           IF (L .LT. 0) THEN
              M = MAX(M, J * 10 + 3)
              CYCLE
           END IF
        END IF
     END DO
!$OMP END PARALLEL DO
     IF (M .NE. 0) THEN
        INFO = -19
        RETURN
     END IF
     T = 0
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,V,W,R,N,LDG,LDV,I,LVACC) PRIVATE(J,P,Q,WV,WS,L) REDUCTION(+:M,T) IF(LOMP)
     DO J = 1, I
        P = R(1,J)
        Q = R(2,J)
        WV = (J - 1) * 6 + 1
        WS = WV + 4
        ! transform V and G from the right
        IF (IAND(R(1,I+J), 4) .NE. 0) THEN
           IF (LVACC) THEN
              L = 0
              CALL ROTC(N, N, V, LDV, P, Q, W(WV), L)
              IF (L .LT. 0) THEN
                 M = M + (I + 1)
                 CYCLE
              END IF
           END IF
           L = 0
           CALL ROTC(N, N, G, LDG, P, Q, W(WV), L)
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
        IF (IAND(R(1,I+J), 8) .NE. 0) M = M + 1
        T = T + R(2,I+J)
     END DO
!$OMP END PARALLEL DO
     IF ((M .LT. 0) .OR. (M .GT. I)) THEN
        INFO = -20
        RETURN
     END IF
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,'(A,I5)') ',', M
     FLUSH(OUTPUT_UNIT)
#endif
     TT = TT + T
     IF (M .GT. 0) THEN
        TM = TM + M
        ! optionally scale G
        L = -1
        !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
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
           L = -1
           !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
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
           L = -1
           !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
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
     ! convergence?
     T = STP + 1
     IF (MOD(T, NS) .EQ. 0) THEN
        L = T / NS
#ifndef NDEBUG
        WRITE (OUTPUT_UNIT,*) 'sweep', L, ' =', TM, '/', TT, ' transf.'
        FLUSH(OUTPUT_UNIT)
#endif
        IF (TM .EQ. 0_INT64) EXIT
        TM = 0_INT64
        TT = 0_INT64
     END IF
  END DO

  ! no convergence if INFO = MRQSTP
  IF (STP .GE. MRQSTP) THEN
     INFO = MRQSTP
  ELSE ! convergence in the terms of sweeps
     INFO = L
  END IF
#ifdef ANIMATE
  IF (C_ASSOCIATED(CTX)) L = INT(PVN_RVIS_FRAME(CTX, G, LDF))
#endif

  ! extract SV from G
  I = 0
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,N,GS) PRIVATE(J) REDUCTION(MAX:I) IF(LOMP)
  DO J = 1, N
     SV(J) = G(J,J)
     IF (.NOT. (SV(J) .LE. HUGE(SV(J)))) THEN
        I = MAX(I, J)
     ELSE ! SV(J) finite
        I = MAX(I, 0)
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (I .NE. 0) THEN
     INFO = -9
     RETURN
  END IF

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
#ifdef ANIMATE
  IF (C_ASSOCIATED(CTX)) L = INT(PVN_RVIS_FRAME(CTX, G, LDF))
#endif
