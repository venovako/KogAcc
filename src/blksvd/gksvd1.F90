  L = INFO
  INFO = 0
  LOMP = .FALSE.
  JS0 = IAND(JOB, 7)
  JS1 = ISHFT(IAND(JOB, 56), -3)
  !$ IF ((JS1 .GE. 2) .AND. (JS1 .LE. 4)) LOMP = .TRUE.

#ifdef NDEBUG
  I = 6
#else
  I = NW
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(W,I) PRIVATE(J) IF(LOMP)
#endif
  DO J = 1, I
     W(J) = ZERO
  END DO
#ifndef NDEBUG
  !$OMP END PARALLEL DO
#endif

  IF (L .LT. 0) INFO = -20
  IF ((JOB .LT. 0) .OR. (JOB .GT. 1023)) INFO = -1
  IF (INFO .NE. 0) RETURN

  N = M
  I = B
  J = IAND(JOB, 63)
  INFO = K
  CALL IBDIMS(N, I, J, M_B, LW, LD, T, INFO)
  IF (INFO .GT. 0) INFO = 0
  IF (NO .LT. T) INFO = -17
  IF (ND .LT. LD) INFO = -15
  IF (NW .LT. LW) INFO = -13
  IF (LDB .LT. I) INFO = -11
  IF (LDV .LT. N) INFO = -9
  IF (LDU .LT. N) INFO = -7
  IF (LDG .LT. N) INFO = -5
  IF (M .NE. J) INFO = -2
  IF (INFO .NE. 0) RETURN

  N = 2 * B
#ifdef CLS
  LX = .FALSE.
#else
  LX = ((K .EQ. REAL64) .AND. (N .LE. 32))
#endif
  M_P = M_B / 2
  ! split W
  IGB = 1
  LB = LDB * N * M_P
  IUB = IGB + LB
  IVB = IUB + LB
  IWB = IVB + LB
  IF ((JS0 .EQ. 3) .OR. (JS0 .EQ. 6)) THEN
     LW = MAX((N - 1), 3) * N
  ELSE ! not dynamic ordering
     LW = MAX(6, (3 * N))
  END IF
  ! split D
  B_P = B * (N - 1)
  LD = B_P + 1
  ! split O
  P = M_B - 1
  IF (MOD(M_B, 2) .EQ. 0) THEN
     S = M_P * P
  ELSE ! M_B odd
     S = M_B * (P / 2)
  END IF
  IO1 = 1
  IF ((JS1 .EQ. 4) .OR. (JS1 .EQ. 7)) THEN
     IO0 = IO1 + M_P * M_B
  ELSE ! not modified modulus
     IO0 = IO1 + S
  END IF
  IF ((JS0 .EQ. 4) .OR. (JS0 .EQ. 7)) B_P = B_P + B
  IOB = IO0 + B_P
  IOD = IOB + M_B
  ! Steps and Pairs
  IF ((JS1 .EQ. 0) .OR. (JS1 .EQ. 1)) THEN
     P = 1
  ELSE ! a parallel ordering
     IF ((JS1 .EQ. 4) .OR. (JS1 .EQ. 7)) THEN
        S = M_B
     ELSE ! not modified modulus
        S = P
     END IF
     P = M_P
  END IF

  LUSID = (IAND(JOB, USID) .NE. 0)
  LUACC = (IAND(JOB, UACC) .NE. 0)
  LVSID = (IAND(JOB, VSID) .NE. 0)
  LVACC = (IAND(JOB, VACC) .NE. 0)

#ifdef ANIMATE
  CALL C_F_POINTER(C_LOC(SV), CP)
  CTX = CP
  CP => NULL()
  LDF = INT(LDG, c_size_t)
  IF (C_ASSOCIATED(CTX)) J = INT(VIS_FRAME(CTX, G, LDF))
#endif

#ifndef NDEBUG
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(SV,M) PRIVATE(J) IF(LOMP)
  DO J = 1, M
     SV(J) = ZERO
  END DO
  !$OMP END PARALLEL DO
#endif

  ! optionally set U and V to I
  IF (LUSID) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(U,M) IF(LOMP)
     DO J = 1, M
        DO I = 1, J-1
           U(I,J) = ZERO
        END DO
        U(J,J) = ONE
        DO I = J+1, M
           U(I,J) = ZERO
        END DO
     END DO
     !$OMP END PARALLEL DO
  END IF
  IF (LVSID) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(V,M) IF(LOMP)
     DO J = 1, M
        DO I = 1, J-1
           V(I,J) = ZERO
        END DO
        V(J,J) = ONE
        DO I = J+1, M
           V(I,J) = ZERO
        END DO
     END DO
     !$OMP END PARALLEL DO
  END IF

  ! scale G
  J = -1
  !$ IF (LOMP) J = -OMP_GET_NUM_THREADS() - 1
  CALL LANGO(M, G, LDG, GN, J)
  IF (J .NE. 0) THEN
     INFO = -4
     RETURN
  END IF
  D(1) = GN
  D(1) = (D(1) * N) * N
  GS = EXPONENT(HUGE(GN)) - EXPONENT(D(1)) - 2
  IF (GS .NE. 0) THEN
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
     CALL SCALG(M, M, G, LDG, GS, J)
     IF (J .NE. 0) THEN
        INFO = -4
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
        J = -1
        !$ IF (LOMP) J = -OMP_GET_NUM_THREADS() - 1
        CALL LANGO(M, U, LDU, UN, J)
        IF (J .NE. 0) THEN
           INFO = -6
           RETURN
        END IF
        D(1) = UN
        D(1) = D(1) * N
        US = EXPONENT(HUGE(UN)) - EXPONENT(D(1)) - 1
     END IF
     IF (US .NE. 0) THEN
        J = 0
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
        CALL SCALG(M, M, U, LDU, US, J)
        IF (J .NE. 0) THEN
           INFO = -6
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
        J = -1
        !$ IF (LOMP) J = -OMP_GET_NUM_THREADS() - 1
        CALL LANGO(M, V, LDV, VN, J)
        IF (J .NE. 0) THEN
           INFO = -8
           RETURN
        END IF
        D(1) = VN
        D(1) = D(1) * N
        VS = EXPONENT(HUGE(VN)) - EXPONENT(D(1)) - 1
     END IF
     IF (VS .NE. 0) THEN
        J = 0
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
        CALL SCALG(M, M, V, LDV, VS, J)
        IF (J .NE. 0) THEN
           INFO = -8
           RETURN
        END IF
        VN = SCALE(VN, VS)
     END IF
  ELSE ! .NOT. LVACC
     VN = ONE
     VS = 0
  END IF

#ifdef PRINTOUT
  WRITE (PRINTOUT,'(A,I1,A,I1,A)') '"BLK_STEP', JS1, '", "BLK_PAIRS", "MAX_STEPS", "SUM_STEPS", "GS', JS0, '"'
  FLUSH(PRINTOUT)
#endif

  DO BS = 0, L-1
     I = BS + 1
#ifdef ANIMATE
     IF (C_ASSOCIATED(CTX)) J = INT(VIS_FRAME(CTX, G, LDF))
#endif
#ifdef PRINTOUT
     WRITE (PRINTOUT,'(I11,A)',ADVANCE='NO') BS, ', '
     FLUSH(PRINTOUT)
#endif
     J = 0
     IF ((JS1 .EQ. 3) .OR. (JS1 .EQ. 6)) THEN
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
        CALL MKBPQ(M, G, LDG, B, W, D, O(1,IO1), O(1,IOB), J)
        NB = J
     ELSE ! a (quasi)-cyclic ordering
        CALL JSTEP(JS1, M_B, S, I, P, O(1,IO1), O(1,IOB), J)
        NB = P
     END IF
#ifdef PRINTOUT
     WRITE (PRINTOUT,'(I11,A)',ADVANCE='NO') NB, ', '
     FLUSH(PRINTOUT)
#endif
     IF ((J .LT. 0) .OR. (NB .LT. 0)) THEN
        INFO = -1000 * I - 100 + J
#ifdef PRINTOUT
        WRITE (PRINTOUT,'(2(I11,A))',ADVANCE='NO') 0, ', ', J, ', '
        WRITE (PRINTOUT,'(I11)') GS
        FLUSH(PRINTOUT)
#endif
        EXIT
     END IF
     ! CONVERGENCE
     IF (NB .EQ. 0) THEN
#ifdef PRINTOUT
        WRITE (PRINTOUT,'(2(I11,A))',ADVANCE='NO') 0, ', ', 0, ', '
        WRITE (PRINTOUT,'(I11)') GS
        FLUSH(PRINTOUT)
#endif
        EXIT
     END IF
     ! pack G
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
     CALL BPACK(M, G, LDG, B, W(IGB), LDB, NB, O(1,IOB), J)
     IF (J .LT. 0) THEN
        INFO = -1000 * I - 200 + J
        EXIT
     END IF
     R = IOB + NB
     IF ((JS0 .EQ. 3) .OR. (JS0 .EQ. 6)) THEN
        IF (LX) THEN
           J = -1
           !$ IF (LOMP) J = -OMP_GET_NUM_THREADS() - 1
        ELSE ! not extended
           J = 0
           !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
        END IF
        CALL BKSVDD(JS0, N, NB, W(IGB), W(IUB), W(IVB), LDB, SV, W(IWB), LW, D, LD, O(1,IOD), O(1,IO0), O(1,R), J)
     ELSE ! not dynamic ordering
        J = 0
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
        CALL BKSVD0(JS0, N, NB, W(IGB), W(IUB), W(IVB), LDB, SV, W(IWB), LW, O(1,IOD), O(1,IO0), O(1,R), J)
     END IF
     Q = J
     IF (J .LT. 0) THEN
        INFO = J
#ifdef PRINTOUT
        WRITE (PRINTOUT,'(2(I11,A))',ADVANCE='NO') J, ', ', 0, ', '
        WRITE (PRINTOUT,'(I11)') GS
        FLUSH(PRINTOUT)
#endif
        EXIT
     END IF
#ifdef PRINTOUT
     T = 0
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(O,R,NB) PRIVATE(J) REDUCTION(+:T) IF(LOMP)
     DO J = R, R+NB-1
        T = T + O(1,J)
     END DO
     !$OMP END PARALLEL DO
     WRITE (PRINTOUT,'(2(I11,A))',ADVANCE='NO') Q, ', ', T, ', '
     WRITE (PRINTOUT,'(I11)') GS
     FLUSH(PRINTOUT)
#endif
     ! CONVERGENCE
     IF (Q .EQ. 0) EXIT
     ! unpack G
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
     CALL BUNPACK(M, G, LDG, B, W(IGB), LDB, NB, O(1,IOB), J)
     IF (J .LT. 0) THEN
        INFO = -1000 * I - 300 + J
        EXIT
     END IF
     IF (LUACC) THEN
        T = LDU
     ELSE ! .NOT. LUACC
        T = -LDU
     END IF
     IF (LVACC) THEN
        R = LDV
     ELSE ! .NOT. LVACC
        R = -LDV
     END IF
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
     CALL BUPDATE(M, B, G, LDG, U, T, V, R, W(IGB), W(IUB), W(IVB), LDB, NB, O(1,IOB), J)
     IF (J .LT. 0) THEN
        INFO = -1000 + J
        EXIT
     END IF
     ! optionally scale G
     J = -1
     !$ IF (LOMP) J = -OMP_GET_NUM_THREADS() - 1
     CALL LANGO(M, G, LDG, GN, J)
     IF (J .NE. 0) THEN
        INFO = -1000 * I - 400 + J
        EXIT
     END IF
     D(1) = GN
     D(1) = (D(1) * N) * N
     T = EXPONENT(HUGE(GN)) - EXPONENT(D(1)) - 2
     IF (T .LT. 0) THEN
        J = 0
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
        CALL SCALG(M, M, G, LDG, T, J)
        IF (J .NE. 0) THEN
           INFO = -1000 * I - 500 + J
           EXIT
        END IF
        GN = SCALE(GN, T)
        GS = GS + T
     END IF
     ! optionally scale U
     IF (LUACC .AND. (.NOT. LUSID)) THEN
#ifdef NDEBUG
        J = 0
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
#else
        J = -1
        !$ IF (LOMP) J = -OMP_GET_NUM_THREADS() - 1
#endif
        CALL LANGO(M, U, LDU, UN, J)
        IF (J .NE. 0) THEN
           INFO = -1000 * I - 600 + J
           EXIT
        END IF
        D(1) = UN
        D(1) = D(1) * N
        T = EXPONENT(HUGE(UN)) - EXPONENT(D(1)) - 1
        IF (T .LT. 0) THEN
           J = 0
           !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
           CALL SCALG(M, M, U, LDU, T, J)
           IF (J .NE. 0) THEN
              INFO = -1000 * I - 700 + J
              EXIT
           END IF
           UN = SCALE(UN, T)
           US = US + T
        END IF
     END IF
     ! optionally scale V
     IF (LVACC .AND. (.NOT. LVSID)) THEN
#ifdef NDEBUG
        J = 0
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
#else
        J = -1
        !$ IF (LOMP) J = -OMP_GET_NUM_THREADS() - 1
#endif
        CALL LANGO(M, V, LDV, VN, J)
        IF (J .NE. 0) THEN
           INFO = -1000 * I - 800 + J
           EXIT
        END IF
        D(1) = VN
        D(1) = D(1) * N
        T = EXPONENT(HUGE(VN)) - EXPONENT(D(1)) - 1
        IF (T .LT. 0) THEN
           J = 0
           !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
           CALL SCALG(M, M, V, LDV, T, J)
           IF (J .NE. 0) THEN
              INFO = -1000 * I - 900 + J
              EXIT
           END IF
           VN = SCALE(VN, T)
           VS = VS + T
        END IF
     END IF
  END DO
  IF (INFO .NE. 0) RETURN
  INFO = BS

  ! extract SV from G
#ifndef NDEBUG
  I = 0
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,M) PRIVATE(J) REDUCTION(MAX:I) IF(LOMP)
#else
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,M) PRIVATE(J) IF(LOMP)
#endif
  DO J = 1, M
     SV(J) = G(J,J)
#ifndef NDEBUG
     IF (.NOT. (SV(J) .LE. HUGE(SV(J)))) I = MAX(I, J)
#endif
  END DO
  !$OMP END PARALLEL DO
#ifndef NDEBUG
  IF (I .NE. 0) THEN
     INFO = -10
     RETURN
  END IF
#endif
  ! backscale G, U, V
  IF (GS .NE. 0) THEN
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
     CALL SCALG(M, M, G, LDG, -GS, J)
     IF (J .NE. 0) THEN
        INFO = -4
        RETURN
     END IF
     GN = SCALE(GN, -GS)
  END IF
  IF (US .NE. 0) THEN
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
     CALL SCALG(M, M, U, LDU, -US, J)
     IF (J .NE. 0) THEN
        INFO = -6
        RETURN
     END IF
     UN = SCALE(UN, -US)
  END IF
  IF (VS .NE. 0) THEN
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
     CALL SCALG(M, M, V, LDV, -VS, J)
     IF (J .NE. 0) THEN
        INFO = -8
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
  IF (C_ASSOCIATED(CTX)) J = INT(VIS_FRAME(CTX, G, LDF))
#endif
