  L = INFO
  INFO = 0
  LOMP = .FALSE.
  IF (L .LT. 0) THEN
     L = -(L + 1)
  ELSE ! L .GE. 0
     !$ LOMP = .TRUE.
     CONTINUE
  END IF

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

  JS = IAND(JOB, 7)
  IF ((JOB .LT. 0) .OR. (JOB .GT. 124) .OR. (JS .GT. 4)) THEN
     INFO = -1
     RETURN
  END IF

  N = M
  I = B
  J = JS
  INFO = -K
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
  M_P = M_B / 2
  ! split W
  LB = LDB * N * M_P * 2
  IGB = 1
  IUB = IGB + LB
  IVB = IUB + LB
  IWB = IVB + LB
  LW = MAX((N - 1), 5) * N
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
  IF (JS .EQ. 4) THEN
     IO0 = IO1 + M_P * M_B
  ELSE ! JS .NE. 4
     IO0 = IO1 + S
  END IF
  IOB = IO0 + B_P
  IOD = IOB + M_B
  ! Steps and Pairs
  IF ((JS .EQ. 0) .OR. (JS .EQ. 1)) THEN
     P = 1
  ELSE ! a parallel ordering
     IF (JS .EQ. 4) THEN
        S = M_B
     ELSE ! (JS .EQ. 2) .OR. (JS .EQ. 3)
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
        US = EXPONENT(HUGE(UN)) - EXPONENT(D(1)) - 2
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
        VS = EXPONENT(HUGE(VN)) - EXPONENT(D(1)) - 2
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

!#ifndef NDEBUG
  WRITE (ERROR_UNIT,'(A,I1,A)') '"BLK_STEP', JS, '", "BLK_PAIRS", "MAX_STEPS", "SUM_STEPS", "GS"'
  FLUSH(ERROR_UNIT)
!#endif

  DO BS = 0, L-1
     I = BS + 1
#ifdef ANIMATE
     IF (C_ASSOCIATED(CTX)) J = INT(VIS_FRAME(CTX, G, LDF))
#endif
!#ifndef NDEBUG
     WRITE (ERROR_UNIT,'(I11,A)',ADVANCE='NO') BS, ', '
     FLUSH(ERROR_UNIT)
!#endif
     J = 0
     IF (JS .EQ. 3) THEN
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
        CALL MKBPQ(M, G, LDG, B, W, D, O(1,IO1), O(1,IOB), J)
        NB = J
     ELSE ! a (quasi)-cyclic ordering
        CALL JSTEP(JS, M_B, S, I, P, O(1,IO1), O(1,IOB), J)
        NB = P
     END IF
!#ifndef NDEBUG
     WRITE (ERROR_UNIT,'(I11,A)',ADVANCE='NO') NB, ', '
     FLUSH(ERROR_UNIT)
!#endif
     IF ((J .LT. 0) .OR. (NB .LT. 0)) THEN
        INFO = -1000 * I - 100 + J
!#ifndef NDEBUG
        WRITE (ERROR_UNIT,'(2(I11,A))',ADVANCE='NO') 0, ', ', J, ', '
        WRITE (ERROR_UNIT,'(I11)') GS
        FLUSH(ERROR_UNIT)
!#endif
        EXIT
     END IF
     ! CONVERGENCE
     IF (NB .EQ. 0) THEN
!#ifndef NDEBUG
        WRITE (ERROR_UNIT,'(2(I11,A))',ADVANCE='NO') 0, ', ', 0, ', '
        WRITE (ERROR_UNIT,'(I11)') GS
        FLUSH(ERROR_UNIT)
!#endif
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
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
     CALL BKSVDD(N, NB, W(IGB), W(IUB), W(IVB), LDB, SV, W(IWB), LW, D, LD, O(1,IOD), O(1,IO0), O(1,R), J)
     Q = J
     IF (J .LT. 0) THEN
        INFO = J
!#ifndef NDEBUG
        WRITE (ERROR_UNIT,'(2(I11,A))',ADVANCE='NO') J, ', ', 0, ', '
        WRITE (ERROR_UNIT,'(I11)') GS
        FLUSH(ERROR_UNIT)
!#endif
        EXIT
     END IF
!#ifndef NDEBUG
     T = 0
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(O,R,NB) PRIVATE(J) REDUCTION(+:T) IF(LOMP)
     DO J = R, R+NB-1
        T = T + O(1,J)
     END DO
     !$OMP END PARALLEL DO
     WRITE (ERROR_UNIT,'(2(I11,A))',ADVANCE='NO') Q, ', ', T, ', '
     WRITE (ERROR_UNIT,'(I11)') GS
     FLUSH(ERROR_UNIT)
!#endif
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
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
     CALL BUPDATE(M, B, G, LDG, U, LDU, V, LDV, W(IGB), W(IUB), W(IVB), LDB, NB, O(1,IOB), J)
     IF (J .LT. 0) THEN
        INFO = -1000 + J
        EXIT
     END IF
     ! optionally scale G
     J = 0
     !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
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
        J = 0
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
        CALL LANGO(M, U, LDU, UN, J)
        IF (J .NE. 0) THEN
           INFO = -1000 * I - 600 + J
           EXIT
        END IF
        D(1) = UN
        D(1) = D(1) * N
        T = EXPONENT(HUGE(UN)) - EXPONENT(D(1)) - 2
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
        J = 0
        !$ IF (LOMP) J = OMP_GET_NUM_THREADS()
        CALL LANGO(M, V, LDV, VN, J)
        IF (J .NE. 0) THEN
           INFO = -1000 * I - 800 + J
           EXIT
        END IF
        D(1) = VN
        D(1) = D(1) * N
        T = EXPONENT(HUGE(VN)) - EXPONENT(D(1)) - 2
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
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,M) PRIVATE(J) IF(LOMP)
  DO J = 1, M
     SV(J) = REAL(G(J,J))
  END DO
  !$OMP END PARALLEL DO

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
