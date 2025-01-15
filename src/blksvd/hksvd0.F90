  IF (INFO .LT. 0) THEN
     MRQSTP = -(INFO + 1)
     LACC = .TRUE.
  ELSE ! INFO .GE. 0
     MRQSTP = INFO
     LACC = .FALSE.
  END IF
  INFO = 0
  LOMP = .FALSE.
  JS = IAND(JOB, 7)
  !$ IF ((JS .GE. 2) .AND. (JS .LE. 4)) LOMP = .TRUE.

  W(1) = ONE
  W(2) = ONE
  W(3) = ONE
  W(4) = ZERO
  W(5) = ZERO
  W(6) = ZERO

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
  IF (C_ASSOCIATED(CTX)) L = INT(PVN_CVIS_FRAME(CTX, G, LDF))
#endif

#ifndef NDEBUG
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(SV,N) PRIVATE(J) IF(LOMP)
  DO J = 1, N
     SV(J) = ZERO
  END DO
  !$OMP END PARALLEL DO
#endif

  IF (N .EQ. 1) THEN
     W(2) = ABS(REAL(G(1,1)))
     W(3) = ABS(AIMAG(G(1,1)))
     GN = CR_HYPOT(W(2), W(3))
     IF (.NOT. (GN .LE. HUGE(GN))) THEN
        INFO = -3
     ELSE ! finite G
        IF (LUSID) THEN
           IF (GN .NE. ZERO) THEN
              U(1,1) = CONJG(G(1,1)) / GN
           ELSE ! GN = 0
              U(1,1) = CONE
           END IF
        END IF
        IF (LVSID) V(1,1) = CONE
        G(1,1) = GN
        SV(1) = GN
        W(1) = MAX(W(2), W(3))
        W(2) = MAX(ABS(REAL(U(1,1))), ABS(AIMAG(U(1,1))))
     END IF
#ifdef ANIMATE
     IF (C_ASSOCIATED(CTX)) L = INT(PVN_CVIS_FRAME(CTX, G, LDF))
#endif
     RETURN
  END IF

  ! optionally set U and V to I
  IF (LUSID) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(U,N) IF(LOMP)
     DO J = 1, N
        DO I = 1, J-1
           U(I,J) = CZERO
        END DO
        U(J,J) = CONE
        DO I = J+1, N
           U(I,J) = CZERO
        END DO
     END DO
     !$OMP END PARALLEL DO
  END IF
  IF (LVSID) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(V,N) IF(LOMP)
     DO J = 1, N
        DO I = 1, J-1
           V(I,J) = CZERO
        END DO
        V(J,J) = CONE
        DO I = J+1, N
           V(I,J) = CZERO
        END DO
     END DO
     !$OMP END PARALLEL DO
  END IF

  IF (LACC) THEN
     ! scale G
     L = -1
     !$ IF (LOMP) L = -OMP_GET_NUM_THREADS() - 1
     CALL LANGO(N, G, LDG, GN, L)
     IF (L .NE. 0) THEN
        INFO = -3
        RETURN
     END IF
     GS = EXPONENT(HUGE(GN)) - EXPONENT(GN) - 9
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
           US = EXPONENT(HUGE(UN)) - EXPONENT(UN) - 4
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
           VS = EXPONENT(HUGE(VN)) - EXPONENT(VN) - 4
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
  ELSE ! fast
     GS = 0
     US = 0
     VS = 0
     GN = ONE
     UN = ONE
     VN = ONE
  END IF

  ! initialize the counters
  TT = 0_INT64
  TM = 0_INT64
#ifdef PRINT0UT
  WRITE (PRINT0UT,*) '"STEP","GN","UN","VN","PAIRS","OFF_G_F","BIG_TRANS"'
  FLUSH(PRINT0UT)
#endif

  DO STP = 0, MRQSTP-1
     T = STP + 1
#ifdef PRINT0UT
     WRITE (PRINT0UT,'(I10)',ADVANCE='NO') T
     WRITE (PRINT0UT,9,ADVANCE='NO') ',', GN
     WRITE (PRINT0UT,9,ADVANCE='NO') ',', UN
     WRITE (PRINT0UT,9,ADVANCE='NO') ',', VN
     FLUSH(PRINT0UT)
#endif
#ifdef ANIMATE
     IF (C_ASSOCIATED(CTX)) L = INT(PVN_CVIS_FRAME(CTX, G, LDF))
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
#ifdef PRINT0UT
     WRITE (PRINT0UT,'(A,I5)',ADVANCE='NO') ',', I
     WRITE (PRINT0UT,9,ADVANCE='NO') ',', -ONE
     FLUSH(PRINT0UT)
#endif

     ! compute and apply the transformations
     M = 0
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,U,W,R,N,LDG,LDU,I,LUACC) PRIVATE(J,G2,U2,V2,P,Q,WV,WS,L,ES) REDUCTION(MAX:M) IF(LOMP)
     DO J = 1, I
        P = R(1,J)
        Q = R(2,J)
#ifndef NDEBUG
        IF ((P .LE. 0) .OR. (Q .LE. P) .OR. (P .GE. N) .OR. (Q .GT. N)) THEN
           M = MAX(M, J * 10)
           CYCLE
        END IF
#endif
        G2(1,1) = G(P,P)
        G2(2,1) = G(Q,P)
        G2(1,2) = G(P,Q)
        G2(2,2) = G(Q,Q)
        WV = (J - 1) * 10 + 1
        WS = WV + 8
        IF ((AIMAG(G2(1,1)) .EQ. ZERO) .AND. (AIMAG(G2(2,1)) .EQ. ZERO) .AND. (AIMAG(G2(1,2)) .EQ. ZERO) .AND. &
             (AIMAG(G2(2,2)) .EQ. ZERO) .AND. (REAL(G2(1,1)) .GE. ZERO) .AND. (REAL(G2(2,1)) .EQ. ZERO) .AND. &
             (REAL(G2(1,2)) .EQ. ZERO) .AND. (REAL(G2(2,2)) .GE. ZERO) .AND. (REAL(G2(1,1)) .GE. REAL(G2(2,2)))) THEN
           R(1,I+J) = 0
           R(2,I+J) = 0
           W(WS) = REAL(G2(1,1))
           W(WS+1) = REAL(G2(2,2))
           CYCLE
        END IF
        ES(1) = 0
        CALL KSVD2(G2, U2, V2, W(WS), ES)
        R(2,I+J) = ES(1)
        CALL CVGPP(G2, U2, V2, W(WS), ES)
        R(1,I+J) = ES(1)
        IF (ES(1) .LT. 0) THEN
           M = MAX(M, J * 10 + 1)
           CYCLE
        END IF
        R(2,I+J) = 1
        W(WV) = REAL(V2(1,1))
        W(WV+1) = AIMAG(V2(1,1))
        W(WV+2) = REAL(V2(2,1))
        W(WV+3) = AIMAG(V2(2,1))
        W(WV+4) = REAL(V2(1,2))
        W(WV+5) = AIMAG(V2(1,2))
        W(WV+6) = REAL(V2(2,2))
        W(WV+7) = AIMAG(V2(2,2))
        ! transform U from the right, conjugate-transpose U2, and transform G from the left
        IF (IAND(ES(1), 2) .NE. 0) THEN
           IF (LUACC) THEN
              L = 0
              CALL ROTC(N, N, U, LDU, P, Q, U2, L)
              IF (L .LT. 0) THEN
                 M = MAX(M, J * 10 + 2)
                 CYCLE
              END IF
           END IF
           G2(1,1) = CONJG(U2(1,1))
           G2(2,1) = CONJG(U2(1,2))
           G2(1,2) = CONJG(U2(2,1))
           G2(2,2) = CONJG(U2(2,2))
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
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,V,W,R,N,LDG,LDV,I,LVACC) PRIVATE(J,V2,P,Q,WV,WS,L) REDUCTION(+:M,T) IF(LOMP)
     DO J = 1, I
        P = R(1,J)
        Q = R(2,J)
        WV = (J - 1) * 10 + 1
        WS = WV + 8
        ! transform V and G from the right
        IF (IAND(R(1,I+J), 4) .NE. 0) THEN
           V2(1,1) = CMPLX(W(WV), W(WV+1), K)
           V2(2,1) = CMPLX(W(WV+2), W(WV+3), K)
           V2(1,2) = CMPLX(W(WV+4), W(WV+5), K)
           V2(2,2) = CMPLX(W(WV+6), W(WV+7), K)
           IF (LVACC) THEN
              L = 0
              CALL ROTC(N, N, V, LDV, P, Q, V2, L)
              IF (L .LT. 0) THEN
                 M = M + (I + 1)
                 CYCLE
              END IF
           END IF
           L = 0
           CALL ROTC(N, N, G, LDG, P, Q, V2, L)
           IF (L .LT. 0) THEN
              M = M + (I + 1)
              CYCLE
           END IF
        END IF
        ! set the new values
        G(P,P) = CMPLX(W(WS), ZERO, K)
        G(Q,P) = CZERO
        G(P,Q) = CZERO
        G(Q,Q) = CMPLX(W(WS+1), ZERO, K)
        IF (IAND(R(1,I+J), 8) .NE. 0) M = M + 1
        T = T + R(2,I+J)
     END DO
!$OMP END PARALLEL DO
     IF ((M .LT. 0) .OR. (M .GT. I)) THEN
        INFO = -20
        RETURN
     END IF
#ifdef PRINT0UT
     WRITE (PRINT0UT,'(A,I5)') ',', M
     FLUSH(PRINT0UT)
#endif
     TT = TT + T
     IF (M .GT. 0) THEN
        TM = TM + M
        IF (LACC) THEN
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
           T = EXPONENT(HUGE(GN)) - EXPONENT(GN) - 9
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
              T = EXPONENT(HUGE(UN)) - EXPONENT(UN) - 4
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
              T = EXPONENT(HUGE(VN)) - EXPONENT(VN) - 4
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
     END IF
     ! convergence?
     T = STP + 1
     IF (MOD(T, NS) .EQ. 0) THEN
        L = T / NS
#ifdef PRINT0UT
        WRITE (PRINT0UT,*) 'sweep', L, ' =', TM, '/', TT, ' transf.'
        FLUSH(PRINT0UT)
#endif
        IF (TM .EQ. 0_INT64) THEN
           ! do not count a fully empty sweep
           IF (TT .EQ. 0_INT64) L = L - 1
           EXIT
        END IF
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
  IF (C_ASSOCIATED(CTX)) L = INT(PVN_CVIS_FRAME(CTX, G, LDF))
#endif

  ! extract SV from G
#ifndef NDEBUG
  I = 0
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,N,GS) PRIVATE(J) REDUCTION(MAX:I) IF(LOMP)
#else
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,N,GS) PRIVATE(J) IF(LOMP)
#endif
  DO J = 1, N
     SV(J) = REAL(G(J,J))
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
#ifdef ANIMATE
  IF (C_ASSOCIATED(CTX)) L = INT(PVN_CVIS_FRAME(CTX, G, LDF))
#endif
