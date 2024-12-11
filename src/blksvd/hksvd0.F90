  MRQSTP = INFO
  INFO = 0
  LOMP = .FALSE.
  IF (MRQSTP .LT. 0) THEN
     MRQSTP = -(MRQSTP + 1)
  ELSE ! MRQSTP >= 0
     !$ LOMP = .TRUE.
     CONTINUE
  END IF

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
  IF (JOB .LT. 0) INFO = -1
  IF (JOB .GT. 124) INFO = -1
  JS = IAND(JOB, 7)
  M = N * (N - 1)
  M_2 = M / 2
  SELECT CASE (JS)
  CASE (0,1) ! row/column cyclic
     NP = 1
     NS = M_2
  CASE (2) ! generalized Mantharam-Eberlein
     IF (MOD(N, 2) .EQ. 0) THEN
        NP = N / 2
        NS = N - 1
     ELSE ! N odd
        INFO = -2
     END IF
  CASE (3) ! dynamic ordering
     NP = N / 2
     IF ((N .GE. 2) .AND. (O(1) .GT. 0)) NP = MIN(NP, O(1))
     NS = 1
  CASE (4) ! modified modulus
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
        SV(1) = REAL(GN, REAL128)
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

  ! scale G
  L = 0
  !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
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
        L = 0
        !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
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
        L = 0
        !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
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

  ! initialize the counters
  TT = 0_INT64
  TM = 0_INT64
  SM = 0_INT64
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
     IF (C_ASSOCIATED(CTX)) L = INT(PVN_CVIS_FRAME(CTX, G, LDF))
#endif

     ! build the current step's pairs
     IF (JS .EQ. 3) THEN
        L = 0
        !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
        CALL MK3PQ(NP, N, G, LDG, W, O, L)
        IF (L .LT. 0) THEN
           INFO = -10
           RETURN
        END IF
        I = L
     ELSE ! tabular O
        W(M_2 + 1) = -ONE
        I = NP
     END IF
     IF (I .GT. 0) THEN
        L = 0
        !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
        CALL JSTEP(JS, N, NS, T, I, O, R, L)
        IF (L .NE. 0) THEN
           INFO = -11
           RETURN
        END IF
     END IF
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,'(A,I5)',ADVANCE='NO') ',', I
     WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', W(M_2 + 1)
     FLUSH(OUTPUT_UNIT)
#endif
     IF (I .EQ. 0) THEN
        ! convergence
#ifndef NDEBUG
        WRITE (OUTPUT_UNIT,'(A,I5)') ',', 0
        FLUSH(OUTPUT_UNIT)
#endif
        EXIT
     ELSE IF (I .GT. 0) THEN
        TT = TT + I
     ELSE ! should never happen
        INFO = -12
        RETURN
     END IF

     ! compute and apply the transformations
     M = 0
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,U,W,R,N,LDG,LDU,I,LUACC) PRIVATE(G2,U2,V2,P,Q,WV,WS,T,L,ES) REDUCTION(+:M) IF(LOMP)
     DO J = 1, I
        P = R(1,J)
        Q = R(2,J)
        IF ((P .LE. 0) .OR. (Q .LE. P) .OR. (P .GE. N) .OR. (Q .GT. N)) THEN
           M = M + 1
           CYCLE
        END IF
        G2(1,1) = G(P,P)
        G2(2,1) = G(Q,P)
        G2(1,2) = G(P,Q)
        G2(2,2) = G(Q,Q)
        WV = (J - 1) * 10 + 1
        WS = WV + 8
        ES(1) = 0
        CALL KSVD2(G2, U2, V2, W(WS), ES)
        R(2,I+J) = ES(1)
        CALL CVGPP(G2, U2, V2, W(WS), ES)
        R(1,I+J) = ES(1)
        W(WV) = REAL(V2(1,1))
        W(WV+1) = AIMAG(V2(1,1))
        W(WV+2) = REAL(V2(2,1))
        W(WV+3) = AIMAG(V2(2,1))
        W(WV+4) = REAL(V2(1,2))
        W(WV+5) = AIMAG(V2(1,2))
        W(WV+6) = REAL(V2(2,2))
        W(WV+7) = AIMAG(V2(2,2))
        T = ES(1)
        IF (T .LT. 0) THEN
           M = M + 1
           CYCLE
        END IF
        ! transform U from the right, conjugate-transpose U2, and transform G from the left
        IF (IAND(T, 2) .NE. 0) THEN
           IF (LUACC) THEN
              L = 0
              CALL ROTC(N, N, U, LDU, P, Q, U2, L)
              IF (L .LT. 0) THEN
                 M = M + 1
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
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,V,W,R,N,LDG,LDV,I,LVACC) PRIVATE(V2,P,Q,WV,WS,T,L) REDUCTION(+:M) IF(LOMP)
     DO J = 1, I
        P = R(1,J)
        Q = R(2,J)
        WV = (J - 1) * 10 + 1
        WS = WV + 8
        T = R(1,I+J)
        ! transform V and G from the right
        IF (IAND(T, 4) .NE. 0) THEN
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
        IF (IAND(T, 8) .NE. 0) M = M + 1
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
     TM = TM + M

     ! optionally scale G
     L = 0
     !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
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
        L = 0
        !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
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
        L = 0
        !$ IF (LOMP) L = OMP_GET_NUM_THREADS()
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

     ! check for convergence
     IF (JS .NE. 3) THEN
        SM = SM + M
        T = STP + 1
        IF (MOD(T, NS) .EQ. 0) THEN
           L = T / NS
#ifndef NDEBUG
           WRITE (OUTPUT_UNIT,*) 'sweep', L, ' completed with:', SM, ' big transformations'
           FLUSH(OUTPUT_UNIT)
#endif
           IF (SM .EQ. 0_INT64) EXIT
           SM = 0_INT64
        END IF
     END IF
  END DO

  ! no convergence if INFO = MRQSTP
  INFO = STP
#ifndef NDEBUG
  WRITE (OUTPUT_UNIT,*) 'exited after:', TT, ' transformations, of which big:', TM
  FLUSH(OUTPUT_UNIT)
#endif
#ifdef ANIMATE
  IF (C_ASSOCIATED(CTX)) L = INT(PVN_CVIS_FRAME(CTX, G, LDF))
#endif

  ! extract SV from G with a safe backscaling
  I = 0
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,N,GS) REDUCTION(MAX:I) IF(LOMP)
  DO J = 1, N
     SV(J) = SCALE(REAL(REAL(G(J,J)), REAL128), -GS)
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
  IF (C_ASSOCIATED(CTX)) L = INT(PVN_CVIS_FRAME(CTX, G, LDF))
#endif
