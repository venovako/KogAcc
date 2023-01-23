  MRQSTP = INFO
  INFO = 0
  LOMP = .FALSE.
  IF (MRQSTP .LT. 0) THEN
     MRQSTP = -(MRQSTP + 1)
  ELSE ! MRQSTP >= 0
     !$ LOMP = .TRUE.
     CONTINUE
  END IF

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

  IF (N .EQ. 1) THEN
     GN = ABS(G(1,1))
     IF (.NOT. (GN .LE. HUGE(GN))) THEN
        INFO = -3
     ELSE ! finite G
        IF (LUSID) U(1,1) = SIGN(ONE, G(1,1))
        IF (LVSID) V(1,1) = ONE
        G(1,1) = GN
        SV(1) = REAL(GN, REAL128)
        W(1) = GN
        W(2) = ONE
        W(3) = ONE
     END IF
     RETURN
  END IF

  ! optionally set U and V to I
  IF (LOMP) THEN
     IF (LUSID) THEN
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(U,N)
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
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(V,N)
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
  ELSE ! sequentially
     IF (LUSID) THEN
        DO J = 1, N
           DO I = 1, J-1
              U(I,J) = ZERO
           END DO
           U(J,J) = ONE
           DO I = J+1, N
              U(I,J) = ZERO
           END DO
        END DO
     END IF
     IF (LVSID) THEN
        DO J = 1, N
           DO I = 1, J-1
              V(I,J) = ZERO
           END DO
           V(J,J) = ONE
           DO I = J+1, N
              V(I,J) = ZERO
           END DO
        END DO
     END IF
  END IF

  IF (W(1) .GT. ZERO) THEN
     XSG = CEILING(W(1))
  ELSE IF (W(1) .LT. ZERO) THEN
     XSG = FLOOR(W(1))
  ELSE ! W(1) = 0 or NaN
     XSG = 0
  END IF

  ! scale G
  !$ L = OMP_GET_NUM_THREADS()
  IF (.NOT. LOMP) L = 0
  CALL LANGO('N', N, G, LDG, GN, L)
  IF (L .NE. 0) THEN
     INFO = -3
     RETURN
  END IF
  IF (XSG .EQ. 0) THEN
     GS = EXPONENT(HUGE(GN)) - EXPONENT(GN) - 3
  ELSE IF (XSG .GT. 0) THEN
     GS = XSG
  ELSE ! XSG < 0
     GS = XSG + 1
  END IF
  IF (GS .NE. 0) THEN
     !$ L = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) L = 0
     CALL SCALG(N, N, G, LDG, GS, L)
     IF (L .NE. 0) THEN
        INFO = -3
        RETURN
     END IF
     GN = SCALE(GN, GS)
  END IF

  IF (W(2) .GT. ZERO) THEN
     XSU = CEILING(W(2))
  ELSE IF (W(2) .LT. ZERO) THEN
     XSU = FLOOR(W(2))
  ELSE ! W(2) = 0 or NaN
     XSU = 0
  END IF

  ! optionally scale U
  IF (LUACC) THEN
     IF (LUSID) THEN
        UN = ONE
        US = 0
     ELSE ! scaling of U might be required
        !$ L = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) L = 0
        CALL LANGO('N', N, U, LDU, UN, L)
        IF (L .NE. 0) THEN
           INFO = -5
           RETURN
        END IF
        IF (XSU .EQ. 0) THEN
           US = EXPONENT(HUGE(UN)) - EXPONENT(UN) - 2
        ELSE IF (XSU .GT. 0) THEN
           US = XSU
        ELSE ! XSU < 0
           US = XSU + 1
        END IF
     END IF
     IF (US .NE. 0) THEN
        !$ L = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) L = 0
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

  IF (W(3) .GT. ZERO) THEN
     XSV = CEILING(W(3))
  ELSE IF (W(3) .LT. ZERO) THEN
     XSV = FLOOR(W(3))
  ELSE ! W(3) = 0 or NaN
     XSV = 0
  END IF

  ! optionally scale V
  IF (LVACC) THEN
     IF (LVSID) THEN
        VN = ONE
        VS = 0
     ELSE ! scaling of V might be required
        !$ L = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) L = 0
        CALL LANGO('N', N, V, LDV, VN, L)
        IF (L .NE. 0) THEN
           INFO = -7
           RETURN
        END IF
        IF (XSV .EQ. 0) THEN
           VS = EXPONENT(HUGE(VN)) - EXPONENT(VN) - 2
        ELSE IF (XSV .GT. 0) THEN
           VS = XSV
        ELSE ! XSV < 0
           VS = XSV + 1
        END IF
     END IF
     IF (VS .NE. 0) THEN
        !$ L = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) L = 0
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

  ! associate R with SV
  CALL C_F_POINTER(C_LOC(SV), R, [2,2*NP])
  IF (.NOT. ASSOCIATED(R)) THEN
     INFO = -9
     RETURN
  END IF

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

     ! build the current step's pairs
     IF (JS .EQ. 3) THEN
        !$ L = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) L = 0
        CALL MK3PQ(NP, N, G, LDG, W, O, L)
        IF (L .LT. 0) THEN
           INFO = -10
           RETURN
        END IF
        I = L
     ELSE ! tabular O
#ifdef NDEBUG
        W(M_2 + 1) = -ONE
#else
        CALL LANGO('O', N, G, LDG, W(M_2 + 1), L)
#endif
        I = NP
     END IF
     IF (I .GT. 0) THEN
        !$ L = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) L = 0
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
     IF (LOMP .AND. (I .GT. 1)) THEN
        !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,U,W,R,N,LDG,LDU,I,XSG,LUACC) PRIVATE(G2,U2,P,Q,WV,WS,T,L) REDUCTION(+:M)
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
           WV = (J - 1) * 6 + 1
           WS = WV + 4
           IF (XSG .EQ. 0) THEN
              T = 0
           ELSE ! no inner scaling
              T = -1
           END IF
           CALL KSVD2(G2, U2, W(WV), W(WS), T)
           R(2,I+J) = T
           CALL CVGPP(G2, U2, W(WV), W(WS), T)
           R(1,I+J) = T
           IF (T .LT. 0) THEN
              M = M + 1
              CYCLE
           END IF
           ! transform U from the right, transpose U2, and transform G from the left
           IF (IAND(T, 2) .NE. 0) THEN
              IF (LUACC) THEN
                 !$ L = OMP_GET_NUM_THREADS()
                 CALL ROTC(N, N, U, LDU, P, Q, U2, L)
                 IF (L .NE. 0) THEN
                    M = M + 1
                    CYCLE
                 END IF
              END IF
              G2(1,1) = U2(1,1)
              G2(2,1) = U2(1,2)
              G2(1,2) = U2(2,1)
              G2(2,2) = U2(2,2)
              !$ L = OMP_GET_NUM_THREADS()
              CALL ROTR(N, N, G, LDG, P, Q, G2, L)
              IF (L .NE. 0) THEN
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
        !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,V,W,R,N,LDG,LDV,I,LVACC) PRIVATE(P,Q,WV,WS,T,L) REDUCTION(+:M)
        DO J = 1, I
           P = R(1,J)
           Q = R(2,J)
           WV = (J - 1) * 6 + 1
           WS = WV + 4
           T = R(1,I+J)
           ! transform V and G from the right
           IF (IAND(T, 4) .NE. 0) THEN
              IF (LVACC) THEN
                 !$ L = OMP_GET_NUM_THREADS()
                 CALL ROTC(N, N, V, LDV, P, Q, W(WV), L)
                 IF (L .NE. 0) THEN
                    M = M + (I + 1)
                    CYCLE
                 END IF
              END IF
              !$ L = OMP_GET_NUM_THREADS()
              CALL ROTC(N, N, G, LDG, P, Q, W(WV), L)
              IF (L .NE. 0) THEN
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
     ELSE ! sequentially
        DO J = 1, I
           P = R(1,J)
           Q = R(2,J)
           IF ((P .LE. 0) .OR. (Q .LE. P) .OR. (P .GE. N) .OR. (Q .GT. N)) THEN
              INFO = -13
              RETURN
           END IF
           G2(1,1) = G(P,P)
           G2(2,1) = G(Q,P)
           G2(1,2) = G(P,Q)
           G2(2,2) = G(Q,Q)
           WV = (J - 1) * 6 + 1
           WS = WV + 4
           IF (XSG .EQ. 0) THEN
              T = 0
           ELSE ! no inner scaling
              T = -1
           END IF
           CALL KSVD2(G2, U2, W(WV), W(WS), T)
           R(2,I+J) = T
           CALL CVGPP(G2, U2, W(WV), W(WS), T)
           R(1,I+J) = T
           IF (T .LT. 0) THEN
              INFO = -14
              RETURN
           END IF
           ! transform U from the right, transpose U2, and transform G from the left
           IF (IAND(T, 2) .NE. 0) THEN
              IF (LUACC) THEN
                 L = 0
                 CALL ROTC(N, N, U, LDU, P, Q, U2, L)
                 IF (L .NE. 0) THEN
                    INFO = -15
                    RETURN
                 END IF
              END IF
              G2(1,1) = U2(1,1)
              G2(2,1) = U2(1,2)
              G2(1,2) = U2(2,1)
              G2(2,2) = U2(2,2)
              L = 0
              CALL ROTR(N, N, G, LDG, P, Q, G2, L)
              IF (L .NE. 0) THEN
                 INFO = -16
                 RETURN
              END IF
           END IF
           ! transform V and G from the right
           IF (IAND(T, 4) .NE. 0) THEN
              IF (LVACC) THEN
                 L = 0
                 CALL ROTC(N, N, V, LDV, P, Q, W(WV), L)
                 IF (L .NE. 0) THEN
                    INFO = -17
                    RETURN
                 END IF
              END IF
              L = 0
              CALL ROTC(N, N, G, LDG, P, Q, W(WV), L)
              IF (L .NE. 0) THEN
                 INFO = -18
                 RETURN
              END IF
           END IF
           ! set the new values
           G(P,P) = W(WS)
           G(Q,P) = ZERO
           G(P,Q) = ZERO
           G(Q,Q) = W(WS+1)
           IF (IAND(T, 8) .NE. 0) M = M + 1
        END DO
     END IF
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,'(A,I5)') ',', M
     FLUSH(OUTPUT_UNIT)
#endif

     IF (M .GT. 0) THEN
        TM = TM + M

        ! optionally scale G
        IF (XSG .EQ. 0) THEN
           !$ L = OMP_GET_NUM_THREADS()
           IF (.NOT. LOMP) L = 0
           CALL LANGO('N', N, G, LDG, GN, L)
           IF (L .NE. 0) THEN
              INFO = -3
              RETURN
           END IF
           T = EXPONENT(HUGE(GN)) - EXPONENT(GN) - 3
           IF (T .LT. 0) THEN
              !$ L = OMP_GET_NUM_THREADS()
              IF (.NOT. LOMP) L = 0
              CALL SCALG(N, N, G, LDG, T, L)
              IF (L .NE. 0) THEN
                 INFO = -3
                 RETURN
              END IF
              GN = SCALE(GN, T)
              GS = GS + T
           END IF
        END IF

        ! optionally scale U
        IF (LUACC .AND. (.NOT. LUSID) .AND. (XSU .EQ. 0)) THEN
           !$ L = OMP_GET_NUM_THREADS()
           IF (.NOT. LOMP) L = 0
           CALL LANGO('N', N, U, LDU, UN, L)
           IF (L .NE. 0) THEN
              INFO = -5
              RETURN
           END IF
           T = EXPONENT(HUGE(UN)) - EXPONENT(UN) - 2
           IF (T .LT. 0) THEN
              !$ L = OMP_GET_NUM_THREADS()
              IF (.NOT. LOMP) L = 0
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
        IF (LVACC .AND. (.NOT. LVSID) .AND. (XSV .EQ. 0)) THEN
           !$ L = OMP_GET_NUM_THREADS()
           IF (.NOT. LOMP) L = 0
           CALL LANGO('N', N, V, LDV, VN, L)
           IF (L .NE. 0) THEN
              INFO = -7
              RETURN
           END IF
           T = EXPONENT(HUGE(VN)) - EXPONENT(VN) - 2
           IF (T .LT. 0) THEN
              !$ L = OMP_GET_NUM_THREADS()
              IF (.NOT. LOMP) L = 0
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

  ! extract SV from G with a safe backscaling
  IF (LOMP) THEN
     I = 0
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,N,GS) REDUCTION(MAX:I)
     DO J = 1, N
        SV(J) = SCALE(REAL(G(J,J), REAL128), -GS)
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
  ELSE ! sequentially
     DO J = 1, N
        SV(J) = SCALE(REAL(G(J,J), REAL128), -GS)
        IF (.NOT. (SV(J) .LE. HUGE(SV(J)))) THEN
           INFO = -9
           RETURN
        END IF
     END DO
  END IF

  ! backscale G, U, V
  IF (GS .NE. 0) THEN
     !$ L = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) L = 0
     CALL SCALG(N, N, G, LDG, -GS, L)
     IF (L .NE. 0) THEN
        INFO = -3
        RETURN
     END IF
     GN = SCALE(GN, -GS)
  END IF
  IF (US .NE. 0) THEN
     !$ L = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) L = 0
     CALL SCALG(N, N, U, LDU, -US, L)
     IF (L .NE. 0) THEN
        INFO = -5
        RETURN
     END IF
     UN = SCALE(UN, -US)
  END IF
  IF (VS .NE. 0) THEN
     !$ L = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) L = 0
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
