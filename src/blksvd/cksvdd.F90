!>@brief \b CKSVDD computes the SVD of G as U S V^H, with S returned in SV and U and V optionally accumulated on either identity for the SVD, or on preset input matrices.
SUBROUTINE CKSVDD(JOB, N, G, LDG, U, LDU, V, LDV, SV, W, D, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL32, REAL64, REAL128
  !$ USE OMP_LIB
  IMPLICIT NONE

#ifdef CR_MATH
  INTERFACE
#ifdef NDEBUG
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
#else
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
#endif
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       REAL(KIND=c_float), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_float) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif
  INTERFACE
     SUBROUTINE CLANGO(O, N, G, LDG, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       CHARACTER, INTENT(IN) :: O
       INTEGER, INTENT(IN) :: N, LDG
       COMPLEX(KIND=REAL32), INTENT(IN) :: G(N,LDG)
       REAL(KIND=REAL32), INTENT(OUT) :: S
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CLANGO
  END INTERFACE
  INTERFACE
     SUBROUTINE CSCALG(M, N, G, LDG, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, S
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: G(LDG,N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CSCALG
  END INTERFACE
  INTERFACE
     SUBROUTINE CMKDPQ(N, G, LDG, D, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDG
       COMPLEX(KIND=REAL32), INTENT(IN) :: G(LDG,N)
       REAL(KIND=REAL64), INTENT(OUT) :: D(*)
       INTEGER, INTENT(INOUT) :: O(2,*), INFO
     END SUBROUTINE CMKDPQ
  END INTERFACE
  INTERFACE
     SUBROUTINE CKSVD2(G, U, V, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       COMPLEX(KIND=REAL32), INTENT(IN) :: G(2,2)
       COMPLEX(KIND=REAL32), INTENT(OUT) :: U(2,2), V(2,2)
       REAL(KIND=REAL32), INTENT(OUT) :: S(2)
       INTEGER, INTENT(INOUT) :: INFO(3)
     END SUBROUTINE CKSVD2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE CCVGPP(G, U, V, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       COMPLEX(KIND=REAL32), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
       REAL(KIND=REAL32), INTENT(INOUT) :: S(2)
       INTEGER, INTENT(INOUT) :: INFO(3)
     END SUBROUTINE CCVGPP
  END INTERFACE
  INTERFACE
     SUBROUTINE CROTC(M, N, G, LDG, P, Q, W, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, P, Q
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=REAL32), INTENT(IN) :: W(2,2)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CROTC
  END INTERFACE
  INTERFACE
     SUBROUTINE CROTR(M, N, G, LDG, P, Q, W, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, P, Q
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=REAL32), INTENT(IN) :: W(2,2)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CROTR
  END INTERFACE

  INTEGER, PARAMETER :: K = REAL32, USID = 8, UACC = 16, VSID = 32, VACC = 64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  COMPLEX(KIND=K), PARAMETER :: CZERO = (ZERO,ZERO), CONE = (ONE,ZERO)
  INTEGER, INTENT(IN) :: JOB, N, LDG, LDU, LDV
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N), U(LDU,N), V(LDV,N)
  REAL(KIND=REAL128), INTENT(INOUT) :: SV(N)
  REAL(KIND=K), INTENT(INOUT) :: W(*)
  REAL(KIND=REAL64), INTENT(OUT) :: D(*)
  INTEGER, INTENT(INOUT) :: O(2,*), INFO

  COMPLEX(KIND=K) :: G2(2,2), U2(2,2), V2(2,2)
  REAL(KIND=K) :: GN, UN, VN
  INTEGER(KIND=INT64) :: TT, TM, SM
  INTEGER :: MRQSTP, I, J, L, M, P, Q, T, GS, US, VS, WV, WS, STP, XSG, XSU, XSV, ES(3)
  LOGICAL :: LOMP, LUSID, LUACC, LVSID, LVACC

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
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  LUSID = (IAND(JOB, USID) .NE. 0)
  LUACC = (IAND(JOB, UACC) .NE. 0)
  LVSID = (IAND(JOB, VSID) .NE. 0)
  LVACC = (IAND(JOB, VACC) .NE. 0)

  M = N * (N - 1)
  I = N / 2
  J = M / 2

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
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(V,N)
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
  ELSE ! sequentially
     IF (LUSID) THEN
        DO J = 1, N
           DO I = 1, J-1
              U(I,J) = CZERO
           END DO
           U(J,J) = CONE
           DO I = J+1, N
              U(I,J) = CZERO
           END DO
        END DO
     END IF
     IF (LVSID) THEN
        DO J = 1, N
           DO I = 1, J-1
              V(I,J) = CZERO
           END DO
           V(J,J) = CONE
           DO I = J+1, N
              V(I,J) = CZERO
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
  CALL CLANGO('N', N, G, LDG, GN, L)
  IF (L .NE. 0) THEN
     INFO = -3
     RETURN
  END IF
  IF (XSG .EQ. 0) THEN
     GS = EXPONENT(HUGE(GN)) - EXPONENT(GN) - 9
  ELSE IF (XSG .GT. 0) THEN
     GS = XSG
  ELSE ! XSG < 0
     GS = XSG + 1
  END IF
  IF (GS .NE. 0) THEN
     !$ L = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) L = 0
     CALL CSCALG(N, N, G, LDG, GS, L)
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
        CALL CLANGO('N', N, U, LDU, UN, L)
        IF (L .NE. 0) THEN
           INFO = -5
           RETURN
        END IF
        IF (XSU .EQ. 0) THEN
           US = EXPONENT(HUGE(UN)) - EXPONENT(UN) - 4
        ELSE IF (XSU .GT. 0) THEN
           US = XSU
        ELSE ! XSU < 0
           US = XSU + 1
        END IF
     END IF
     IF (US .NE. 0) THEN
        !$ L = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) L = 0
        CALL CSCALG(N, N, U, LDU, US, L)
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
        CALL CLANGO('N', N, V, LDV, VN, L)
        IF (L .NE. 0) THEN
           INFO = -7
           RETURN
        END IF
        IF (XSV .EQ. 0) THEN
           VS = EXPONENT(HUGE(VN)) - EXPONENT(VN) - 4
        ELSE IF (XSV .GT. 0) THEN
           VS = XSV
        ELSE ! XSV < 0
           VS = XSV + 1
        END IF
     END IF
     IF (VS .NE. 0) THEN
        !$ L = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) L = 0
        CALL CSCALG(N, N, V, LDV, VS, L)
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

  DO STP = 0, MRQSTP-1
     T = STP + 1

     ! build the current step's pairs
     !$ L = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) L = 0
     CALL CMKDPQ(N, G, LDG, D, O, L)
     IF (L .LT. 0) THEN
        INFO = -10
        RETURN
     END IF
     I = L
     ! convergence
     IF (I .EQ. 0) EXIT
     IF (I .GT. 0) THEN
        TT = TT + I
     ELSE ! should never happen
        INFO = -12
        RETURN
     END IF

     ! compute and apply the transformations
     M = 0
     IF (LOMP .AND. (I .GT. 1)) THEN
        !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,U,W,O,N,LDG,LDU,I,XSG,LUACC) PRIVATE(G2,U2,V2,P,Q,WV,WS,T,L,ES) REDUCTION(+:M)
        DO J = 1, I
           L = (N * (N - 1)) / 2
           P = O(1,L+J)
           Q = O(2,L+J)
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
           IF (XSG .EQ. 0) THEN
              T = 0
           ELSE ! no inner scaling
              T = -1
           END IF
           ES(1) = T
           CALL CKSVD2(G2, U2, V2, W(WS), ES)
           O(2,L+I+J) = ES(1)
           CALL CCVGPP(G2, U2, V2, W(WS), ES)
           O(1,L+I+J) = ES(1)
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
                 CALL CROTC(N, N, U, LDU, P, Q, U2, L)
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
              CALL CROTR(N, N, G, LDG, P, Q, G2, L)
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
        !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,V,W,O,N,LDG,LDV,I,LVACC) PRIVATE(V2,P,Q,WV,WS,T,L) REDUCTION(+:M)
        DO J = 1, I
           L = (N * (N - 1)) / 2
           P = O(1,L+J)
           Q = O(2,L+J)
           WV = (J - 1) * 10 + 1
           WS = WV + 8
           T = O(1,L+I+J)
           ! transform V and G from the right
           IF (IAND(T, 4) .NE. 0) THEN
              V2(1,1) = CMPLX(W(WV), W(WV+1), K)
              V2(2,1) = CMPLX(W(WV+2), W(WV+3), K)
              V2(1,2) = CMPLX(W(WV+4), W(WV+5), K)
              V2(2,2) = CMPLX(W(WV+6), W(WV+7), K)
              IF (LVACC) THEN
                 L = 0
                 CALL CROTC(N, N, V, LDV, P, Q, V2, L)
                 IF (L .LT. 0) THEN
                    M = M + (I + 1)
                    CYCLE
                 END IF
              END IF
              L = 0
              CALL CROTC(N, N, G, LDG, P, Q, V2, L)
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
     ELSE ! sequentially
        DO J = 1, I
           L = (N * (N - 1)) / 2
           P = O(1,L+J)
           Q = O(2,L+J)
           IF ((P .LE. 0) .OR. (Q .LE. P) .OR. (P .GE. N) .OR. (Q .GT. N)) THEN
              INFO = -13
              RETURN
           END IF
           G2(1,1) = G(P,P)
           G2(2,1) = G(Q,P)
           G2(1,2) = G(P,Q)
           G2(2,2) = G(Q,Q)
           WV = (J - 1) * 10 + 1
           WS = WV + 8
           IF (XSG .EQ. 0) THEN
              T = 0
           ELSE ! no inner scaling
              T = -1
           END IF
           ES(1) = T
           CALL CKSVD2(G2, U2, V2, W(WS), ES)
           O(2,L+I+J) = ES(1)
           CALL CCVGPP(G2, U2, V2, W(WS), ES)
           O(1,L+I+J) = ES(1)
           T = ES(1)
           IF (T .LT. 0) THEN
              INFO = -14
              RETURN
           END IF
           ! transform U from the right, conjugate-transpose U2, and transform G from the left
           IF (IAND(T, 2) .NE. 0) THEN
              IF (LUACC) THEN
                 L = 0
                 CALL CROTC(N, N, U, LDU, P, Q, U2, L)
                 IF (L .LT. 0) THEN
                    INFO = -15
                    RETURN
                 END IF
              END IF
              G2(1,1) = CONJG(U2(1,1))
              G2(2,1) = CONJG(U2(1,2))
              G2(1,2) = CONJG(U2(2,1))
              G2(2,2) = CONJG(U2(2,2))
              L = 0
              CALL CROTR(N, N, G, LDG, P, Q, G2, L)
              IF (L .LT. 0) THEN
                 INFO = -16
                 RETURN
              END IF
           END IF
           ! transform V and G from the right
           IF (IAND(T, 4) .NE. 0) THEN
              IF (LVACC) THEN
                 L = 0
                 CALL CROTC(N, N, V, LDV, P, Q, V2, L)
                 IF (L .LT. 0) THEN
                    INFO = -17
                    RETURN
                 END IF
              END IF
              L = 0
              CALL CROTC(N, N, G, LDG, P, Q, V2, L)
              IF (L .LT. 0) THEN
                 INFO = -18
                 RETURN
              END IF
           END IF
           ! set the new values
           G(P,P) = CMPLX(W(WS), ZERO, K)
           G(Q,P) = CZERO
           G(P,Q) = CZERO
           G(Q,Q) = CMPLX(W(WS+1), ZERO, K)
           IF (IAND(T, 8) .NE. 0) M = M + 1
        END DO
     END IF

     IF (M .GT. 0) THEN
        TM = TM + M

        ! optionally scale G
        IF (XSG .EQ. 0) THEN
           !$ L = OMP_GET_NUM_THREADS()
           IF (.NOT. LOMP) L = 0
           CALL CLANGO('N', N, G, LDG, GN, L)
           IF (L .NE. 0) THEN
              INFO = -3
              RETURN
           END IF
           T = EXPONENT(HUGE(GN)) - EXPONENT(GN) - 9
           IF (T .LT. 0) THEN
              !$ L = OMP_GET_NUM_THREADS()
              IF (.NOT. LOMP) L = 0
              CALL CSCALG(N, N, G, LDG, T, L)
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
           CALL CLANGO('N', N, U, LDU, UN, L)
           IF (L .NE. 0) THEN
              INFO = -5
              RETURN
           END IF
           T = EXPONENT(HUGE(UN)) - EXPONENT(UN) - 4
           IF (T .LT. 0) THEN
              !$ L = OMP_GET_NUM_THREADS()
              IF (.NOT. LOMP) L = 0
              CALL CSCALG(N, N, U, LDU, T, L)
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
           CALL CLANGO('N', N, V, LDV, VN, L)
           IF (L .NE. 0) THEN
              INFO = -7
              RETURN
           END IF
           T = EXPONENT(HUGE(VN)) - EXPONENT(VN) - 4
           IF (T .LT. 0) THEN
              !$ L = OMP_GET_NUM_THREADS()
              IF (.NOT. LOMP) L = 0
              CALL CSCALG(N, N, V, LDV, T, L)
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

  ! extract SV from G with a safe backscaling
  IF (LOMP) THEN
     I = 0
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,N,GS) REDUCTION(MAX:I)
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
  ELSE ! sequentially
     DO J = 1, N
        SV(J) = SCALE(REAL(REAL(G(J,J)), REAL128), -GS)
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
     CALL CSCALG(N, N, G, LDG, -GS, L)
     IF (L .NE. 0) THEN
        INFO = -3
        RETURN
     END IF
     GN = SCALE(GN, -GS)
  END IF
  IF (US .NE. 0) THEN
     !$ L = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) L = 0
     CALL CSCALG(N, N, U, LDU, -US, L)
     IF (L .NE. 0) THEN
        INFO = -5
        RETURN
     END IF
     UN = SCALE(UN, -US)
  END IF
  IF (VS .NE. 0) THEN
     !$ L = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) L = 0
     CALL CSCALG(N, N, V, LDV, -VS, L)
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
END SUBROUTINE CKSVDD
