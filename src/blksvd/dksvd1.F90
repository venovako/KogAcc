!!! TODO: REQUIRES TESTING; NO SCALING IMPLEMENTED !!!
!>@brief \b DKSVD1 computes the SVD of G as U S V^T, with S returned in SV and U and V optionally accumulated on either identity for the SVD, or on preset input matrices.
SUBROUTINE DKSVD1(JOB, M, B, G, LDG, U, LDU, V, LDV, SV, LDB, W, NW, D, ND, O, NO, INFO)
#ifdef ANIMATE
  USE, INTRINSIC :: ISO_C_BINDING
#endif
!#ifdef NDEBUG
!  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
!#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128, OUTPUT_UNIT
!#endif
  !$ USE OMP_LIB
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE IBDIMS(N, B, M, M_B, NW, ND, NO, INFO)
       INTEGER, INTENT(INOUT) :: N, B, M, INFO
       INTEGER, INTENT(OUT) :: M_B, NW, ND, NO
     END SUBROUTINE IBDIMS
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE JSTEP(J, N, S, T, P, O, R, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: J, N, S, T, P, O(*)
       INTEGER, INTENT(OUT) :: R(2,P), INFO
     END SUBROUTINE JSTEP
  END INTERFACE
  INTERFACE
     SUBROUTINE DMKBPQ(M, G, LDG, B, W, D, O, OD, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, LDG, B, O(2,*)
       REAL(KIND=REAL64), INTENT(IN) :: G(LDG,M)
       REAL(KIND=REAL64), INTENT(OUT) :: W(M/B,M/B)
       REAL(KIND=REAL128), INTENT(OUT) :: D(*)
       INTEGER, INTENT(OUT) :: OD(2,*)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DMKBPQ
  END INTERFACE
  INTERFACE
     SUBROUTINE DBPACK(M, G, LDG, B, GB, LDB, NB, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64
       INTEGER, INTENT(IN) :: M, LDG, B, LDB, NB, O(2,NB)
       REAL(KIND=REAL64), INTENT(IN) :: G(LDG,M)
       REAL(KIND=REAL64), INTENT(OUT) :: GB(LDB,2*B,NB)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DBPACK
  END INTERFACE
  INTERFACE
     SUBROUTINE DBUNPACK(M, G, LDG, B, GB, LDB, NB, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, LDG, B, LDB, NB, O(2,NB)
       REAL(KIND=REAL64), INTENT(OUT) :: G(LDG,M)
       REAL(KIND=REAL64), INTENT(IN) :: GB(LDB,2*B,NB)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DBUNPACK
  END INTERFACE
  INTERFACE
     SUBROUTINE DBKSVDD(B2, NB, GB, UB, VB, LDB, SB, WB, LW, DB, LD, OD, OB, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128 ! HACK
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: B2, NB, LDB, LW, LD, OB(2,*)
       REAL(KIND=REAL64), INTENT(INOUT) :: GB(LDB,B2,NB)
       REAL(KIND=REAL64), INTENT(OUT) :: UB(LDB,B2,NB), VB(LDB,B2,NB), SB(B2,NB), WB(LW,NB)
       REAL(KIND=REAL128), INTENT(OUT) :: DB(LD,NB)
       INTEGER, INTENT(OUT) :: OD(2,B2,NB)
       INTEGER, INTENT(INOUT) :: O(2,NB), INFO
     END SUBROUTINE DBKSVDD
  END INTERFACE
  INTERFACE
     SUBROUTINE DBUPDATE(M, B, G, LDG, U, LDU, V, LDV, GB, UB, VB, LDB, NB, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, B, LDG, LDU, LDV, LDB, NB, O(2,2*NB)
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,M), U(LDU,M), V(LDV,M), GB(LDB,2*B,NB), UB(LDB,2*B,NB)
       REAL(KIND=REAL64), INTENT(IN) :: VB(LDB,2*B,NB)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DBUPDATE
  END INTERFACE
  ! TODO: CL = -K, WL = 5 for COMPLEX
  INTEGER, PARAMETER :: K = REAL64, KK = REAL128, USID = 8, UACC = 16, VSID = 32, VACC = 64, CL = K, WL = 3
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: JOB, M, B, LDG, LDU, LDV, LDB, NW, ND, NO
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,M), U(LDU,M), V(LDV,M)
#ifdef ANIMATE
  REAL(KIND=K), INTENT(INOUT), TARGET :: SV(M)
#else
  REAL(KIND=K), INTENT(OUT) :: SV(M)
#endif
  REAL(KIND=K), INTENT(OUT) :: W(NW)
  REAL(KIND=KK), INTENT(OUT) :: D(ND)
  INTEGER, INTENT(INOUT) :: O(2,NO), INFO

  INTEGER :: I, J, L, N, P, Q, R, S, T, JS, BS, M_B, M_P, B_P, LB, LW, LD, NB
  INTEGER :: IGB, IUB, IVB, IWB, IO1, IO0, IOB, IOD
  LOGICAL :: LOMP, LUSID, LUACC, LVSID, LVACC
#ifdef ANIMATE
  TYPE(c_ptr) :: CTX
  TYPE(c_ptr), POINTER :: CP
  INTEGER(KIND=c_size_t) :: LDF
  INTEGER(KIND=c_int), EXTERNAL :: PVN_RVIS_FRAME
#define VIS_FRAME PVN_RVIS_FRAME
#endif
#define MKBPQ DMKBPQ
#define BPACK DBPACK
#define BKSVDD DBKSVDD
#define BUNPACK DBUNPACK
#define BUPDATE DBUPDATE
  L = INFO
  INFO = 0
  JS = IAND(JOB, 7)
  IF ((JOB .LT. 0) .OR. (JOB .GT. 124) .OR. (JS .GT. 4)) THEN
     INFO = -1
     RETURN
  END IF

  N = M
  I = B
  J = JS
  INFO = CL
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
  LB = LDB * N * M_P
  IGB = 1
  IUB = IGB + LB
  IVB = IUB + LB
  IWB = IVB + LB
  LW = MAX((N - 1), WL) * N
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

  LOMP = .FALSE.
  IF (L .LT. 0) THEN
     L = -(L + 1)
  ELSE ! L .GE. 0
     !$ LOMP = .TRUE.
     CONTINUE
  END IF
  LUSID = (IAND(JOB, USID) .NE. 0)
  LUACC = (IAND(JOB, UACC) .NE. 0)
  LVSID = (IAND(JOB, VSID) .NE. 0)
  LVACC = (IAND(JOB, VACC) .NE. 0)

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

#ifdef ANIMATE
  CALL C_F_POINTER(C_LOC(SV), CP)
  CTX = CP
  CP => NULL()
  LDF = INT(LDG, c_size_t)
  IF (C_ASSOCIATED(CTX)) J = INT(VIS_FRAME(CTX, G, LDF))
#endif

  DO BS = 0, L-1
#ifdef ANIMATE
     IF (C_ASSOCIATED(CTX)) J = INT(VIS_FRAME(CTX, G, LDF))
#endif
     I = BS + 1
!#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,'(I11,A)',ADVANCE='NO') I, ','
     FLUSH(OUTPUT_UNIT)
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
     WRITE (OUTPUT_UNIT,'(I11,A)',ADVANCE='NO') NB, ','
     FLUSH(OUTPUT_UNIT)
!#endif
     IF ((J .LT. 0) .OR. (NB .LT. 0)) THEN
        INFO = -1000 * I - 100 + J
        EXIT
     END IF
     ! CONVERGENCE
     IF (NB .EQ. 0) EXIT
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
        EXIT
     END IF
!#ifndef NDEBUG
     T = 0
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(O,R) PRIVATE(J) REDUCTION(+:T) IF(LOMP)
     DO J = R, R+NB-1
        T = T + O(1,J)
     END DO
     !$OMP END PARALLEL DO
     WRITE (OUTPUT_UNIT,'(I11,A,I11)') Q, ',', T
     FLUSH(OUTPUT_UNIT)
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
  END DO
#ifdef ANIMATE
  IF (C_ASSOCIATED(CTX)) J = INT(VIS_FRAME(CTX, G, LDF))
#endif
  IF (INFO .NE. 0) RETURN
  INFO = BS

  ! extract SV from G
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,M) PRIVATE(J) IF(LOMP)
  DO J = 1, M
     SV(J) = G(J,J)
  END DO
  !$OMP END PARALLEL DO
END SUBROUTINE DKSVD1
