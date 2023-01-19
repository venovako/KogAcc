!>@brief \b SKSVD0 computes the SVD of G as U S V^T, with S returned in SV and U and V optionally accumulated on either identity for the SVD, or on preset input matrices.
SUBROUTINE SKSVD0(JOB, N, G, LDG, U, LDU, V, LDV, SV, W, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING
#ifdef NDEBUG
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL128
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL128, OUTPUT_UNIT
#endif
  !$ USE OMP_LIB
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE SLANGO(O, N, G, LDG, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       CHARACTER, INTENT(IN) :: O
       INTEGER, INTENT(IN) :: N, LDG
       REAL(KIND=REAL32), INTENT(IN) :: G(N,LDG)
       REAL(KIND=REAL32), INTENT(OUT) :: S
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE SLANGO
  END INTERFACE
  INTERFACE
     SUBROUTINE SSCALG(M, N, G, LDG, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, S
       REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE SSCALG
  END INTERFACE
  INTERFACE
     SUBROUTINE SMK3PQ(K, N, G, LDG, W, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: K, N, LDG
       REAL(KIND=REAL32), INTENT(IN) :: G(LDG,N)
       REAL(KIND=REAL32), INTENT(OUT) :: W(N*N)
       INTEGER, INTENT(OUT) :: O(2*N*(N-1))
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE SMK3PQ
  INTERFACE
     PURE SUBROUTINE JSTEP(J, N, S, T, P, O, R, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: J, N, S, T, P, O(*)
       INTEGER, INTENT(OUT) :: R(2,P), INFO
     END SUBROUTINE JSTEP
  END INTERFACE

  INTEGER, PARAMETER :: K = REAL32, USID = 8, UACC = 16, VSID = 32, VACC = 64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: JOB, N, LDG, LDU, LDV
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N), U(LDU,N), V(LDV,N)
  REAL(KIND=REAL128), INTENT(OUT), TARGET :: SV(N)
  REAL(KIND=K), INTENT(OUT) :: W(N*N)
  INTEGER, INTENT(INOUT) :: O(2*N*(N-1)), INFO

  INTEGER, POINTER, CONTIGUOUS :: R(:,:)
  REAL(KIND=K) :: GN, UN, VN
  INTEGER :: MRQSTP, I, J, M, M_2, NP, NS, P, Q, T, JS, GS, US, VS, STP
  LOGICAL :: LOMP, LUSID, LUACC, LVSID, LVACC

#define LANGO SLANGO
#define SCALG SSCALG
#define MK3PQ SMK3PQ

  MRQSTP = INFO
  INFO = 0
  LOMP = .FALSE.
  IF (MRQSTP .LT. 0) THEN
     MRQSTP = -(MRQSTP + 1)
  ELSE ! I >= 0
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
        W(1) = ZERO
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

  ! associate R with SV
  CALL C_F_POINTER(C_LOC(SV), R, [2,NP])

  DO STP = 0, MRQSTP-1
     T = STP + 1
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,'(I4)',ADVANCE='NO') T
     FLUSH(OUTPUT_UNIT)
#endif

     ! scale G
     !$ INFO = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) INFO = 0
     CALL LANGO('N', N, G, LDG, GN, INFO)
     IF (INFO .NE. 0) THEN
        INFO = -3
        RETURN
     END IF
     GS = EXPONENT(HUGE(GN)) - EXPONENT(GN) - 3
     IF (GS .NE. 0) THEN
        !$ INFO = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) INFO = 0
        CALL SCALG(N, N, G, LDG, GS, INFO)
        IF (INFO .NE. 0) THEN
           INFO = -3
           RETURN
        END IF
        GN = SCALE(GN, GS)
     END IF
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', GN
     FLUSH(OUTPUT_UNIT)
#endif

     ! optionally scale U
     IF (LUACC) THEN
        IF (LUSID) THEN
           UN = ONE
           US = 0
        ELSE ! scaling of U might be required
           !$ INFO = OMP_GET_NUM_THREADS()
           IF (.NOT. LOMP) INFO = 0
           CALL LANGO('N', N, U, LDU, UN, INFO)
           IF (INFO .NE. 0) THEN
              INFO = -5
              RETURN
           END IF
           US = EXPONENT(HUGE(UN)) - EXPONENT(UN) - 2
        END IF
        IF (US .NE. 0) THEN
           !$ INFO = OMP_GET_NUM_THREADS()
           IF (.NOT. LOMP) INFO = 0
           CALL SCALG(N, N, U, LDU, US, INFO)
           IF (INFO .NE. 0) THEN
              INFO = -5
              RETURN
           END IF
           UN = SCALE(UN, US)
        END IF
     ELSE ! .NOT. LUACC
        UN = ONE
        US = 0
     END IF
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', UN
     FLUSH(OUTPUT_UNIT)
#endif

     ! optionally scale V
     IF (LVACC) THEN
        IF (LVSID) THEN
           VN = ONE
           VS = 0
        ELSE ! scaling of V might be required
           !$ INFO = OMP_GET_NUM_THREADS()
           IF (.NOT. LOMP) INFO = 0
           CALL LANGO('N', N, V, LDV, VN, INFO)
           IF (INFO .NE. 0) THEN
              INFO = -7
              RETURN
           END IF
           VS = EXPONENT(HUGE(VN)) - EXPONENT(VN) - 2
        END IF
        IF (VS .NE. 0) THEN
           !$ INFO = OMP_GET_NUM_THREADS()
           IF (.NOT. LOMP) INFO = 0
           CALL SCALG(N, N, V, LDV, VS, INFO)
           IF (INFO .NE. 0) THEN
              INFO = -7
              RETURN
           END IF
           VN = SCALE(VN, VS)
        END IF
     ELSE ! .NOT. LVACC
        VN = ONE
        VS = 0
     END IF
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', VN
     FLUSH(OUTPUT_UNIT)
#endif

     ! build the current step's pairs
     IF (JS .EQ. 3) THEN
        !$ INFO = OMP_GET_NUM_THREADS()
        IF (.NOT. LOMP) INFO = 0
        CALL SMK3PQ(NP, N, G, LDG, W, O, INFO)
        IF (INFO .LT. 0) THEN
           INFO = -10
           RETURN
        END IF
        I = INFO
     ELSE ! tabular O
        I = NP
     END IF
     !$ INFO = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) INFO = 0
     CALL JSTEP(JS, N, NS, T, I, O, R, INFO)
     IF (INFO .NE. 0) THEN
        INFO = -11
        RETURN
     END IF
#ifndef NDEBUG
     WRITE (OUTPUT_UNIT,'(A,I4)',ADVANCE='NO') ',', I
     IF (JS .EQ. 3) THEN
        WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', W(M_2 + 1)
     ELSE ! tabular O
        WRITE (OUTPUT_UNIT,9,ADVANCE='NO') ',', -ONE
     END IF
     FLUSH(OUTPUT_UNIT)
#endif

     ! TODO
  END DO

  R => NULL()
  ! no convergence if INFO = MRQSTP
  INFO = STP

  ! extract SV from G with a safe backscaling
  IF (LOMP) THEN
     I = 0
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,SV,N,GS) REDUCTION(MAX:I)
     DO J = 1, N
        SV(J) = SCALE(REAL(G(J,J), REAL128), -GS)
        ! should never happen
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
        ! should never happen
        IF (.NOT. (SV(J) .LE. HUGE(SV(J)))) THEN
           INFO = -9
           RETURN
        END IF
     END DO
  END IF

  ! backscale G, U, V
  IF (GS .NE. 0) THEN
     !$ INFO = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) INFO = 0
     CALL SCALG(N, N, G, LDG, -GS, INFO)
     IF (INFO .NE. 0) THEN
        INFO = -3
        RETURN
     END IF
     GN = SCALE(GN, -GS)
  END IF
  IF (US .NE. 0) THEN
     !$ INFO = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) INFO = 0
     CALL SCALG(N, N, U, LDU, -US, INFO)
     IF (INFO .NE. 0) THEN
        INFO = -5
        RETURN
     END IF
     UN = SCALE(UN, -US)
  END IF
  IF (VS .NE. 0) THEN
     !$ INFO = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) INFO = 0
     CALL SCALG(N, N, V, LDV, -VS, INFO)
     IF (INFO .NE. 0) THEN
        INFO = -7
        RETURN
     END IF
     VN = SCALE(VN, -VS)
  END IF
  W(1) = GN
  W(2) = W(M_2 + 1)
  W(3) = UN
  W(4) = VN
9 FORMAT(A,ES16.9E2)
END SUBROUTINE SKSVD0
