!>@brief \b SKSVD0 computes the SVD of G as U S V^T, with S returned in SV and U and V optionally accumulated on either identity for the SVD, or on preset input matrices.
SUBROUTINE SKSVD0(JOB, N, G, LDG, U, LDU, V, LDV, SV, W, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL128
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

  INTEGER, PARAMETER :: K = REAL32, USID = 8, UACC = 16, VSID = 32, VACC = 64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: JOB, N, LDG, LDU, LDV
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N), U(LDU,N), V(LDV,N)
  REAL(KIND=REAL128), INTENT(OUT) :: SV(N)
  REAL(KIND=K), INTENT(OUT) :: W(N*N)
  INTEGER, INTENT(INOUT) :: O(2*N*(N-1)), INFO
  REAL(KIND=K) :: GN, UN, VN
  INTEGER :: MRQSTP, I, J, JS, GS, US, VS, STP
  LOGICAL :: LOMP, LUSID, LUACC, LVSID, LVACC

#define LANGO SLANGO
#define SCALG SSCALG

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
  IF (JS .GT. 4) INFO = -1
  IF (((JS .EQ. 2) .OR. (JS .EQ. 4)) .AND. (MOD(N, 2) .NE. 0)) INFO = -2
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
     END IF
     RETURN
  END IF

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

  ! optionally set U and V to I
  IF (LOMP) THEN
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
  ELSE ! OpenMP
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
  END IF

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

  DO STP = 0, MRQSTP-1
     ! TODO
     CONTINUE
  END DO

  ! no convergence if INFO = MRQSTP
  INFO = STP

  ! extract SV from G with a safe backscaling
  IF (LOMP) THEN
     DO J = 1, N
        SV(J) = SCALE(REAL(G(J,J), REAL128), -GS)
        ! should never happen
        IF (.NOT. (SV(J) .LE. HUGE(SV(J)))) THEN
           INFO = -9
           RETURN
        END IF
     END DO
  ELSE ! OpenMP
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
  END IF
  IF (US .NE. 0) THEN
     !$ INFO = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) INFO = 0
     CALL SCALG(N, N, U, LDU, -US, INFO)
     IF (INFO .NE. 0) THEN
        INFO = -5
        RETURN
     END IF
  END IF
  IF (VS .NE. 0) THEN
     !$ INFO = OMP_GET_NUM_THREADS()
     IF (.NOT. LOMP) INFO = 0
     CALL SCALG(N, N, V, LDV, -VS, INFO)
     IF (INFO .NE. 0) THEN
        INFO = -7
        RETURN
     END IF
  END IF
END SUBROUTINE SKSVD0
