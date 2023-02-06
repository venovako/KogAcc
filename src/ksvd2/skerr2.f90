!>@brief \b SKERR2 computes the errors in the SVD of G.
SUBROUTINE SKERR2(G, U, V, S, E, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL128
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  REAL(KIND=REAL32), INTENT(IN) :: G(2,2), U(2,2), V(2,2), S(2)
  REAL(KIND=K), INTENT(OUT) :: E(3)
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: GX(2,2), UX(2,2), VX(2,2), SX(2,2)

  IF (INFO .LE. -HUGE(INFO)) RETURN

  UX(1,1) = REAL(U(1,1), K)
  UX(2,1) = REAL(U(2,1), K)
  UX(1,2) = REAL(U(1,2), K)
  UX(2,2) = REAL(U(2,2), K)

  VX = MATMUL(TRANSPOSE(UX), UX)
  VX(1,1) = VX(1,1) - ONE
  VX(2,2) = VX(2,2) - ONE
  E(1) = HYPOT(HYPOT(VX(1,1), VX(2,1)), HYPOT(VX(1,2), VX(2,2)))

  VX(1,1) = REAL(V(1,1), K)
  VX(2,1) = REAL(V(2,1), K)
  VX(1,2) = REAL(V(1,2), K)
  VX(2,2) = REAL(V(2,2), K)

  GX = MATMUL(TRANSPOSE(VX), VX)
  GX(1,1) = GX(1,1) - ONE
  GX(2,2) = GX(2,2) - ONE
  E(2) = HYPOT(HYPOT(GX(1,1), GX(2,1)), HYPOT(GX(1,2), GX(2,2)))

  GX(1,1) = REAL(G(1,1), K)
  GX(2,1) = REAL(G(2,1), K)
  GX(1,2) = REAL(G(1,2), K)
  GX(2,2) = REAL(G(2,2), K)
  E(3) = HYPOT(HYPOT(GX(1,1), GX(2,1)), HYPOT(GX(1,2), GX(2,2)))

  SX(1,1) = REAL(S(1), K)
  SX(2,2) = REAL(S(2), K)
  IF (INFO .NE. 0) THEN
     SX(1,1) = SCALE(SX(1,1), -INFO)
     SX(2,2) = SCALE(SX(2,2), -INFO)
     INFO = 0
  END IF

  UX(1,1) = UX(1,1) * SX(1,1)
  UX(2,1) = UX(2,1) * SX(1,1)
  UX(1,2) = UX(1,2) * SX(2,2)
  UX(2,2) = UX(2,2) * SX(2,2)
  SX = MATMUL(UX, TRANSPOSE(VX)) - GX
  IF (E(3) .EQ. ZERO) THEN
     E(3) = HYPOT(HYPOT(SX(1,1), SX(2,1)), HYPOT(SX(1,2), SX(2,2)))
     IF (E(3) .EQ. ZERO) THEN
        E(3) = ONE
     ELSE ! infinity
        E(3) = E(3) / ZERO
     END IF
  ELSE ! || G ||_F > 0
     E(3) = HYPOT(HYPOT(SX(1,1), SX(2,1)), HYPOT(SX(1,2), SX(2,2))) / E(3)
  END IF
END SUBROUTINE SKERR2
