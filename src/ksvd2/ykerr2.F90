!>@brief \b YKERR2 computes the errors in the SVD of G.
PURE SUBROUTINE YKERR2(G, U, V, S, E, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL128
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  COMPLEX(KIND=K), PARAMETER :: CONE = (1.0_K,ZERO)
  COMPLEX(KIND=REAL128), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
  REAL(KIND=REAL128), INTENT(IN) :: S(2)
  REAL(KIND=K), INTENT(OUT) :: E(3)
  INTEGER, INTENT(IN) :: INFO(3)
  COMPLEX(KIND=K) :: GX(2,2), UX(2,2), VX(2,2), SX(2,2)

  IF (INFO(1) .LT. -HUGE(0)) RETURN

  ! UX = U
  UX(1,1) = CMPLX(REAL(REAL(U(1,1)),K), REAL(AIMAG(U(1,1)),K), K)
  UX(2,1) = CMPLX(REAL(REAL(U(2,1)),K), REAL(AIMAG(U(2,1)),K), K)
  UX(1,2) = CMPLX(REAL(REAL(U(1,2)),K), REAL(AIMAG(U(1,2)),K), K)
  UX(2,2) = CMPLX(REAL(REAL(U(2,2)),K), REAL(AIMAG(U(2,2)),K), K)

  ! GX = U^H
  GX(1,1) = CONJG(UX(1,1))
  GX(2,1) = CONJG(UX(1,2))
  GX(1,2) = CONJG(UX(2,1))
  GX(2,2) = CONJG(UX(2,2))

  ! VX = U^H U - I
  VX(1,1) = GX(1,1) * UX(1,1) + GX(1,2) * UX(2,1) - CONE
  VX(2,1) = GX(2,1) * UX(1,1) + GX(2,2) * UX(2,1)
  VX(1,2) = GX(1,1) * UX(1,2) + GX(1,2) * UX(2,2)
  VX(2,2) = GX(2,1) * UX(1,2) + GX(2,2) * UX(2,2) - CONE
  E(1) = HYPOT(HYPOT(HYPOT(REAL(VX(1,1)), AIMAG(VX(1,1))), HYPOT(REAL(VX(2,1)), AIMAG(VX(2,1)))),&
       HYPOT(HYPOT(REAL(VX(1,2)), AIMAG(VX(1,2))), HYPOT(REAL(VX(2,2)), AIMAG(VX(2,2)))))

  ! SX = V
  SX(1,1) = CMPLX(REAL(REAL(V(1,1)),K), REAL(AIMAG(V(1,1)),K), K)
  SX(2,1) = CMPLX(REAL(REAL(V(2,1)),K), REAL(AIMAG(V(2,1)),K), K)
  SX(1,2) = CMPLX(REAL(REAL(V(1,2)),K), REAL(AIMAG(V(1,2)),K), K)
  SX(2,2) = CMPLX(REAL(REAL(V(2,2)),K), REAL(AIMAG(V(2,2)),K), K)

  ! VX = V^H
  VX(1,1) = CONJG(SX(1,1))
  VX(2,1) = CONJG(SX(1,2))
  VX(1,2) = CONJG(SX(2,1))
  VX(2,2) = CONJG(SX(2,2))

  ! GX = V^H V - I
  GX(1,1) = VX(1,1) * SX(1,1) + VX(1,2) * SX(2,1) - CONE
  GX(2,1) = VX(2,1) * SX(1,1) + VX(2,2) * SX(2,1)
  GX(1,2) = VX(1,1) * SX(1,2) + VX(1,2) * SX(2,2)
  GX(2,2) = VX(2,1) * SX(1,2) + VX(2,2) * SX(2,2) - CONE
  E(2) = HYPOT(HYPOT(HYPOT(REAL(GX(1,1)), AIMAG(GX(1,1))), HYPOT(REAL(GX(2,1)), AIMAG(GX(2,1)))),&
       HYPOT(HYPOT(REAL(GX(1,2)), AIMAG(GX(1,2))), HYPOT(REAL(GX(2,2)), AIMAG(GX(2,2)))))

  ! GX = G
  GX(1,1) = CMPLX(REAL(REAL(G(1,1)),K), REAL(AIMAG(G(1,1)),K), K)
  GX(2,1) = CMPLX(REAL(REAL(G(2,1)),K), REAL(AIMAG(G(2,1)),K), K)
  GX(1,2) = CMPLX(REAL(REAL(G(1,2)),K), REAL(AIMAG(G(1,2)),K), K)
  GX(2,2) = CMPLX(REAL(REAL(G(2,2)),K), REAL(AIMAG(G(2,2)),K), K)
  E(3) = HYPOT(HYPOT(HYPOT(REAL(GX(1,1)), AIMAG(GX(1,1))), HYPOT(REAL(GX(2,1)), AIMAG(GX(2,1)))),&
       HYPOT(HYPOT(REAL(GX(1,2)), AIMAG(GX(1,2))), HYPOT(REAL(GX(2,2)), AIMAG(GX(2,2)))))

  ! diag(SX) = 2^(-INFO) S
  SX(1,1) = CMPLX(REAL(S(1), K), ZERO, K)
  ! SX(2,1) = CZERO
  ! SX(1,2) = CZERO
  SX(2,2) = CMPLX(REAL(S(2), K), ZERO, K)
  SX(1,1) = CMPLX(SCALE(REAL(SX(1,1)), INFO(2) - INFO(1)), ZERO, K)
  SX(2,2) = CMPLX(SCALE(REAL(SX(2,2)), INFO(3) - INFO(1)), ZERO, K)

  ! UX = U \Sigma
  UX(1,1) = UX(1,1) * REAL(SX(1,1))
  UX(2,1) = UX(2,1) * REAL(SX(1,1))
  UX(1,2) = UX(1,2) * REAL(SX(2,2))
  UX(2,2) = UX(2,2) * REAL(SX(2,2))

  ! SX = (U \Sigma) V^H - G
  SX(1,1) = UX(1,1) * VX(1,1) + UX(1,2) * VX(2,1) - GX(1,1)
  SX(2,1) = UX(2,1) * VX(1,1) + UX(2,2) * VX(2,1) - GX(2,1)
  SX(1,2) = UX(1,1) * VX(1,2) + UX(1,2) * VX(2,2) - GX(1,2)
  SX(2,2) = UX(2,1) * VX(1,2) + UX(2,2) * VX(2,2) - GX(2,2)
  IF (E(3) .EQ. ZERO) THEN
     E(3) = HYPOT(HYPOT(HYPOT(REAL(SX(1,1)), AIMAG(SX(1,1))), HYPOT(REAL(SX(2,1)), AIMAG(SX(2,1)))),&
          HYPOT(HYPOT(REAL(SX(1,2)), AIMAG(SX(1,2))), HYPOT(REAL(SX(2,2)), AIMAG(SX(2,2)))))
     IF (E(3) .NE. ZERO) E(3) = E(3) / ZERO
  ELSE ! || G ||_F > 0
     E(3) = HYPOT(HYPOT(HYPOT(REAL(SX(1,1)), AIMAG(SX(1,1))), HYPOT(REAL(SX(2,1)), AIMAG(SX(2,1)))),&
          HYPOT(HYPOT(REAL(SX(1,2)), AIMAG(SX(1,2))), HYPOT(REAL(SX(2,2)), AIMAG(SX(2,2))))) / E(3)
  END IF
END SUBROUTINE YKERR2
