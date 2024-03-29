!>@brief \b CLANGO computes approximations of the various norms of G or off(G).
SUBROUTINE CLANGO(O, N, G, LDG, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE

  INTERFACE
     PURE FUNCTION CLANGE(NORM, M, N, A, LDA, WORK)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       CHARACTER, INTENT(IN) :: NORM
       INTEGER, INTENT(IN) :: M, N, LDA
       COMPLEX(KIND=REAL32), INTENT(IN) :: A(LDA,*)
       ! a dirty trick to preserve purity since WORK should not be referenced here
       REAL(KIND=REAL32), INTENT(IN) :: WORK
       REAL(KIND=REAL32) :: CLANGE
     END FUNCTION CLANGE
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE CLASSQ(N, X, INCX, SC, SM)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, INCX
       COMPLEX(KIND=REAL32), INTENT(IN) :: X(*)
       REAL(KIND=REAL32), INTENT(INOUT) :: SC, SM
     END SUBROUTINE CLASSQ
  END INTERFACE

  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32, ONE = 1.0_REAL32
  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDG
  COMPLEX(KIND=REAL32), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL32), INTENT(OUT) :: S
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=REAL32) :: SC, SM
  INTEGER :: I, J

  S = ZERO
  I = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN

  SELECT CASE (O)
  CASE ('F','f')
     S = CLANGE('F', N, N, G, LDG, SC)
     IF (.NOT. (S .LE. HUGE(S))) INFO = 1
  CASE ('M','m')
     S = CLANGE('M', N, N, G, LDG, SC)
     IF (.NOT. (S .LE. HUGE(S))) INFO = 1
  CASE ('N','n')
     IF (I .EQ. 0) THEN
        DO J = 1, N
           DO I = 1, N
              SC = ABS(REAL(G(I,J)))
#ifndef NDEBUG
              IF (.NOT. (SC .LE. HUGE(SC))) THEN
                 S = SC
                 INFO = (J - 1) * N + I
                 RETURN
              END IF
#endif
              SM = ABS(AIMAG(G(I,J)))
#ifndef NDEBUG
              IF (.NOT. (SM .LE. HUGE(SM))) THEN
                 S = SM
                 INFO = (J - 1) * N + I
                 RETURN
              END IF
#endif
              S = MAX(S, SC, SM)
           END DO
        END DO
        IF (.NOT. (S .LE. HUGE(S))) INFO = 1
     ELSE ! OpenMP
#ifdef NDEBUG
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,SC,SM) SHARED(G,N) REDUCTION(MAX:S)
        DO J = 1, N
           DO I = 1, N
              SC = ABS(REAL(G(I,J)))
              SM = ABS(AIMAG(G(I,J)))
              S = MAX(S, SC, SM)
           END DO
        END DO
        !$OMP END PARALLEL DO
        IF (.NOT. (S .LE. HUGE(S))) INFO = 1
#else
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,SC,SM) SHARED(G,N) REDUCTION(MAX:S,INFO)
        DO J = 1, N
           DO I = 1, N
              SC = ABS(REAL(G(I,J)))
              SM = ABS(AIMAG(G(I,J)))
              IF ((.NOT. (SC .LE. HUGE(SC))) .OR. (.NOT. (SM .LE. HUGE(SM)))) THEN
                 INFO = MAX(INFO, (J - 1) * N + I)
              ELSE ! SC and SM finite
                 INFO = MAX(INFO, 0)
              END IF
              S = MAX(S, SC, SM)
           END DO
        END DO
        !$OMP END PARALLEL DO
#endif
     END IF
  CASE ('O','o')
     IF (N .GE. 2) THEN
        SC = ZERO
        SM = ONE
        CALL CLASSQ(N-1, G(2,1), 1, SC, SM)
        DO J = 2, N-1
           CALL CLASSQ(J-1, G(1,J), 1, SC, SM)
           CALL CLASSQ(N-J, G(J+1,J), 1, SC, SM)
        END DO
        CALL CLASSQ(N-1, G(1,N), 1, SC, SM)
        S = SC * SQRT(SM)
        IF (.NOT. (S .LE. HUGE(S))) INFO = 1
     END IF
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE CLANGO
