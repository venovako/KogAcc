!>@brief \b DMMEXEC executes a jitted matrix multiply.
SUBROUTINE DMMEXEC(JITTER, JFNPTR, A, B, C, INFO)
  ! TODO: not tested
  USE, INTRINSIC :: ISO_C_BINDING
#ifdef MKL
#if (MKL .EQ. 1)
  USE MKL_JIT_BLAS_LP64
#else
  USE MKL_JIT_BLAS_ILP64
#endif
#endif
  IMPLICIT NONE
  REAL(KIND=c_double), INTENT(IN) :: A(*), B(*)
  REAL(KIND=c_double), INTENT(INOUT) :: C(*)
  TYPE(c_ptr), INTENT(IN) :: JITTER
  TYPE(c_funptr), INTENT(IN) :: JFNPTR
  INTEGER, INTENT(OUT) :: INFO
#ifdef MKL
  PROCEDURE(DGEMM_JIT_KERNEL_T), POINTER :: JFNPRC
  INFO = 0
  IF (.NOT. C_ASSOCIATED(JITTER)) THEN
     INFO = -1
     RETURN
  END IF
  IF (C_ASSOCIATED(JFNPTR)) THEN
     CALL C_F_PROCPOINTER(JFNPTR, JFNPRC)
     IF (ASSOCIATED(JFNPRC)) THEN
        CALL JFNPRC(JITTER, A, B, C)
     ELSE ! should never happen
        INFO = -6
     END IF
  ELSE ! destroy the jitter
     IF (INT(MKL_JIT_DESTROY(JITTER)) .EQ. INT(MKL_JIT_SUCCESS)) THEN
        INFO = 1
     ELSE ! should never happen
        INFO = -7
     END IF
  END IF
#else
  INFO = -8
#endif
END SUBROUTINE DMMEXEC
