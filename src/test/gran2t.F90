  I = COMMAND_ARGUMENT_COUNT()
  IF (I .LT. 1) ERROR STOP 'args: N'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LE. 0) ERROR STOP 'N <= 0'
  Q = QZERO
  Q = -QONE / Q
  F = Q
  K = 0
  L = 1
  M = MOD(N, L)
  L = N / L
  IF (K .LT. M) THEN
     L = L + 1
     I = K * L + 1
  ELSE ! K >= M
     I = M * (L + 1) + (K - M) * L + 1
  END IF
  J = MIN(I + (L - 1), N)
  O = PVN_RAN_OPEN()
  IF (O .LT. 0_c_int) ERROR STOP 'cannot open /dev/random for reading'
  DO K = I, J
     G(1,1) = RAN_SAFE(O)
#ifdef UPPER
     G(2,1) = ZERO
#else
     G(2,1) = RAN_SAFE(O)
#endif
     G(1,2) = RAN_SAFE(O)
     G(2,2) = RAN_SAFE(O)
#ifdef UPPER
#include "grnd2u.F90"
#else
#include "grnd2g.F90"
#endif
  END DO
#ifdef UPPER
  L = 16
#else
  L = 6
#endif
  WRITE (OUTPUT_UNIT,'(I11)',ADVANCE='NO') N
  DO K = 1, L-1
     WRITE (OUTPUT_UNIT,1,ADVANCE='NO') ',', F(1,K)
  END DO
  WRITE (OUTPUT_UNIT,1) ',', F(1,L)
  WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') N
  DO K = 1, L-1
     WRITE (ERROR_UNIT,1,ADVANCE='NO') ',', -F(2,K)
  END DO
  WRITE (ERROR_UNIT,1) ',', -F(2,L)
  O = PVN_RAN_CLOSE(O)
