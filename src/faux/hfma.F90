  AR = REAL(A)
  AI = AIMAG(A)
  BR = REAL(B)
  BI = AIMAG(B)
  CR = REAL(C)
  CI = AIMAG(C)
#ifndef NDEBUG
  DR = 0.0_K
  DI = 0.0_K
#endif
  CALL PFMA(DR, DI, AR, AI, BR, BI, CR, CI)
  KFMA = CMPLX(DR, DI, K)
