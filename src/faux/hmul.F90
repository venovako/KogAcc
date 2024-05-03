  AR = REAL(A)
  AI = AIMAG(A)
  BR = REAL(B)
  BI = AIMAG(B)
#ifndef NDEBUG
  CR = 0.0_K
  CI = 0.0_K
#endif
  CALL PMUL(CR, CI, AR, AI, BR, BI)
  KMUL = CMPLX(CR, CI, K)
