  RMIN = TINY(A)
  A = HUGE(A)
  RMAX = SCALE(A, -2)
  DO WHILE ((.NOT. (A .GE. RMIN)) .OR. (.NOT. (A .LE. RMAX)))
     READ (U) R
     A = ABS(R)
  END DO
