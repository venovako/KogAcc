#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA, IEEE_MAX_NUM, IEEE_MIN_NUM
#else
  INTERFACE
     PURE FUNCTION IEEE_FMA(X, Y, Z) BIND(C,NAME='fmal')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y, Z
       REAL(KIND=c_long_double) :: IEEE_FMA
     END FUNCTION IEEE_FMA
  END INTERFACE
  INTERFACE
     PURE FUNCTION IEEE_MAX_NUM(X, Y) BIND(C,NAME='fmaxl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: IEEE_MAX_NUM
     END FUNCTION IEEE_MAX_NUM
  END INTERFACE
  INTERFACE
     PURE FUNCTION IEEE_MIN_NUM(X, Y) BIND(C,NAME='fminl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: IEEE_MIN_NUM
     END FUNCTION IEEE_MIN_NUM
  END INTERFACE
#endif
