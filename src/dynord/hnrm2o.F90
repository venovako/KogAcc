  NRM2O = ZERO
  IF (INFO .EQ. 0) THEN
     DO J = 1, N
        DO I = 1, M
           NRM2O = CR_HYPOT(NRM2O, CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J))))
        END DO
     END DO
  ELSE ! ||off(G)||_F
     DO J = 1, N
        DO I = 1, J-1
           NRM2O = CR_HYPOT(NRM2O, CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J))))
        END DO
        DO I = J+1, M
           NRM2O = CR_HYPOT(NRM2O, CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J))))
        END DO
     END DO
  END IF
