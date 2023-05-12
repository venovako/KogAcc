  IF (INFO .LE. -HUGE(INFO)) RETURN
  IF (INFO .NE. 0) THEN
     INFO = -INFO
     T = SCALE(S(1), INFO)
     IF (.NOT. (ABS(T) .LE. HUGE(T))) THEN
        T = SCALE(S(2), INFO)
        IF (.NOT. (ABS(T) .LE. HUGE(T))) THEN
           INFO = -3
        ELSE ! S(2) OK
           S(2) = T
           INFO = -1
        END IF
     ELSE ! S(1) OK
        S(1) = T
        T = SCALE(S(2), INFO)
        IF (.NOT. (ABS(T) .LE. HUGE(T))) THEN
           INFO = -2
        ELSE ! S(2) OK
           S(2) = T
           INFO = 0
        END IF
     END IF
  END IF
  IF (INFO .NE. 0) RETURN

  ! G not diagonal
  LND = ((G(2,1) .NE. ZERO) .OR. (G(1,2) .NE. ZERO))
  IF (LND) INFO = IOR(INFO, 1)

  ! U not identity
  LNU = ((U(1,1) .NE. ONE) .OR. (U(2,1) .NE. ZERO) .OR. (U(1,2) .NE. ZERO) .OR. (U(2,2) .NE. ONE))
  IF (LNU) INFO = IOR(INFO, 2)

  ! V not identity
  LNV = ((V(1,1) .NE. ONE) .OR. (V(2,1) .NE. ZERO) .OR. (V(1,2) .NE. ZERO) .OR. (V(2,2) .NE. ONE))
  IF (LNV) INFO = IOR(INFO, 4)

  ! transformation not `small'
  IF (LND .AND. (LNU .OR. LNV)) INFO = IOR(INFO, 8)
