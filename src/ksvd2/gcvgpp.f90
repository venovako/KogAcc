  IF (INFO .LE. -HUGE(INFO)) RETURN
  IF (INFO .NE. 0) THEN
     INFO = -INFO
     S(1) = SCALE(S(1), INFO)
     S(2) = SCALE(S(2), INFO)
     INFO = 0
  END IF

  IF (.NOT. (S(1) .LE. HUGE(S(1)))) INFO = -1
  IF (.NOT. (S(2) .LE. HUGE(S(2)))) INFO = INFO - 2
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
