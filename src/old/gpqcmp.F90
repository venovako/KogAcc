  ! This is the generic part of the (w,p,q)-comparing routines.
  IF (XW .LT. YW) THEN
     INFO = 1
  ELSE IF (XW .GT. YW) THEN
     INFO = -1
  ELSE IF (XW .EQ. YW) THEN
     XB = XQ - XP
     YB = YQ - YP
     IF (XB .LT. YB) THEN
        INFO = 2
     ELSE IF (XB .GT. YB) THEN
        INFO = -2
     ELSE ! XB = YB
        IF (XP .LT. YP) THEN
           INFO = 3
        ELSE IF (XP .GT. YP) THEN
           INFO = -3
        ELSE ! XP = YP
           IF (XQ .LT. YQ) THEN
              INFO = 4
           ELSE IF (XQ .GT. YQ) THEN
              INFO = -4
           ELSE ! XQ = YQ
              INFO = 0
           END IF
        END IF
     END IF
  ELSE ! NaN magnitude(s)
     IF (YW .EQ. YW) THEN
        INFO = 5
     ELSE ! NaN(YW)
        INFO = -5
     END IF
  END IF
