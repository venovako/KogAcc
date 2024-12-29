  I = COMMAND_ARGUMENT_COUNT()
  IF ((I .LT. 2) .OR. (I .GT. 3)) STOP 'args: ((U|u)[pper]|(G|g)[eneral]) N [P]'
  IF (I .EQ. 3) THEN
     CALL GET_COMMAND_ARGUMENT(3, CLA)
     READ (CLA,*) P
  ELSE ! I = 2
     P = 0
  END IF
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  READ (CLA,*) N
  IF (N .LT. 0) STOP 'the second argument is invalid'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  SELECT CASE (CLA(1:1))
  CASE ('G','g')
     UPPER = .FALSE.
  CASE ('U','u')
     UPPER = .TRUE.
  CASE DEFAULT
     STOP 'the first argument is invalid'
  END SELECT
  U = PVN_RAN_OPEN()
  IF (U .LT. 0_c_int) STOP 'cannot open /dev/random for reading'
  IF (UPPER) THEN
     DO I = 1, N
        WRITE (*,1,ADVANCE='NO') '(', RAN_SAFE(U, P)
        WRITE (*,1,ADVANCE='NO') ',', RAN_SAFE(U, P)
        WRITE (*,1,ADVANCE='NO') ') (', RAN_SAFE(U, P)
        WRITE (*,1,ADVANCE='NO') ',', RAN_SAFE(U, P)
        WRITE (*,*) ')'
        WRITE (*,1,ADVANCE='NO') '(', ZERO
        WRITE (*,1,ADVANCE='NO') ',', ZERO
        WRITE (*,1,ADVANCE='NO') ') (', RAN_SAFE(U, P)
        WRITE (*,1,ADVANCE='NO') ',', RAN_SAFE(U, P)
        WRITE (*,*) ')'
     END DO
  ELSE ! general
     DO I = 1, 2*N
        WRITE (*,1,ADVANCE='NO') '(', RAN_SAFE(U, P)
        WRITE (*,1,ADVANCE='NO') ',', RAN_SAFE(U, P)
        WRITE (*,1,ADVANCE='NO') ') (', RAN_SAFE(U, P)
        WRITE (*,1,ADVANCE='NO') ',', RAN_SAFE(U, P)
        WRITE (*,*) ')'
     END DO
  END IF
  U = PVN_RAN_CLOSE(U)
