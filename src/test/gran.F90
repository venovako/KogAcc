  I = COMMAND_ARGUMENT_COUNT()
  IF (I .LT. 2) ERROR STOP 'args: ((U|u)[pper]|(G|g)[eneral]) N'
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  READ (CLA,*) N
  IF (N .LT. 0) ERROR STOP 'the second argument is invalid'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  SELECT CASE (CLA(1:1))
  CASE ('G','g')
     UPPER = .FALSE.
  CASE ('U','u')
     UPPER = .TRUE.
  CASE DEFAULT
     ERROR STOP 'the first argument is invalid'
  END SELECT
  U = ORFILE()
  IF (UPPER) THEN
     DO I = 1, N
        WRITE (*,1,ADVANCE='NO') '', RSAFE(U)
        WRITE (*,1) ' ', RSAFE(U)
        WRITE (*,1,ADVANCE='NO') '', ZERO
        WRITE (*,1) ' ', RSAFE(U)
     END DO
  ELSE ! general
     DO I = 1, N
        WRITE (*,1,ADVANCE='NO') '', RSAFE(U)
        WRITE (*,1) ' ', RSAFE(U)
        WRITE (*,1,ADVANCE='NO') '', RSAFE(U)
        WRITE (*,1) ' ', RSAFE(U)
     END DO
  END IF
