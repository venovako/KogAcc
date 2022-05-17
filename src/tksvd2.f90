  ! This is the generic part of the tester routines.

  SELECT CASE (COMMAND_ARGUMENT_COUNT())
  CASE (0)
     WRITE (*,'(A)',ADVANCE='NO') 'G(1,1)='
     READ (*,*) G(1,1)
     WRITE (*,'(A)',ADVANCE='NO') 'G(2,1)='
     READ (*,*) G(2,1)
     WRITE (*,'(A)',ADVANCE='NO') 'G(1,2)='
     READ (*,*) G(1,2)
     WRITE (*,'(A)',ADVANCE='NO') 'G(2,2)='
     READ (*,*) G(2,2)
  CASE (1)
     CALL GET_COMMAND_ARGUMENT(1, CLA, STATUS=INFO)
     IF (INFO .NE. 0) STOP 'the command line argument is invalid'
     OPEN(UNIT=1, FILE=TRIM(CLA), ACTION='READ', STATUS='OLD', IOSTAT=INFO)
     IF (INFO .NE. 0) STOP 'cannot open the input file'
     READ (1,*) G(1,1), G(1,2)
     READ (1,*) G(2,1), G(2,2)
     CLOSE(UNIT=1, IOSTAT=INFO)
     IF (INFO .NE. 0) STOP 'cannot close the input file'
  CASE (4)
     CALL GET_COMMAND_ARGUMENT(1, CLA, STATUS=INFO)
     IF (INFO .NE. 0) STOP 'the first command line argument is invalid'
     READ (CLA,*) G(1,1)
     CALL GET_COMMAND_ARGUMENT(2, CLA, STATUS=INFO)
     IF (INFO .NE. 0) STOP 'the second command line argument is invalid'
     READ (CLA,*) G(2,1)
     CALL GET_COMMAND_ARGUMENT(3, CLA, STATUS=INFO)
     IF (INFO .NE. 0) STOP 'the third command line argument is invalid'
     READ (CLA,*) G(1,2)
     CALL GET_COMMAND_ARGUMENT(4, CLA, STATUS=INFO)
     IF (INFO .NE. 0) STOP 'the fourth command line argument is invalid'
     READ (CLA,*) G(2,2)
  CASE DEFAULT
     STOP 'zero, one, or four command line arguments required: G(1,1) G(2,1) G(1,2) G(2,2)'
  END SELECT
  WRITE (*,1) 'G(1,1)=', G(1,1)
  WRITE (*,1) 'G(2,1)=', G(2,1)
  WRITE (*,1) 'G(1,2)=', G(1,2)
  WRITE (*,1) 'G(2,2)=', G(2,2)
  CALL KSVD2(G, U, V, S, INFO)
  WRITE (*,1) 'U(1,1)=', U(1,1)
  WRITE (*,1) 'U(2,1)=', U(2,1)
  WRITE (*,1) 'U(1,2)=', U(1,2)
  WRITE (*,1) 'U(2,2)=', U(2,2)
  WRITE (*,1) 'V(1,1)=', V(1,1)
  WRITE (*,1) 'V(2,1)=', V(2,1)
  WRITE (*,1) 'V(1,2)=', V(1,2)
  WRITE (*,1) 'V(2,2)=', V(2,2)
  WRITE (*,1) 'S(1)=', S(1)
  WRITE (*,1) 'S(2)=', S(2)
  IF (INFO .EQ. IERR) THEN
     STOP 'INFO=ERROR'
  ELSE ! all OK
     WRITE (*,2) 'INFO=', INFO
  END IF
  UX(1,1) = U(1,1)
  UX(2,1) = U(2,1)
  UX(1,2) = U(1,2)
  UX(2,2) = U(2,2)
  SX(1,1) = 1.0_KX
  SX(2,1) = 0.0_KX
  SX(1,2) = 0.0_KX
  SX(2,2) = 1.0_KX
  VX = MATMUL(TRANSPOSE(UX), UX) - SX
  WRITE (*,1) '||U^T U - I||_F=', HYPOT(HYPOT(VX(1,1), VX(2,1)), HYPOT(VX(1,2), VX(2,2)))
  VX(1,1) = V(1,1)
  VX(2,1) = V(2,1)
  VX(1,2) = V(1,2)
  VX(2,2) = V(2,2)
  GX = MATMUL(TRANSPOSE(VX), VX) - SX
  WRITE (*,1) '||V^T V - I||_F=', HYPOT(HYPOT(GX(1,1), GX(2,1)), HYPOT(GX(1,2), GX(2,2)))
  GX(1,1) = G(1,1)
  GX(2,1) = G(2,1)
  GX(1,2) = G(1,2)
  GX(2,2) = G(2,2)
  SX(1,1) = SCALE(S(1), -INFO)
  WRITE (*,1) 'SIGMA(1)=', SX(1,1)
  SX(2,2) = SCALE(S(2), -INFO)
  WRITE (*,1) 'SIGMA(2)=', SX(2,2)
  UX = MATMUL(MATMUL(UX, SX), TRANSPOSE(VX)) - GX
  SX(2,1) = HYPOT(HYPOT(UX(1,1), UX(2,1)), HYPOT(UX(1,2), UX(2,2)))
  SX(1,2) = HYPOT(HYPOT(GX(1,1), GX(2,1)), HYPOT(GX(1,2), GX(2,2)))
  IF ((SX(1,2) .EQ. 0.0_KX) .AND. (SX(2,1) .EQ. 0.0_KX)) THEN
     SX(2,1) = 0.0_KX
  ELSE ! general case
     SX(2,1) = SX(2,1) / SX(1,2)
  END IF
  WRITE (*,1) '||U SIGMA V^T - G||_F / ||G||_F=', SX(2,1)
