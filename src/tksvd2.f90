  INFO = COMMAND_ARGUMENT_COUNT()
  IF (INFO .EQ. 4) THEN
     CALL GET_COMMAND_ARGUMENT(1, CLA, STATUS=INFO)
     IF (INFO .NE. 0) STOP 'first command line argument is invalid'
     READ (CLA,*) G(1,1)
     CALL GET_COMMAND_ARGUMENT(2, CLA, STATUS=INFO)
     IF (INFO .NE. 0) STOP 'second command line argument is invalid'
     READ (CLA,*) G(2,1)
     CALL GET_COMMAND_ARGUMENT(3, CLA, STATUS=INFO)
     IF (INFO .NE. 0) STOP 'third command line argument is invalid'
     READ (CLA,*) G(1,2)
     CALL GET_COMMAND_ARGUMENT(4, CLA, STATUS=INFO)
     IF (INFO .NE. 0) STOP 'fourth command line argument is invalid'
     READ (CLA,*) G(2,2)
  ELSE IF (INFO .EQ. 0) THEN
     WRITE (*,'(A)',ADVANCE='NO') 'G(1,1)='
     READ (*,*) G(1,1)
     WRITE (*,'(A)',ADVANCE='NO') 'G(2,1)='
     READ (*,*) G(2,1)
     WRITE (*,'(A)',ADVANCE='NO') 'G(1,2)='
     READ (*,*) G(1,2)
     WRITE (*,'(A)',ADVANCE='NO') 'G(2,2)='
     READ (*,*) G(2,2)
  ELSE ! invalid command line
     STOP 'zero or four command line arguments required: G(1,1) G(2,1) G(1,2) G(2,2)'
  END IF
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
  WRITE (*,1) '||U SIGMA V^T - G||_F / ||G||_F=', &
       HYPOT(HYPOT(UX(1,1), UX(2,1)), HYPOT(UX(1,2), UX(2,2))) / &
       HYPOT(HYPOT(GX(1,1), GX(2,1)), HYPOT(GX(1,2), GX(2,2)))
