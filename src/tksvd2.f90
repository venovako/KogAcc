  WRITE (*,'(A)',ADVANCE='NO') 'G(1,1)='
  READ (*,*) G(1,1)
  WRITE (*,'(A)',ADVANCE='NO') 'G(2,1)='
  READ (*,*) G(2,1)
  WRITE (*,'(A)',ADVANCE='NO') 'G(1,2)='
  READ (*,*) G(1,2)
  WRITE (*,'(A)',ADVANCE='NO') 'G(2,2)='
  READ (*,*) G(2,2)
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
