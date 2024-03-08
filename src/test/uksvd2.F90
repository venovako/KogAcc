  ! This is the generic part of the complex KSVD2 tester routines.
  ONCE = .TRUE.
  I = -1
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
     CALL GET_COMMAND_ARGUMENT(1, CLA, STATUS=I)
     IF ((I .NE. 0) .OR. (LEN_TRIM(CLA) .LE. 0))  ERROR STOP 'the input file name is invalid'
     IF ((LEN_TRIM(CLA) .EQ. 1) .AND. (CLA(1:1) .EQ. '-')) THEN
        I = INPUT_UNIT
     ELSE ! assume (0,5,6) = (stderr,stdin,stdout)
        I = 1
     END IF
     IF (I .NE. INPUT_UNIT) OPEN(UNIT=I, FILE=TRIM(CLA), ACTION='READ', STATUS='OLD', IOSTAT=J)
     IF (J .NE. 0) ERROR STOP 'cannot open the input file'
     ONCE = .FALSE.
  CASE (4)
     CALL GET_COMMAND_ARGUMENT(1, CLA, STATUS=I)
     IF (I .NE. 0) ERROR STOP 'the first argument is invalid'
     READ (CLA,*) G(1,1)
     CALL GET_COMMAND_ARGUMENT(2, CLA, STATUS=I)
     IF (I .NE. 0) ERROR STOP 'the second argument is invalid'
     READ (CLA,*) G(2,1)
     CALL GET_COMMAND_ARGUMENT(3, CLA, STATUS=I)
     IF (I .NE. 0) ERROR STOP 'the third argument is invalid'
     READ (CLA,*) G(1,2)
     CALL GET_COMMAND_ARGUMENT(4, CLA, STATUS=I)
     IF (I .NE. 0) ERROR STOP 'the fourth argument is invalid'
     READ (CLA,*) G(2,2)
  CASE DEFAULT
     ERROR STOP 'zero, one [input file name], or four [G(1,1) G(2,1) G(1,2) G(2,2)] arguments required'
  END SELECT
  DO WHILE (.TRUE.)
     IF (.NOT. ONCE) THEN
        READ (I,*,IOSTAT=J) G(1,1), G(1,2), G(2,1), G(2,2)
        IF (J .NE. 0) EXIT
     END IF
     WRITE (*,2) 'G(1,1)=', G(1,1)
     WRITE (*,2) 'G(2,1)=', G(2,1)
     WRITE (*,2) 'G(1,2)=', G(1,2)
     WRITE (*,2) 'G(2,2)=', G(2,2)
     INFO = 0
     CALL KSVD2(G, U, V, S, INFO)
     WRITE (*,2) 'U(1,1)=', U(1,1)
     WRITE (*,2) 'U(2,1)=', U(2,1)
     WRITE (*,2) 'U(1,2)=', U(1,2)
     WRITE (*,2) 'U(2,2)=', U(2,2)
     WRITE (*,2) 'V(1,1)=', V(1,1)
     WRITE (*,2) 'V(2,1)=', V(2,1)
     WRITE (*,2) 'V(1,2)=', V(1,2)
     WRITE (*,2) 'V(2,2)=', V(2,2)
     WRITE (*,1) 'S(1)=', S(1)
     WRITE (*,1) 'S(2)=', S(2)
     IF (INFO(1) .EQ. IERR) THEN
        WRITE (*,'(A)') 'INFO=ERROR'
     ELSE ! all OK
        WRITE (*,3) 'INFO(1)=', INFO(1)
        WRITE (*,3) 'INFO(2)=', INFO(2)
        WRITE (*,3) 'INFO(3)=', INFO(3)
     END IF
     UX(1,1) = U(1,1)
     UX(2,1) = U(2,1)
     UX(1,2) = U(1,2)
     UX(2,2) = U(2,2)
     SX(1,1) = 1.0_KX
     SX(2,1) = 0.0_KX
     SX(1,2) = 0.0_KX
     SX(2,2) = 1.0_KX
     VX(1,1) = CONJG(UX(1,1))
     VX(2,1) = CONJG(UX(1,2))
     VX(1,2) = CONJG(UX(2,1))
     VX(2,2) = CONJG(UX(2,2))
     VX = MATMUL(VX, UX) - SX
     WRITE (*,1) '||U^H U - I||_F=',&
          CR_HYPOT(CR_HYPOT(CR_HYPOT(REAL(VX(1,1)),AIMAG(VX(1,1))), CR_HYPOT(REAL(VX(2,1)),AIMAG(VX(2,1)))),&
          CR_HYPOT(CR_HYPOT(REAL(VX(1,2)),AIMAG(VX(1,2))), CR_HYPOT(REAL(VX(2,2)),AIMAG(VX(2,2)))))
     VX(1,1) = V(1,1)
     VX(2,1) = V(2,1)
     VX(1,2) = V(1,2)
     VX(2,2) = V(2,2)
     GX(1,1) = CONJG(VX(1,1))
     GX(2,1) = CONJG(VX(1,2))
     GX(1,2) = CONJG(VX(2,1))
     GX(2,2) = CONJG(VX(2,2))
     GX = MATMUL(GX, VX) - SX
     WRITE (*,1) '||V^H V - I||_F=',&
          CR_HYPOT(CR_HYPOT(CR_HYPOT(REAL(GX(1,1)),AIMAG(GX(1,1))), CR_HYPOT(REAL(GX(2,1)),AIMAG(GX(2,1)))),&
          CR_HYPOT(CR_HYPOT(REAL(GX(1,2)),AIMAG(GX(1,2))), CR_HYPOT(REAL(GX(2,2)),AIMAG(GX(2,2)))))
     GX(1,1) = G(1,1)
     GX(2,1) = G(2,1)
     GX(1,2) = G(1,2)
     GX(2,2) = G(2,2)
     VX(1,1) = CONJG(VX(1,1))
     SX(2,1) = REAL(VX(2,1))
     SX(1,2) = -AIMAG(VX(2,1))
     VX(2,1) = CONJG(VX(1,2))
     VX(1,2) = CMPLX(SX(2,1), SX(1,2), KX)
     VX(2,2) = CONJG(VX(2,2))
     ! avoid a possible overflow due to the backscaling
     SX(1,1) = S(1)
     SX(1,1) = SCALE(SX(1,1), INFO(2) - INFO(1))
     WRITE (*,1,ADVANCE='NO') 'SIGMA(1)=', SX(1,1)
     IF (.NOT. (SX(1,1) .LE. HUGE(S(1)))) WRITE(*,'(A)',ADVANCE='NO') ' !'
     WRITE (*,*)
     SX(2,1) = 0.0_KX
     SX(1,2) = 0.0_KX
     SX(2,2) = S(2)
     SX(2,2) = SCALE(SX(2,2), INFO(3) - INFO(1))
     WRITE (*,1,ADVANCE='NO') 'SIGMA(2)=', SX(2,2)
     IF (.NOT. (SX(2,2) .LE. HUGE(S(2)))) WRITE(*,'(A)',ADVANCE='NO') ' !'
     WRITE (*,*)
     UX = MATMUL(MATMUL(UX, SX), VX) - GX
     SX(2,1) = CR_HYPOT(CR_HYPOT(CR_HYPOT(REAL(UX(1,1)),AIMAG(UX(1,1))), CR_HYPOT(REAL(UX(2,1)),AIMAG(UX(2,1)))),&
          CR_HYPOT(CR_HYPOT(REAL(UX(1,2)),AIMAG(UX(1,2))), CR_HYPOT(REAL(UX(2,2)),AIMAG(UX(2,2)))))
     SX(1,2) = CR_HYPOT(CR_HYPOT(CR_HYPOT(REAL(GX(1,1)),AIMAG(GX(1,1))), CR_HYPOT(REAL(GX(2,1)),AIMAG(GX(2,1)))),&
          CR_HYPOT(CR_HYPOT(REAL(GX(1,2)),AIMAG(GX(1,2))), CR_HYPOT(REAL(GX(2,2)),AIMAG(GX(2,2)))))
     IF ((SX(1,2) .EQ. 0.0_KX) .AND. (SX(2,1) .EQ. 0.0_KX)) THEN
        SX(2,1) = 0.0_KX
     ELSE ! the general case
        SX(2,1) = SX(2,1) / SX(1,2)
     END IF
     WRITE (*,1) '||U SIGMA V^H - G||_F / ||G||_F=', SX(2,1)
     IF (ONCE) EXIT
  END DO
  IF (.NOT. ONCE) THEN
     IF (I .NE. INPUT_UNIT) THEN
        CLOSE(UNIT=I, IOSTAT=J)
     ELSE
        J = 0
     END IF
     IF (J .NE. 0) ERROR STOP 'cannot close the input file'
  END IF
