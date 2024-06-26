  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 3) THEN
     CALL GET_COMMAND_ARGUMENT(0, BN)
     WRITE (ERROR_UNIT,*) TRIM(BN), ' J N BN'
     ERROR STOP 'INVALID COMMAND LINE'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, BN, I, INFO)
  IF (INFO .NE. 0) ERROR STOP 'J'
  READ (BN,*) J
  CALL GET_COMMAND_ARGUMENT(2, BN, I, INFO)
  IF (INFO .NE. 0) ERROR STOP 'N'
  READ (BN,*) N
  CALL GET_COMMAND_ARGUMENT(3, BN, I, INFO)
  IF (INFO .NE. 0) ERROR STOP 'BN'

  L = 0
  IF ((J .GE. 2) .AND. (J .LE. 4)) THEN
     !$ L = 1
     CONTINUE
  ELSE IF ((J .GE. 5) .AND. (J .LE. 7)) THEN
     J = J - 3
  ELSE IF (J .GE. 8) THEN
     ERROR STOP 'J'
  END IF

  IF (N .EQ. 0) ERROR STOP 'N'
  IF (N .LT. 0) THEN
     N = -N
     I = -1
  ELSE ! the default mode is "slow"
     I = 0
  END IF

  IF ((J .EQ. 2) .OR. (J .EQ. 4)) THEN
     INFO = L
     CALL NB2M(N, 2, M, INFO)
     IF (INFO .NE. 0) ERROR STOP 'NB2M'
  ELSE ! all other strategies
     M = N
  END IF
  LDV = 64
  LDU = LDV / INT(SIZEOF(0.0_K))
  LDG = MOD(M, LDU)
  IF (LDG .NE. 0) M = M + (LDU - LDG)
  LDG = M
  LDU = M
  LDV = M

  ALLOCATE(U(LDU,M))
  IF (M .GT. N) THEN
     INFO = L
     CALL BRDG(M, N, U, LDU, INFO)
     IF (INFO .NE. 0) ERROR STOP 'BRDG(U)'
  END IF

  ALLOCATE(V(LDV,M))
  IF (M .GT. N) THEN
     INFO = L
     CALL BRDG(M, N, V, LDV, INFO)
     IF (INFO .NE. 0) ERROR STOP 'BRDG(V)'
  END IF

  ALLOCATE(G(LDG,M))
  IF (M .GT. N) THEN
     INFO = L
     CALL BRDG(M, N, G, LDG, INFO)
     IF (INFO .NE. 0) ERROR STOP 'BRDG(G)'
  END IF

  INFO = L
  CALL RDINP(BN, M, N, G, LDG, INFO)
  IF (INFO .NE. 0) ERROR STOP 'RDINP'

  ALLOCATE(SV(M))
  ALLOCATE(W(MAX(M,3)*M))
  ! if, e.g., ||G||_F is known to be numerically finite and reasonably below HUGE,
  ! the dynamic scaling can be turned off for speed
  W(1) = REAL(I, K)
  W(2) = 0.0_K
  W(3) = 0.0_K

  ALLOCATE(O(2*M*(M-1)))
  IF ((J .GE. 0) .AND. (J .NE. 3)) THEN
     INFO = L
     CALL JSWEEP(J, M, S, P, O, INFO)
     IF (INFO .NE. 0) ERROR STOP 'JSWEEP'
  ELSE IF (J .LT. 0) THEN
     S = 1
     P = MIN(-J, M / 2)
     IF (M .GT. 1) O(1) = P
     J = 3
  ELSE ! J = 3
     S = 1
     P = M / 2
     IF (M .GT. 1) O(1) = 0
  END IF

  ALLOCATE(R(2,M))

#ifdef ANIMATE
  JOB = 3
  I = LEN_TRIM(BN) + 1
  BN(I:I) = C_NULL_CHAR
  CTX = PVN_RVIS_START(M, M, JOB, BN)
  BN(I:I) = ' '
  BN = TRIM(BN)
  CALL C_F_POINTER(C_LOC(SV), CP)
  CP = CTX
  CP => NULL()
#endif

  JOB = J + 120
  INFO = -HUGE(INFO)
  INFO = INFO - 1
  !$ IF (L .NE. 0) INFO = -(INFO + 1)
  CALL SYSTEM_CLOCK(C0)
  CALL KSVD0(JOB, M, G, LDG, U, LDU, V, LDV, SV, W, O, R, INFO)
  CALL SYSTEM_CLOCK(C1, CR)
  T = REAL(CR, REAL128)
  T = REAL(C1 - C0, REAL128) / T
  WRITE (OUTPUT_UNIT,'(A,F15.6,A,I11,A)',ADVANCE='NO') 'KSVD0 took ', T, ' s with ', INFO, ' steps and W=('
  WRITE (OUTPUT_UNIT,9) W(1), ',', W(2), ',', W(3), ')'
  FLUSH(OUTPUT_UNIT)
  IF (INFO .LT. 0) THEN
     WRITE (ERROR_UNIT,*) 'ERROR in KSVD0'
  ELSE IF (INFO .EQ. HUGE(INFO)) THEN
     WRITE (ERROR_UNIT,*) 'NO CONVERGENCE'
  ELSE ! all OK
     WRITE (ERROR_UNIT,*) 'SUCCESS'
  END IF

  DEALLOCATE(R)
  DEALLOCATE(O)
  DEALLOCATE(W)
  DEALLOCATE(G)

  INFO = L
  CALL WROUT(BN, J, N, U, LDU, V, LDV, SV, INFO)
  IF (INFO .NE. 0) ERROR STOP 'WROUT'

  DEALLOCATE(SV)
  DEALLOCATE(V)
  DEALLOCATE(U)

#ifdef ANIMATE
  IF (C_ASSOCIATED(CTX)) THEN
     M = ANIMATE
     JOB = 8
     BN(I:I) = C_NULL_CHAR
     I = INT(PVN_RVIS_STOP(CTX, M, M, JOB, BN))
     CTX = C_NULL_PTR
  END IF
#endif
