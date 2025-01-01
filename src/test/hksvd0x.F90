  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 3) THEN
     CALL GET_COMMAND_ARGUMENT(0, BN)
     WRITE (ERROR_UNIT,*) TRIM(BN), ' J N BN'
     STOP 'INVALID COMMAND LINE'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, BN, I, INFO)
  IF (INFO .NE. 0) STOP 'J'
  READ (BN,*) J
  CALL GET_COMMAND_ARGUMENT(2, BN, I, INFO)
  IF (INFO .NE. 0) STOP 'N'
  READ (BN,*) N
  CALL GET_COMMAND_ARGUMENT(3, BN, I, INFO)
  IF (INFO .NE. 0) STOP 'BN'

  L = 0
  !$ L = 1
  IF ((J .LT. 0) .OR. (J .GT. 7)) STOP 'J'
  IF (N .LE. 0) STOP 'N'
  IF ((J .EQ. 2) .OR. (J .EQ. 4) .OR. (J .EQ. 5) .OR. (J .EQ. 7)) THEN
     INFO = L
     CALL NB2M(N, 2, M, INFO)
     IF (INFO .NE. 0) STOP 'NB2M'
  ELSE ! all other strategies
     M = N
  END IF
  LDV = 32
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
     IF (INFO .NE. 0) STOP 'BRDG(U)'
  END IF

  ALLOCATE(V(LDV,M))
  IF (M .GT. N) THEN
     INFO = L
     CALL BRDG(M, N, V, LDV, INFO)
     IF (INFO .NE. 0) STOP 'BRDG(V)'
  END IF

  ALLOCATE(G(LDG,M))
  IF (M .GT. N) THEN
     INFO = L
     CALL BRDG(M, N, G, LDG, INFO)
     IF (INFO .NE. 0) STOP 'BRDG(G)'
  END IF

  INFO = L
  CALL RDINP(BN, M, N, G, LDG, INFO)
  IF (INFO .NE. 0) STOP 'RDINP'

#ifdef ANIMATE
  ALLOCATE(SV(MAX(2,M)))
#else
  ALLOCATE(SV(M))
#endif
  ALLOCATE(W(MAX(6,MAX(M,5)*M)))
  ALLOCATE(O(2*M*(M-1)))
  IF ((J .NE. 3) .AND. (J .NE. 6)) THEN
     INFO = L
     CALL JSWEEP(J, M, S, P, O, INFO)
     IF (INFO .NE. 0) STOP 'JSWEEP'
  ELSE ! dynamic ordering
     S = 1
     P = M / 2
     IF (M .GT. 1) O(1) = 0
  END IF
  ALLOCATE(R(2,M))

#ifdef ANIMATE
  JOB = 3
  I = LEN_TRIM(BN) + 1
  BN(I:I) = 'R'
  CN = BN
  CN(I:I) = 'I'
  I = I + 1
  BN(I:I) = C_NULL_CHAR
  CN(I:I) = C_NULL_CHAR
  CTX = PVN_CVIS_START(M, M, JOB, BN, CN)
  BN(I:I) = ' '
  I = I - 1
  BN(I:I) = ' '
  BN = TRIM(BN)
  CALL C_F_POINTER(C_LOC(SV), CP)
  CP = CTX
  CP => NULL()
#endif

  JOB = J + 960
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

  INFO = -INT(W(4))
  CALL WROUT(BN, J, N, U, LDU, V, LDV, SV, INFO)
  IF (INFO .NE. 0) STOP 'WROUT'

  DEALLOCATE(R)
  DEALLOCATE(O)
  DEALLOCATE(W)
  DEALLOCATE(SV)
  DEALLOCATE(G)
  DEALLOCATE(V)
  DEALLOCATE(U)

#ifdef ANIMATE
  IF (C_ASSOCIATED(CTX)) THEN
     M = ANIMATE
     JOB = 8
     BN(I:I) = 'R'
     I = I + 1
     BN(I:I) = C_NULL_CHAR
     I = INT(PVN_CVIS_STOP(CTX, M, M, JOB, BN, JOB, CN))
     CTX = C_NULL_PTR
  END IF
#endif
