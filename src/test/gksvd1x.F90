  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 4) THEN
     CALL GET_COMMAND_ARGUMENT(0, BN)
     WRITE (ERROR_UNIT,*) TRIM(BN), ' J N B BN'
     STOP 'INVALID COMMAND LINE'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, BN, I, INFO)
  IF (INFO .NE. 0) STOP 'J'
  READ (BN,*) J
  CALL GET_COMMAND_ARGUMENT(2, BN, I, INFO)
  IF (INFO .NE. 0) STOP 'N'
  READ (BN,*) N
  CALL GET_COMMAND_ARGUMENT(3, BN, I, INFO)
  IF (INFO .NE. 0) STOP 'B'
  READ (BN,*) B
  CALL GET_COMMAND_ARGUMENT(4, BN, I, INFO)
  IF (INFO .NE. 0) STOP 'BN'

  L = 0
  IF (N .GT. 0) THEN
     !$ L = 1
     CONTINUE
  ELSE IF (N .LT. 0) THEN
     N = -N
  ELSE ! N .EQ. 0
     STOP 'N'
  END IF
  IF ((J .LT. 0) .OR. (J .GT. 4)) STOP 'J'
  IF (B .EQ. 0) THEN
     B = 16
  ELSE IF ((B .LT. 0) .OR. (B .GT. 16)) THEN
     STOP 'B'
  END IF

  LDG = N
  LDB = B
  M = J
  INFO = K
  CALL IBDIMS(LDG, LDB, M, M_B, NW, ND, NO, INFO)
  IF (INFO .LT. 0) STOP 'IBDIMS'
  LDU = LDG
  LDV = LDG

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

  ALLOCATE(SV(M))
  ALLOCATE(W(NW))
  ALLOCATE(D(ND))
  ALLOCATE(O(2,NO))

  SELECT CASE (J)
  CASE (0,3)
     CALL INITRC(M_B, O, INFO)
     IF (MOD(M_B, 2) .EQ. 0) THEN
        I = (M_B / 2) * (M_B - 1)
     ELSE ! M_B odd
        I = M_B * ((M_B - 1) / 2)
     END IF
  CASE (1)
     CALL INITCC(M_B, O, INFO)
     IF (MOD(M_B, 2) .EQ. 0) THEN
        I = (M_B / 2) * (M_B - 1)
     ELSE ! M_B odd
        I = M_B * ((M_B - 1) / 2)
     END IF
  CASE (2)
     CALL INITME(M_B, O, INFO)
     I = (M_B / 2) * (M_B - 1)
  CASE (4)
     CALL INITMM(M_B, O, INFO)
     I = (M_B / 2) * M_B
  CASE DEFAULT
     STOP 'J'
  END SELECT
  I = I + 1
  IF (INFO .EQ. 0) CALL INITRC(2*B, O(1,I), INFO)
  IF (INFO .NE. 0) STOP 'INIT(O)'

#ifdef ANIMATE
  JOB = 3
  I = LEN_TRIM(BN) + 1
  BN(I:I) = C_NULL_CHAR
  CTX = VIS_START(M, M, JOB, BN)
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
  CALL KSVD1(JOB, M, B, G, LDG, U, LDU, V, LDV, SV, LDB, W, NW, D, ND, O, NO, INFO)
  CALL SYSTEM_CLOCK(C1, CR)
  T = REAL(CR, REAL128)
  T = REAL(C1 - C0, REAL128) / T
  WRITE (OUTPUT_UNIT,'(A,F15.6,A,I11,A)') 'KSVD1 took ', T, ' s with ', INFO, ' block steps'
  FLUSH(OUTPUT_UNIT)

  INFO = 0
  CALL WROUT(BN, J, N, U, LDU, V, LDV, SV, INFO)
  IF (INFO .NE. 0) STOP 'WROUT'

  DEALLOCATE(O)
  DEALLOCATE(D)
  DEALLOCATE(W)
  DEALLOCATE(SV)
  DEALLOCATE(G)
  DEALLOCATE(V)
  DEALLOCATE(U)

#ifdef ANIMATE
  IF (C_ASSOCIATED(CTX)) THEN
     M = ANIMATE
     JOB = 8
     BN(I:I) = C_NULL_CHAR
     I = INT(VIS_STOP(CTX, M, M, JOB, BN))
     CTX = C_NULL_PTR
  END IF
#endif
