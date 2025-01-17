!>@brief \b IBDIMSX tests the IBDIMS routine.
PROGRAM IBDIMSX
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128, ERROR_UNIT, OUTPUT_UNIT
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE IBDIMS(N, B, M, M_B, NW, ND, NO, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(INOUT) :: N, B, M, INFO
       INTEGER, INTENT(OUT) :: M_B, NW, ND, NO
     END SUBROUTINE IBDIMS
  END INTERFACE

  INTEGER, PARAMETER :: CLAL = 256
  CHARACTER(LEN=CLAL) :: CLA
  INTEGER :: N, LDG, B, LDB, M, M_B, NW, ND, NO, INFO

  INFO = COMMAND_ARGUMENT_COUNT()
  IF (INFO .NE. 4) THEN
     CALL GET_COMMAND_ARGUMENT(0, CLA)
     WRITE (ERROR_UNIT,*) TRIM(CLA), ' N B J S'
     STOP 'INVALID COMMAND LINE'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, CLA, M_B, INFO)
  IF (INFO .NE. 0) STOP 'N'
  READ (CLA,*) N
  CALL GET_COMMAND_ARGUMENT(2, CLA, M_B, INFO)
  IF (INFO .NE. 0) STOP 'B'
  READ (CLA,*) B
  CALL GET_COMMAND_ARGUMENT(3, CLA, M_B, INFO)
  IF (INFO .NE. 0) STOP 'J'
  READ (CLA,*) M
  CALL GET_COMMAND_ARGUMENT(4, CLA, M_B, INFO)
  IF (INFO .NE. 0) STOP 'S'
  READ (CLA,*) INFO

  SELECT CASE (INFO)
  CASE (-REAL128)
     WRITE (OUTPUT_UNIT,*) 'Assuming COMPLEX(REAL128).'
  CASE (-REAL64)
     WRITE (OUTPUT_UNIT,*) 'Assuming COMPLEX(REAL64).'
  CASE (-REAL32)
     WRITE (OUTPUT_UNIT,*) 'Assuming COMPLEX(REAL32).'
  CASE (REAL32)
     WRITE (OUTPUT_UNIT,*) 'Assuming REAL32.'
  CASE (REAL64)
     WRITE (OUTPUT_UNIT,*) 'Assuming REAL64.'
  CASE (REAL128)
     WRITE (OUTPUT_UNIT,*) 'Assuming REAL128.'
  CASE DEFAULT
     STOP 'S must be one of +/-REAL32, +/-REAL64, +/-REAL128'
  END SELECT

  SELECT CASE (M)
  CASE (0)
     WRITE (OUTPUT_UNIT,*) 'Assuming the row-cyclic block-ordering.'
  CASE (1)
     WRITE (OUTPUT_UNIT,*) 'Assuming the column-cyclic block-ordering.'
  CASE (2,5)
     WRITE (OUTPUT_UNIT,*) 'Assuming the generalized Mantharam-Eberlein block-ordering.'
  CASE (3,6)
     WRITE (OUTPUT_UNIT,*) 'Assuming the dynamic block-ordering.'
  CASE (4,7)
     WRITE (OUTPUT_UNIT,*) 'Assuming the modified modulus block-ordering.'
  CASE DEFAULT
     STOP 'J must be between 0 and 7'
  END SELECT

  WRITE (OUTPUT_UNIT,*) 'N [    initial order of G] =', N
  WRITE (OUTPUT_UNIT,*) 'B [ block row/column size] =', B
  LDG = N
  LDB = B
  CALL IBDIMS(LDG, LDB, M, M_B, NW, ND, NO, INFO)
  WRITE (OUTPUT_UNIT,*) 'M [  order of G, bordered] =', M
  WRITE (OUTPUT_UNIT,*) 'LDG [   leading dim for M] =', LDG
  WRITE (OUTPUT_UNIT,*) 'LDB [ leading dim for 2*B] =', LDB
  WRITE (OUTPUT_UNIT,*) 'M / B [    exact division] =', M_B
  WRITE (OUTPUT_UNIT,*) 'len(W) [element type of G] =', NW
  WRITE (OUTPUT_UNIT,*) 'len(D) [ higher precision] =', ND
  WRITE (OUTPUT_UNIT,*) 'ncols(O) [ integer O(2,:)] =', NO
  WRITE (OUTPUT_UNIT,*) 'values in an L1 cache line =', INFO
END PROGRAM IBDIMSX
