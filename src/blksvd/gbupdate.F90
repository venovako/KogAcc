#ifndef NDEBUG
  I = 0
  L = 2 * B
  IF (NB .LT. 0) I = -13
  IF (LDB .LT. L) I = -12
  IF (LDG .LT. M) I = -4
  IF (B .LT. 1) I = -2
  IF (M .LT. 2) I = -1
  IF (I .NE. 0) THEN
     INFO = I
     RETURN
  END IF
  IF (MOD(M, B) .NE. 0) THEN
     INFO = -2
     RETURN
  END IF
#endif
  ! GB is assumed to be unpacked back to G prior to invoking this routine.
  L = INFO
  INFO = 0
  IF (NB .EQ. 0) RETURN
  IF (L .GE. 0) THEN
     ! Reuse the block transformations already (potentially) in the cache.
     IF (LDU .GT. 0) THEN
#include "gbupU.F90"
     END IF
#include "gbupGr.F90"
#include "gbupGc.F90"
     IF (LDV .GT. 0) THEN
#include "gbupV.F90"
     END IF
  ELSE ! L .LT. 0
     ! TODO: consider updating U and V from the right concurrently.
     IF (LDU .GT. 0) THEN
#include "gbupU.F90"
     END IF
     IF (LDV .GT. 0) THEN
#include "gbupV.F90"
     END IF
#include "gbupGc.F90"
#include "gbupGr.F90"
  END IF
