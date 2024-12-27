  I = 0
  L = 2 * B
  IF (NB .LT. 0) I = -13
  IF (LDB .LT. L) I = -12
  IF (LDV .LT. M) I = -8
  IF (LDU .LT. M) I = -6
  IF (LDG .LT. M) I = -4
  IF (B .LE. 0) I = -2
  IF (M .LT. 2) I = -1
  IF (I .NE. 0) THEN
     INFO = I
     RETURN
  END IF
  IF (MOD(M, B) .NE. 0) THEN
     INFO = -2
     RETURN
  END IF
  IF (NB .EQ. 0) RETURN
  ! GB is assumed to be unpacked back to G prior to invoking this routine.
  L = INFO
  INFO = 0
  IF (L .LT. 0) THEN
     ! TODO: consider updating U and V from the right concurrently.
#include "gbupU.F90"
#include "gbupV.F90"
#include "gbupGc.F90"
#include "gbupGr.F90"
  ELSE ! L .GE. 0
     ! Reuse the block transformations already (potentially) in the cache.
#include "gbupU.F90"
#include "gbupGr.F90"
#include "gbupGc.F90"
#include "gbupV.F90"
  END IF
