FUNCTION ORFILE()
  IMPLICIT NONE
  INTEGER :: ORFILE
  OPEN(NEWUNIT=ORFILE,FILE='/dev/random',ACCESS='STREAM',ACTION='READ',FORM='UNFORMATTED',STATUS='OLD')
END FUNCTION ORFILE
