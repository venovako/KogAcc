  ES(1) = INT(INFO(1), c_int)
  KND = LJSV2(G(1,1), G(2,1), G(1,2), G(2,2), &
       U(1,1), U(2,1), U(1,2), U(2,2), V(1,1), V(2,1), V(1,2), V(2,2), S(1), S(2), ES)
  IF (KND .LT. 0_c_int) THEN
     INFO(1) = -HUGE(0)-1
  ELSE ! all OK
     INFO(1) = INT(ES(1))
  END IF
  INFO(2) = INT(ES(2))
  INFO(3) = INT(ES(3))
