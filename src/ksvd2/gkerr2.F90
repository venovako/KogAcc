#ifndef NDEBUG
  E = ZERO
  EX = ZERO
#endif
  IF (INFO(1) .LT. -HUGE(0)) RETURN
  CALL QLJR2(G(1,1), G(2,1), G(1,2), G(2,2), &
       U(1,1), U(2,1), U(1,2), U(2,2), &
       V(1,1), V(2,1), V(1,2), V(2,2), &
       S(1), S(2), INFO, EX)
  E(1) = EX(2)
  E(2) = EX(3)
  E(3) = EX(4)
