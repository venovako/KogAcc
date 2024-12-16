  IF (INFO(1) .LT. -HUGE(0)) RETURN
  CALL YLJR2(REAL(G(1,1)),AIMAG(G(1,1)), REAL(G(2,1)),AIMAG(G(2,1)), REAL(G(1,2)),AIMAG(G(1,2)), REAL(G(2,2)),AIMAG(G(2,2)), &
       REAL(U(1,1)),AIMAG(U(1,1)), REAL(U(2,1)),AIMAG(U(2,1)), REAL(U(1,2)),AIMAG(U(1,2)), REAL(U(2,2)),AIMAG(U(2,2)), &
       REAL(V(1,1)),AIMAG(V(1,1)), REAL(V(2,1)),AIMAG(V(2,1)), REAL(V(1,2)),AIMAG(V(1,2)), REAL(V(2,2)),AIMAG(V(2,2)), &
       S(1), S(2), INFO, EX)
  E(1) = EX(2)
  E(2) = EX(3)
  E(3) = EX(4)
