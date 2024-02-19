     ! use G(2,1) to determine the signs of the other three elements of G
     M = EXPONENT(G(2,1))
     IF (IAND(M, 1) .NE. 0) G(1,1) = -G(1,1)
     G(2,1) = ZERO
     IF (IAND(M, 4) .NE. 0) G(1,2) = -G(1,2)
     IF (IAND(M, 8) .NE. 0) G(2,2) = -G(2,2)
     INFO = 0
     QG(1,1) = G(1,1)
     QG(2,1) = G(2,1)
     QG(1,2) = G(1,2)
     QG(2,2) = G(2,2)
     CALL QKSVD2(QG, QU, QV, QS, INFO)
     IF (INFO .LE. -HUGE(INFO)) CALL STHALT('QKSVD2')
     IF (INFO .NE. 0) THEN
        L = -INFO
        QS(1) = SCALE(QS(1), L)
        QS(2) = SCALE(QS(2), L)
        INFO = 0
     END IF
     Q = QS(1) / QS(2)
     IF (Q .NE. Q) THEN
        Q = QZERO
        Q = QONE / Q
     END IF
     ! \kappa_2(G)
     F(1,1) = MAX(Q, F(1,1))
     F(2,1) = MAX(-Q, F(2,1))
     CALL KSVD2(G, U, V, S, INFO)
     IF (INFO .LE. -HUGE(INFO)) CALL STHALT('KSVD2')
     IF (INFO .NE. 0) THEN
        L = -INFO
        S(1) = SCALE(S(1), L)
        S(2) = SCALE(S(2), L)
        INFO = 0
     END IF
     CALL KERR2(G, U, V, S, E(1,1), INFO)
     E(4,1) = MAX(ABS(QS(1) - S(1)) / QS(1), QZERO)
     E(5,1) = MAX(ABS(QS(2) - S(2)) / QS(2), QZERO)
     F(1,2) = MAX(E(1,1), F(1,2))
     F(2,2) = MAX(-E(1,1), F(2,2))
     F(1,3) = MAX(E(2,1), F(1,3))
     F(2,3) = MAX(-E(2,1), F(2,3))
     F(1,4) = MAX(E(3,1), F(1,4))
     F(2,4) = MAX(-E(3,1), F(2,4))
     F(1,5) = MAX(E(4,1), F(1,5))
     F(2,5) = MAX(-E(4,1), F(2,5))
     F(1,6) = MAX(E(5,1), F(1,6))
     F(2,6) = MAX(-E(5,1), F(2,6))
     CALL KALT2(G, U, V, S, INFO)
     IF (INFO .LE. -HUGE(INFO)) CALL STHALT('KALT2')
     IF (INFO .NE. 0) THEN
        L = -INFO
        S(1) = SCALE(S(1), L)
        S(2) = SCALE(S(2), L)
        INFO = 0
     END IF
     CALL KERR2(G, U, V, S, E(1,2), INFO)
     E(4,2) = MAX(ABS(QS(1) - S(1)) / QS(1), QZERO)
     E(5,2) = MAX(ABS(QS(2) - S(2)) / QS(2), QZERO)
     F(1,7) = MAX(E(1,2), F(1,7))
     F(2,7) = MAX(-E(1,2), F(2,7))
     F(1,8) = MAX(E(2,2), F(1,8))
     F(2,8) = MAX(-E(2,2), F(2,8))
     F(1,9) = MAX(E(3,2), F(1,9))
     F(2,9) = MAX(-E(3,2), F(2,9))
     F(1,10) = MAX(E(4,2), F(1,10))
     F(2,10) = MAX(-E(4,2), F(2,10))
     F(1,11) = MAX(E(5,2), F(1,11))
     F(2,11) = MAX(-E(5,2), F(2,11))
     CALL LWSV2(G, U, V, S, INFO)
     IF (INFO .LE. -HUGE(INFO)) CALL STHALT('LWSV2')
     IF (INFO .NE. 0) THEN
        L = -INFO
        S(1) = SCALE(S(1), L)
        S(2) = SCALE(S(2), L)
        INFO = 0
     END IF
     CALL KERR2(G, U, V, S, E(1,3), INFO)
     ! be extremely cautious
     S(1) = ABS(S(1))
     S(2) = ABS(S(2))
     IF (S(1) .LT. S(2)) THEN
        T = S(1)
        S(1) = S(2)
        S(2) = T
     END IF
     E(4,3) = MAX(ABS(QS(1) - S(1)) / QS(1), QZERO)
     E(5,3) = MAX(ABS(QS(2) - S(2)) / QS(2), QZERO)
     F(1,12) = MAX(E(1,3), F(1,12))
     F(2,12) = MAX(-E(1,3), F(2,12))
     F(1,13) = MAX(E(2,3), F(1,13))
     F(2,13) = MAX(-E(2,3), F(2,13))
     F(1,14) = MAX(E(3,3), F(1,14))
     F(2,14) = MAX(-E(3,3), F(2,14))
     F(1,15) = MAX(E(4,3), F(1,15))
     F(2,15) = MAX(-E(4,3), F(2,15))
     F(1,16) = MAX(E(5,3), F(1,16))
     F(2,16) = MAX(-E(5,3), F(2,16))
     DO L = 1, 5
        Q = E(L,3) / E(L,1)
        INFO = 16 + L
        F(1,INFO) = MAX(Q, F(1,INFO))
        F(2,INFO) = MAX(-Q, F(2,INFO))
     END DO
     DO L = 1, 5
        Q = E(L,3) / E(L,2)
        INFO = 21 + L
        F(1,INFO) = MAX(Q, F(1,INFO))
        F(2,INFO) = MAX(-Q, F(2,INFO))
     END DO
